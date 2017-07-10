/**
 * Copyright 2011-2017 Green Energy Corp.
 *
 * Licensed to Green Energy Corp (www.greenenergycorp.com) under one or more
 * contributor license agreements. See the NOTICE file distributed with this
 * work for additional information regarding copyright ownership. Green Energy
 * Corp licenses this file to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package io.greenbus.edge.configure.dyn

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api.{ ActiveSetHandle, ProducerHandle }
import io.greenbus.edge.configure.endpoint.{ FepConfigurerMgr, ModuleConfiguration }
import io.greenbus.edge.configure.sql.server.{ ModuleDb, ModuleDbEntry }
import io.greenbus.edge.data.{ IndexableValue, Value, ValueMap, ValueString }
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

case class ModuleValueUpdate(component: String, nodeOpt: Option[String], data: Array[Byte])
case class ModuleValueRemove(component: String, nodeOpt: Option[String])

object SummarySubject {

  sealed trait Updates
  case class RemoveModule(module: String) extends Updates
  case class UpdateModule(module: String, removed: Set[String], updated: Map[String, Option[String]]) extends Updates

  sealed trait State
  case class Pending(buffer: mutable.ArrayBuffer[Updates]) extends State
  case class Resolved(current: Map[String, Map[String, Option[String]]]) extends State

  def toUpdate(current: Map[String, Map[String, Option[String]]]): Map[IndexableValue, Value] = {
    current.map {
      case (component, moduleMap) =>
        val value: Map[Value, Value] = moduleMap.map {
          case (module, nodeOpt) =>
            (ValueString(module), nodeOpt.map(ValueString).getOrElse(ValueString("")))
        }
        (ValueString(component), ValueMap(value))
    }
  }
}
class SummarySubject(handle: ActiveSetHandle) {
  import SummarySubject._

  private var state: State = Pending(mutable.ArrayBuffer.empty[Updates])

  def original(current: Map[String, Map[String, Option[String]]]): Unit = {
    state match {
      case Pending(buffer) => {

        buffer.foldLeft(current) {
          case (curr, RemoveModule(module)) => curr - module
          case (curr, UpdateModule(module, removed, updated)) =>
            curr.get(module) match {
              case None => curr + (module -> updated)
              case Some(prev) => curr + (module -> ((prev -- removed) ++ updated))
            }

        }

        val map = toUpdate(current)
        handle.update(map)
        state = Resolved(current)
      }
      case Resolved(_) =>
    }
  }

  def moduleRemoved(module: String): Unit = {
    state match {
      case Pending(buffer) => {
        buffer += RemoveModule(module)
      }
      case Resolved(current) =>
        val updated = current - module
        val map = toUpdate(updated)
        handle.update(map)
        state = Resolved(updated)
    }
  }

  def moduleUpdated(module: String, removed: Set[String], updates: Map[String, Option[String]]): Unit = {
    state match {
      case Pending(buffer) => {
        buffer += UpdateModule(module, removed, updates)
      }
      case Resolved(current) =>
        val updated = current.get(module) match {
          case None => current + (module -> updates)
          case Some(prev) => current + (module -> ((prev -- removed) ++ updates))
        }

        val map = toUpdate(updated)
        handle.update(map)
        state = Resolved(updated)
    }
  }
}

object ModuleIndex {

  def toActiveSetMap(values: Seq[ModuleDbEntry]): Map[IndexableValue, Value] = {
    val activeSet: Map[IndexableValue, Value] = values.flatMap { mv =>
      FepConfigurerMgr.fromDbBytes(s"${mv.module}/${mv.component}", mv.data)
        .map(v => (ValueString(mv.module), v))
    }.toMap

    activeSet
  }

  def toActiveSetMap(simple: Map[String, Array[Byte]]): Map[IndexableValue, Value] = {
    simple.flatMap {
      case (module, data) =>
        FepConfigurerMgr.fromDbBytes(s"$module", data)
          .map(v => (ValueString(module), v))
    }
  }

}
class ModuleIndex(eventThread: CallMarshaller, db: ModuleDb) extends LazyLogging {
  private var sequence: Long = 0
  private val nodeComponentMap = mutable.Map.empty[String, mutable.Map[String, NodeComponentSubject]]
  private var moduleSummarySubject = Option.empty[SummarySubject]
  private var producerHandleOpt = Option.empty[ProducerHandle]

  def setProducerHandle(handle: ProducerHandle): Unit = {
    producerHandleOpt = Some(handle)
  }

  def setSummary(summaryHandle: ActiveSetHandle): Unit = {
    moduleSummarySubject = Some(new SummarySubject(summaryHandle))

    db.componentSummary().foreach { summaries =>
      eventThread.marshal {
        val current = summaries.groupBy(_.module).mapValues(values => values.map(v => (v.component, v.nodeOpt)).toMap)
        moduleSummarySubject.foreach(_.original(current))
      }
    }
  }

  def register(node: String, component: String, handle: ActiveSetHandle): Unit = {
    logger.info(s"Registered: $node - $component")
    val seq = sequence
    sequence += 1

    val componentMap = nodeComponentMap.getOrElseUpdate(node, mutable.Map.empty[String, NodeComponentSubject])
    componentMap.update(component, new NodeComponentSubject(seq, handle))

    db.valuesForNodeComponent(node, component).foreach { componentValues =>
      eventThread.marshal {
        logger.debug(s"valuesForNodeComponent: $componentValues")
        nodeComponentMap.get(node).foreach { componentMap =>
          componentMap.get(component).foreach { subj =>
            if (subj.sequence == seq) {
              val simpleMap = componentValues.map(cv => (cv.module, cv.data)).toMap
              subj.original(simpleMap)
              producerHandleOpt.foreach(_.flush())
            }
          }
        }
      }
    }
  }
  def unregister(node: String, component: String): Unit = {
    logger.info(s"Unregistered: $node - $component")

    nodeComponentMap.get(node).foreach { componentMap =>
      componentMap -= component
    }
    if (!nodeComponentMap.get(node).exists(_.nonEmpty)) {
      nodeComponentMap -= node
    }
  }

  def onModuleRemoved(module: String, removes: Seq[ModuleValueRemove]): Unit = {

    moduleSummarySubject.foreach(_.moduleRemoved(module))

    removes.foreach { mvr =>

      for {
        node <- mvr.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvr.component)
      } {
        subj.updates(Set(module), Seq())
      }
    }

    producerHandleOpt.foreach(_.flush())
  }

  def onModuleUpdates(module: String, removes: Seq[ModuleValueRemove], updates: Seq[ModuleValueUpdate]): Unit = {

    moduleSummarySubject.foreach(_.moduleUpdated(module, removes.map(_.component).toSet, updates.map(mvu => (mvu.component, mvu.nodeOpt)).toMap))

    removes.foreach { mvr =>
      for {
        node <- mvr.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvr.component)
      } {
        subj.updates(Set(module), Seq())
      }
    }

    updates.foreach { mvu =>
      for {
        node <- mvu.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvu.component)
      } {
        subj.updates(Set(), Seq((module, mvu.data)))
      }
    }
    producerHandleOpt.foreach(_.flush())
  }
}
