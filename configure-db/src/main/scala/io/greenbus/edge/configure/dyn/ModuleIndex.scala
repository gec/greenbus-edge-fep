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
import io.greenbus.edge.configure.endpoint.FepConfigurerMgr
import io.greenbus.edge.configure.sql.server.{ ModuleDb, ModuleDbEntry }
import io.greenbus.edge.data.{ IndexableValue, Value, ValueString }
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

case class ModuleValueUpdate(component: String, nodeOpt: Option[String], data: Array[Byte])
case class ModuleValueRemove(component: String, nodeOpt: Option[String])

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
  private var producerHandleOpt = Option.empty[ProducerHandle]

  def setProducerHandle(handle: ProducerHandle): Unit = {
    producerHandleOpt = Some(handle)
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

  def onModuleUpdates(module: String, removes: Seq[ModuleValueRemove], updates: Seq[ModuleValueUpdate]): Unit = {

    removes.foreach { mvr =>
      for {
        node <- mvr.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvr.component)
      } {
        subj.updates(Set(module), Seq())
        producerHandleOpt.foreach(_.flush())
      }
    }

    updates.foreach { mvu =>
      for {
        node <- mvu.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvu.component)
      } {
        subj.updates(Set(), Seq((module, mvu.data)))
        producerHandleOpt.foreach(_.flush())
      }
    }
  }
}
