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

import io.greenbus.edge.api.ActiveSetHandle
import io.greenbus.edge.configure.endpoint.FepConfigurerMgr
import io.greenbus.edge.configure.sql.server.{ ModuleComponentValue, ModuleDb }
import io.greenbus.edge.data.{ IndexableValue, Value, ValueString }
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable

import scala.concurrent.ExecutionContext.Implicits.global

case class ModuleValueUpdate(component: String, nodeOpt: Option[String], data: Array[Byte])
case class ModuleValueRemove(component: String, nodeOpt: Option[String])

object ModuleIndex {

  def toActiveSetMap(values: Seq[ModuleComponentValue]): Map[IndexableValue, Value] = {
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
class ModuleIndex(eventThread: CallMarshaller, db: ModuleDb) {
  private var sequence: Long = 0
  private val nodeComponentMap = mutable.Map.empty[String, mutable.Map[String, NodeComponentSubject]]

  def register(node: String, component: String, handle: ActiveSetHandle): Unit = {
    val seq = sequence
    sequence += 1

    val componentMap = nodeComponentMap.getOrElseUpdate(node, mutable.Map.empty[String, NodeComponentSubject])
    componentMap.update(component, new NodeComponentSubject(seq, handle))

    db.valuesForNodeComponent(node, component).foreach { componentValues =>
      eventThread.marshal {
        nodeComponentMap.get(node).foreach { componentMap =>
          componentMap.get(component).foreach { subj =>
            if (subj.sequence == seq) {
              val simpleMap = componentValues.map(cv => (cv.module, cv.data)).toMap
              subj.original(simpleMap)
            }
          }
        }
      }
    }
  }
  def unregister(node: String, component: String): Unit = {
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
      } subj.updates(Set(module), Seq())
    }

    updates.foreach { mvu =>
      for {
        node <- mvu.nodeOpt
        componentMap <- nodeComponentMap.get(node)
        subj <- componentMap.get(mvu.component)
      } subj.updates(Set(), Seq((module, mvu.data)))
    }
  }
}

/*
class ModuleIndex(eventThread: CallMarshaller, db: ModuleDb) {
  private var sequence: Long = 0
  private val nodeComponentMap = mutable.Map.empty[String, mutable.Map[String, (Long, ActiveSetHandle, Map[String, Array[Byte]])]]

  //def getNodeComponent(node: String, component: String)

  def register(node: String, component: String, handle: ActiveSetHandle): Unit = {
    val seq = sequence
    sequence += 1

    val componentMap = nodeComponentMap.getOrElseUpdate(node, mutable.Map.empty[String, (Long, ActiveSetHandle, Map[String, Array[Byte]])])
    componentMap.update(component, (seq, handle, Map()))

    db.valuesForNodeComponent(node, component).foreach { componentValues =>
      eventThread.marshal {
        nodeComponentMap.get(node).foreach { componentMap =>
          componentMap.get(component).foreach {
            case (regSeq, _, _) =>
              if (regSeq == seq) {

                val simpleMap = componentValues.map(cv => (cv.module, cv.data)).toMap

                val value = ModuleIndex.toActiveSetMap(simpleMap)
                componentMap.put(component, (regSeq, handle, simpleMap))
                handle.update(value)
              }
          }
        }
      }
    }
  }
  def unregister(node: String, component: String): Unit = {
    nodeComponentMap.get(node).foreach { componentMap =>
      componentMap -= component
    }
    if (!nodeComponentMap.get(node).exists(_.nonEmpty)) {
      nodeComponentMap -= node
    }
  }

  //case class ModuleConfiguration(components: Map[String, (Value, Option[String])])

  def onModuleUpdates(module: String, removes: Seq[ModuleValueRemove], updates: Seq[ModuleValueUpdate]): Unit = {



  }

  /*def onModuleUpdate(module: String, config: ModuleConfiguration): Unit = {
    config.components.foreach {
      case (component, (value, nodeOpt)) =>



    }
  }*/

}*/
