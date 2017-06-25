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

import scala.collection.mutable

object NodeComponentSubject {

  case class Updates(removes: Set[String], updates: Seq[(String, Array[Byte])])

  sealed trait State
  case class Pending(buffer: mutable.ArrayBuffer[Updates]) extends State
  case class Resolved(current: Map[String, Array[Byte]]) extends State

}
class NodeComponentSubject(val sequence: Long, activeSetHandle: ActiveSetHandle) {
  import NodeComponentSubject._
  private var state: State = Pending(mutable.ArrayBuffer.empty[Updates])

  def original(map: Map[String, Array[Byte]]): Unit = {
    state match {
      case Pending(buffer) => {

        val withUpdates = buffer.foldLeft(map) {
          case (curr, update) =>
            (curr -- update.removes) ++ update.updates
        }

        val rendered = ModuleIndex.toActiveSetMap(withUpdates)
        activeSetHandle.update(rendered)
        state = Resolved(withUpdates)
      }
      case Resolved(_) =>
    }
  }

  def updates(removes: Set[String], updates: Seq[(String, Array[Byte])]): Unit = {
    state match {
      case Pending(buffer) => {
        buffer += Updates(removes, updates)
      }
      case Resolved(current) => {
        val updated = (current -- removes) ++ updates
        val rendered = ModuleIndex.toActiveSetMap(updated)
        activeSetHandle.update(rendered)
        state = Resolved(updated)
      }
    }
  }
}