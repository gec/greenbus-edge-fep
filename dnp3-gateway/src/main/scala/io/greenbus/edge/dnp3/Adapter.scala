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
package io.greenbus.edge.dnp3

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.data.{ SampleValue, ValueBool, ValueDouble, ValueInt32 }
import io.greenbus.edge.flow.Sink
import org.totalgrid.dnp3._

import scala.collection.mutable

class StackAdapter(obs: Boolean => Unit) extends IStackObserver {
  override def OnStateChange(aState: StackStates) {
    aState match {
      case StackStates.SS_COMMS_DOWN => obs(false)
      case StackStates.SS_COMMS_UP => obs(true)
      case StackStates.SS_UNKNOWN => obs(false)
    }
  }
}

trait CommsObserver {
  def commsUp(value: Boolean): Unit
}

trait MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit
}

object Dnp3Mgr {
  case class StackRecord(name: String, portName: String, measObserver: MeasAdapter, stackAdapter: StackAdapter)
}
class Dnp3Mgr {
  import Dnp3Mgr._

  private val stackManager = new StackManager
  private val logAdapter = new LogAdapter

  stackManager.AddLogHook(logAdapter)

  private var map = Map.empty[String, StackRecord]

  def add(key: String, config: Dnp3MasterConfig, measObserver: MeasObserver, commsObserver: Boolean => Unit): ICommandAcceptor = {
    remove(key)

    val portName = s"$key-${config.address}:${config.port}"
    println("PORT NAME: " + portName)
    val settings = new PhysLayerSettings(config.logLevel, config.retryMs)
    stackManager.AddTCPClient(portName, settings, config.address, config.port)

    val measAdapter = new MeasAdapter(measObserver)

    val stackAdapter = new StackAdapter(commsObserver)
    config.stack.getMaster.setMpObserver(stackAdapter)

    val commandAcceptor = stackManager.AddMaster(portName, key, config.logLevel, measAdapter, config.stack)

    map += (key -> StackRecord(key, portName, measAdapter, stackAdapter))

    commandAcceptor
  }

  def remove(key: String): Unit = {
    map.get(key).foreach { record =>
      stackManager.RemoveStack(record.name)
      stackManager.RemovePort(record.portName)
      map -= key
    }
  }

  def shutdown(): Unit = {
    stackManager.Shutdown()
  }
}

object MeasAdapter {

  val binaryPrefix = "binary"
  val analogPrefix = "analog"
  val counterPrefix = "counter"
  val controlStatusPrefix = "controlStatus"
  val setpointStatusPrefix = "setpointStatus"

  def id(prefix: String, index: Long): String = {
    s"${prefix}_$index"
  }

}
class MeasAdapter(observer: MeasObserver) extends IDataObserver with LazyLogging {
  import MeasAdapter._
  import SafeExecution._

  private var wallTime = Option.empty[Long]
  private val batch = mutable.ArrayBuffer.empty[(String, SampleValue)]

  override def _Start(): Unit = safeExecute {
    wallTime = Some(System.currentTimeMillis())
  }

  override def _End(): Unit = safeExecute {
    val result = batch.toVector
    batch.clear()
    if (result.nonEmpty) {
      observer.flush(result)
    }
  }
  override def _Update(v: Binary, index: Long): Unit = safeExecute {
    val value = ValueBool(v.get_binary())
    batch += ((id(binaryPrefix, index), value))
  }

  override def _Update(v: Analog, index: Long) = safeExecute {
    val value = ValueDouble(v.get_analog())
    batch += ((id(analogPrefix, index), value))
  }

  override def _Update(v: Counter, index: Long) = safeExecute {
    val value = ValueInt32(v.get_counter())
    batch += ((id(counterPrefix, index), value))
  }

  override def _Update(v: ControlStatus, index: Long) = safeExecute {
    val value = ValueBool(v.get_controlstatus())
    batch += ((id(controlStatusPrefix, index), value))
  }

  override def _Update(v: SetpointStatus, index: Long) = safeExecute {
    val value = ValueDouble(v.get_setpointstatus())
    batch += ((id(setpointStatusPrefix, index), value))
  }
}