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
package io.greenbus.edge.modbus

import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.fep.{ KeyedDeviceObserver, MeasObserver }
import org.totalgrid.modbus.{ ChannelObserver, ModbusBit, ModbusDeviceObserver, ModbusRegister }

class DeviceObserver(map: InputMapping, observer: KeyedDeviceObserver) extends ModbusDeviceObserver with ChannelObserver {

  private def publish(results: Traversable[(String, SampleValue)]): Unit = {
    if (results.nonEmpty) observer.handleBatch(results.toSeq)
  }

  override def onReadDiscreteInput(list: Traversable[ModbusBit]): Unit = {
    publish(map.convertDiscreteInput(list))
  }

  override def onReadCoilStatus(list: Traversable[ModbusBit]): Unit = {
    publish(map.convertCoilStatus(list))
  }

  override def onReadHoldingRegister(list: Traversable[ModbusRegister]): Unit = {
    publish(map.convertHoldingRegister(list))
  }

  override def onReadInputRegister(list: Traversable[ModbusRegister]): Unit = {
    publish(map.convertInputRegister(list))
  }

  override def onCommSuccess(): Unit = {}

  override def onCommFailure(): Unit = {}

  def onChannelOpening(): Unit = {

  }

  def onChannelOnline(): Unit = {
    observer.handleOnline()
  }

  def onChannelOffline(): Unit = {
    observer.handleOffline()
  }
}