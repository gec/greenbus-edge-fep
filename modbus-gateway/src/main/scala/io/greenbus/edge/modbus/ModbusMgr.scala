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

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.modbus.config.model.{ ModbusGateway, ProtocolType }
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller
import org.totalgrid.modbus._
import org.totalgrid.modbus.poll.Poll

class ModbusMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices) extends LazyLogging {
  private val modbus = ModbusManager.start(8192, 6)

  def onGatewayConfigured(key: String, config: ModbusGateway): Unit = {
    logger.info(s"Gateway configured: $key")

    val connectionTimeoutMs = 5000
    val operationTimeoutMs = 5000

    val polls: Seq[Poll] = Seq()

    val obs = new DeviceObserver

    val master = config.modbus.protocol match {
      case ProtocolType.RTU =>
        modbus.addRtuMaster(
          config.modbus.tcpClient.host,
          config.modbus.tcpClient.port,
          config.modbus.address.toByte,
          obs,
          obs,
          polls,
          connectionTimeoutMs,
          config.modbus.tcpClient.retryMs,
          operationTimeoutMs)
      case ProtocolType.TCPIP =>
        modbus.addTcpMaster(
          config.modbus.tcpClient.host,
          config.modbus.tcpClient.port,
          config.modbus.address.toByte,
          obs,
          obs,
          polls,
          connectionTimeoutMs,
          config.modbus.tcpClient.retryMs,
          operationTimeoutMs)
    }

  }

  def onGatewayRemoved(key: String): Unit = {

  }
}

class DeviceObserver extends ModbusDeviceObserver with ChannelObserver {

  override def onReadDiscreteInput(list: Traversable[ModbusBit]): Unit = {

  }

  override def onReadCoilStatus(list: Traversable[ModbusBit]): Unit = {

  }

  override def onReadHoldingRegister(list: Traversable[ModbusRegister]): Unit = {

  }

  override def onReadInputRegister(list: Traversable[ModbusRegister]): Unit = {

  }

  override def onCommSuccess(): Unit = {}

  override def onCommFailure(): Unit = {}

  def onChannelOpening(): Unit = {

  }

  def onChannelOnline(): Unit = {
    //statusUpdate(StackStatusUpdated(FrontEndConnectionStatus.Status.COMMS_UP))
  }

  def onChannelOffline(): Unit = {
    //statusUpdate(StackStatusUpdated(FrontEndConnectionStatus.Status.COMMS_DOWN))
  }
}