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
import io.greenbus.edge.fep._
import io.greenbus.edge.modbus.config.model.{ ModbusGateway, ProtocolType }
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller
import org.totalgrid.modbus._

class ModbusMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices, eventSink: EventSink) extends ConfigurationHandler[ModbusGateway] with LazyLogging {
  private val modbus = ModbusManager.start(8192, 6)
  private var resources = Map.empty[String, (RawModbusEndpoint, FrontendPublisher, ModbusMaster)]

  def onConfigured(key: String, config: ModbusGateway): Unit = {
    eventThread.marshal {
      logger.info(s"Gateway configured: $key")

      val (measObserver, cmdAdapter) = RawModbusEndpoint.build(eventThread, localId, producerServices, config)
      val outputAdapter = new CommandAdapter(config.modbus.commandMappings)

      val gatewayPub = FrontendPublisher.load(eventThread, producerServices, outputAdapter, config.endpoint)

      val observer = new SplittingMeasObserver(Seq(
        measObserver,
        new FrontendAdapter(gatewayPub)))

      val connectionTimeoutMs = 5000
      val operationTimeoutMs = 5000

      val polls = PollConfigConverter.load(config)

      val mapper = new InputMapping(config.modbus.discreteInputs, config.modbus.coilStatuses, config.modbus.inputRegisters, config.modbus.holdingRegisters)
      val obs = new DeviceObserver(mapper, observer)

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

      cmdAdapter.setOps(master)
      resources += (key -> (measObserver, gatewayPub, master))
    }
  }

  def onRemoved(key: String): Unit = {
    logger.info(s"Gateway removed: $key")
    eventThread.marshal {
      eventSink.publishEvent(Seq("source", "updated"), s"Gateway removed: $key")
      remove(key)
    }
  }

  private def remove(key: String): Unit = {
    resources.get(key).foreach {
      case (handle1, handle2, master) =>
        master.close()
        handle1.close()
        handle2.close()
    }
  }

  def close(): Unit = {
    logger.info(s"Gateway mgr closed")
    eventThread.marshal {
      resources.foreach {
        case (_, (handle1, handle2, master)) =>
          master.close()
          handle1.close()
          handle2.close()
      }

      modbus.shutdown()
    }
  }
}
