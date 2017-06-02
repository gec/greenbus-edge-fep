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

import io.greenbus.edge.fep.NodeSettings
import io.greenbus.edge.peer.{ AmqpEdgeService, PeerClientSettings }
import io.greenbus.edge.thread.EventThreadService

import scala.concurrent.ExecutionContext.Implicits.global

class ModbusGatewayRunner {

  def main(args: Array[String]): Unit = {
    val baseDir = Option(System.getProperty("io.greenbus.config.base")).getOrElse("")
    val amqpConfigPath = Option(System.getProperty("io.greenbus.edge.config.client")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.peer.client.cfg")
    val nodeConfig = Option(System.getProperty("io.greenbus.edge.config.node")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.node.cfg")
    val clientSettings = PeerClientSettings.load(amqpConfigPath)
    val nodeSettings = NodeSettings.load(nodeConfig)

    val services = AmqpEdgeService.build(clientSettings.host, clientSettings.port, retryIntervalMs = clientSettings.retryIntervalMs, connectTimeoutMs = clientSettings.connectTimeoutMs)
    services.start()
    val producerServices = services.producer

    val eventThread = EventThreadService.build("Modbus MGR")

    val gatewayId = nodeSettings.name
  }
}
