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
import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.dnp3.config.model._
import io.greenbus.edge.dnp3.sub.ConfigSubscriber
import io.greenbus.edge.fep.{ GatewayEndpointPublisher, NodeSettings }
import io.greenbus.edge.peer.AmqpEdgeConnectionManager
import io.greenbus.edge.peer.PeerClientSettings
import io.greenbus.edge.thread.EventThreadService

import scala.concurrent.ExecutionContext.Implicits.global

object EdgeDNP3Gateway extends LazyLogging {
  logger.info("Initializing slf4j")

  def buildMaster: Master = {
    Master(
      StackConfig(
        LinkLayer(isMaster = true, localAddress = 100, remoteAddress = 1, userConfirmations = false, ackTimeoutMs = 1000, numRetries = 3),
        AppLayer(timeoutMs = 5000, maxFragSize = 2048, numRetries = 0)),
      MasterSettings(allowTimeSync = true, integrityPeriodMs = 300000, taskRetryMs = 5000),
      Seq(Scan(
        enableClass1 = true,
        enableClass2 = true,
        enableClass3 = true,
        periodMs = 2000)),
      Unsol(doTask = true, enable = true, enableClass1 = true, enableClass2 = true, enableClass3 = true))
  }

  def buildGateway: DNP3Gateway = {

    DNP3Gateway(buildMaster,
      TCPClient("127.0.0.1", 20000, 5000),
      InputModel(
        binaryInputs = IndexSet(Seq(IndexRange(0, 10))),
        analogInputs = IndexSet(Seq(IndexRange(0, 10))),
        counterInputs = IndexSet(Seq(IndexRange(0, 10))),
        binaryOutputs = IndexSet(Seq(IndexRange(0, 10))),
        analogOutputs = IndexSet(Seq(IndexRange(0, 10)))),
      OutputModel(
        controls = Seq(Control(
          "control_0",
          index = 0,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.PULSE,
            onTime = Some(100),
            offTime = Some(100),
            count = Some(1))), Control(
          "control_1_on",
          index = 1,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.LATCH_ON,
            onTime = None,
            offTime = None,
            count = None)), Control(
          "control_1_off",
          index = 1,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.LATCH_OFF,
            onTime = None,
            offTime = None,
            count = None))),
        setpoints = Seq(Setpoint(
          "setpoint_0",
          index = 0,
          function = FunctionType.SelectBeforeOperate),
          Setpoint(
            "setpoint_1",
            index = 1,
            function = FunctionType.SelectBeforeOperate))),
      FrontendConfigExample.build)
  }

  def main(args: Array[String]): Unit = {

    val baseDir = Option(System.getProperty("io.greenbus.config.base")).getOrElse("")
    val amqpConfigPath = Option(System.getProperty("io.greenbus.edge.config.client")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.peer.client.cfg")
    val nodeConfig = Option(System.getProperty("io.greenbus.edge.config.node")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.node.cfg")
    val clientSettings = PeerClientSettings.load(amqpConfigPath)
    val nodeSettings = NodeSettings.load(nodeConfig)

    val services = AmqpEdgeConnectionManager.build(
      clientSettings.host,
      clientSettings.port,
      retryIntervalMs = clientSettings.retryIntervalMs,
      connectTimeoutMs = clientSettings.connectTimeoutMs)

    val consumerServices = services.buildConsumerServices()
    val producerServices = services.buildProducerServices()
    services.start()

    val eventThread = EventThreadService.build("DNP MGR")

    val gatewayId = nodeSettings.name

    val publisher = new GatewayEndpointPublisher(eventThread, producerServices.endpointBuilder(EndpointId(Path(Seq("dnpgateway", gatewayId)))))

    val gatewayMgr = new DNPGatewayMgr(eventThread, gatewayId, producerServices, publisher)

    val configSubscriber = new ConfigSubscriber(eventThread, consumerServices, gatewayMgr, publisher)

    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      def run(): Unit = {
        gatewayMgr.close()
        services.shutdown()
        eventThread.close()
      }
    }))
  }
}
