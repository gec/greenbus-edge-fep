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
import io.greenbus.edge.api.stream.{ ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.dnp3.config.Example
import io.greenbus.edge.dnp3.config.model._
import io.greenbus.edge.fep.PublisherHandle
import io.greenbus.edge.fep.model._
import io.greenbus.edge.flow.{ QueuedDistributor, Sink }
import io.greenbus.edge.peer.{ AmqpEdgeService, ProducerServices }
import io.greenbus.edge.thread.{ CallMarshaller, EventThreadService }

import scala.concurrent.ExecutionContext.Implicits.global

object EdgeDNP3Gateway {

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
          "control0",
          index = 0,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.PULSE,
            onTime = Some(100),
            offTime = Some(100),
            count = Some(1))), Control(
          "control1_on",
          index = 1,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.LATCH_ON,
            onTime = None,
            offTime = None,
            count = None)), Control(
          "control1_off",
          index = 1,
          function = FunctionType.SelectBeforeOperate,
          controlOptions = ControlOptions(
            controlType = ControlType.LATCH_OFF,
            onTime = None,
            offTime = None,
            count = None))),
        setpoints = Seq(Setpoint(
          "setpoint0",
          index = 0,
          function = FunctionType.SelectBeforeOperate))))
  }

  def buildFep: FrontendEndpointConfiguration = {

    FrontendEndpointConfiguration(
      EndpointId(Path(Seq("mthy", "mgrid", "ess01"))),
      Seq(
        FrontendDataKey(
          "analog_0",
          Path("outputPower"),
          SeriesDescriptor(
            SeriesType.AnalogStatus,
            unit = Some("kW"),
            decimalPoints = Some(2),
            None,
            None),
          Seq(
            LinearTransform(2.0, 5.0)),
          FilterDescriptor(None, None),
          Map(),
          Map()),
        FrontendDataKey(
          "analog_1",
          Path("mode"),
          SeriesDescriptor(
            SeriesType.IntegerEnum,
            None,
            None,
            labeledInteger = Some(Map(0L -> "Constant", 1L -> "Smoothing", 2L -> "GridForming")),
            None),
          Seq(),
          FilterDescriptor(None, None),
          Map(),
          Map()),
        FrontendDataKey(
          "binary_0",
          Path("faultStatus"),
          SeriesDescriptor(
            SeriesType.BooleanStatus,
            None,
            None,
            None,
            labeledBoolean = Some(BooleanLabels("Fault", "Normal"))),
          Seq(),
          FilterDescriptor(None, None),
          Map(),
          Map())))
  }

  def main(args: Array[String]): Unit = {

    val services = AmqpEdgeService.build("127.0.0.1", 50001, 10000)
    services.start()
    val producerServices = services.producer

    val config = buildGateway

    val endConfig = buildFep

    val eventThread = EventThreadService.build("DNP MGR")

    val gatewayMgr = new DNPGatewayMgr(eventThread, "local-me", producerServices)
    gatewayMgr.onGatewayConfigured(endConfig, config)

    System.in.read()

    gatewayMgr.close()
    services.shutdown()
    eventThread.close()
  }
}
