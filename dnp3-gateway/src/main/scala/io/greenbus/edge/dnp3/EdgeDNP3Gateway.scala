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

import io.greenbus.edge.api.stream.{ ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.dnp3.config.Example
import io.greenbus.edge.dnp3.config.model._
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
        binaries = IndexSet(Seq(IndexRange(0, 10))),
        setpoints = IndexSet(Seq(IndexRange(0, 10)))))
  }

  def main(args: Array[String]): Unit = {

    val services = AmqpEdgeService.build("127.0.0.1", 50001, 10000)
    services.start()
    val producerServices = services.producer

    val config = buildGateway

    val eventThread = EventThreadService.build("DNP MGR")

    val gatewayMgr = new DNPGatewayMgr(eventThread, "local-me", producerServices)
    gatewayMgr.onGatewayConfigured(config)

    System.in.read()

    gatewayMgr.close()
    services.shutdown()
    eventThread.close()
  }
}

class DNPGatewayMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices) {

  private val mgr = new Dnp3Mgr[String]

  def onGatewayConfigured(config: DNP3Gateway): Unit = {
    eventThread.marshal {
      val name = config.client.host + ":" + config.client.port
      val stackConfig = Dnp3MasterConfig.load(config)

      val measObserver = RawDnpEndpoint.build(localId, producerServices, config)
      def commsObs(value: Boolean): Unit = println("got comms: " + value)

      mgr.add(name, name, stackConfig, measObserver, commsObs)
    }
  }

  def close(): Unit = {
    eventThread.marshal {
      mgr.shutdown()
    }
  }
}

object RawDnpEndpoint {
  def build(localId: String, producerServices: ProducerServices, config: DNP3Gateway) = {

    val path = Path(Seq("dnp", localId, s"${config.client.host}_${config.client.port}"))
    val b = producerServices.endpointBuilder(EndpointId(path))

    def loadRange(prefix: String, range: IndexRange): Seq[(String, SeriesValueHandle)] = {
      Range(range.start, range.count).map { i =>
        val key = MeasAdapter.id(prefix, i)
        (key, b.seriesValue(Path(Seq(key))))
      }
    }

    def loadSet(prefix: String, set: IndexSet): Seq[(String, SeriesValueHandle)] = {
      set.value.flatMap(loadRange(prefix, _))
    }

    val dataKeys = loadSet(MeasAdapter.binaryPrefix, config.inputModel.binaryInputs) ++
      loadSet(MeasAdapter.analogPrefix, config.inputModel.analogInputs) ++
      loadSet(MeasAdapter.counterPrefix, config.inputModel.counterInputs) ++
      loadSet(MeasAdapter.controlStatusPrefix, config.inputModel.binaryOutputs) ++
      loadSet(MeasAdapter.setpointStatusPrefix, config.inputModel.analogOutputs)

    val dataKeyMap: Map[String, SeriesValueHandle] = dataKeys.toMap

    val built = b.build(100, 100)

    new RawDnpEndpoint(built, dataKeyMap)
  }
}
class RawDnpEndpoint(handle: ProducerHandle, mapping: Map[String, SeriesValueHandle]) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    val now = System.currentTimeMillis()
    batch.foreach {
      case (key, sample) =>
        mapping.get(key).foreach(handle => handle.update(sample, now))
    }
    handle.flush()
  }
}