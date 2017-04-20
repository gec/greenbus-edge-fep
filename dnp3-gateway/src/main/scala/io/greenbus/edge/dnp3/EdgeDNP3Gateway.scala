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
        binaries = IndexSet(Seq(IndexRange(0, 10))),
        setpoints = IndexSet(Seq(IndexRange(0, 10)))))
  }

  /*
  case class IndexDescriptor(key: Path, value: IndexableValue)
case class MetadataDescriptor(key: Path, value: IndexableValue)

case class FrontendDataKey(
  gatewayKey: String,
  path: Path,
  seriesDescriptor: SeriesDescriptor,
  transforms: Seq[TransformDescriptor],
  filter: FilterDescriptor,
  indexes: Map[Path, IndexableValue],
  metadata: Map[Path, Value])

case class FrontendEndpointConfiguration(
  endpointId: EndpointId,
  dataKeys: Seq[FrontendDataKey])

sealed trait SampleType
object SampleType {
  case object Float extends SampleType
  case object Double extends SampleType
  case object Int32 extends SampleType
  case object UInt32 extends SampleType
  case object Int64 extends SampleType
  case object UInt64 extends SampleType
  case object Bool extends SampleType
  case object Byte extends SampleType
}

sealed trait TransformDescriptor
case class TypeCast(target: SampleType) extends TransformDescriptor
case class LinearTransform(scale: Double, offset: Double) extends TransformDescriptor
case object Negate extends TransformDescriptor

case class FilterDescriptor(suppressDuplicates: Option[Boolean], deadband: Option[Double])

case class BooleanLabels(trueLabel: String, falseLabel: String)

case class SeriesDescriptor(
  unit: Option[String],
  labeledInteger: Option[Map[Long, String]],
  labeledBoolean: Option[BooleanLabels])


   */

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

class SplittingMeasObserver(observers: Seq[MeasObserver]) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    observers.foreach(_.flush(batch))
  }
}

class FrontendAdapter(handle: PublisherHandle) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    handle.batch(batch)
  }
}

class DNPGatewayMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices) {

  private val mgr = new Dnp3Mgr[String]

  def onGatewayConfigured(endpointConfig: FrontendEndpointConfiguration, config: DNP3Gateway): Unit = {
    eventThread.marshal {
      val name = config.client.host + ":" + config.client.port
      val stackConfig = Dnp3MasterConfig.load(config)

      val measObserver = RawDnpEndpoint.build(localId, producerServices, config)
      def commsObs(value: Boolean): Unit = println("got comms: " + value)

      val gatewayPub = PublisherHandle.load(producerServices, endpointConfig)

      val observer = new SplittingMeasObserver(Seq(
        measObserver,
        new FrontendAdapter(gatewayPub)))

      mgr.add(name, name, stackConfig, observer, commsObs)
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

    val cfgKey = b.latestKeyValue(Path("config"))

    val built = b.build(100, 100)

    cfgKey.update(DNP3Gateway.write(config))

    new RawDnpEndpoint(built, dataKeyMap)
  }
}
class RawDnpEndpoint(handle: ProducerHandle, mapping: Map[String, SeriesValueHandle]) extends MeasObserver with LazyLogging {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    //logger.debug(s"Raw batch: ${batch}")
    val now = System.currentTimeMillis()
    batch.foreach {
      case (key, sample) =>
        mapping.get(key).foreach(handle => handle.update(sample, now))
    }
    handle.flush()
  }
}