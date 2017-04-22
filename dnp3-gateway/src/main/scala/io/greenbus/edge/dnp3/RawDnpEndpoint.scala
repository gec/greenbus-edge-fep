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

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.api.stream.{ KeyMetadata, OutputStatusHandle, ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.data._
import io.greenbus.edge.dnp3.config.model._
import io.greenbus.edge.edm.core.EdgeCoreModel
import io.greenbus.edge.fep.{ ControlEntry, FrontendOutputDelegate }
import io.greenbus.edge.flow
import io.greenbus.edge.flow.Receiver
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable

//case class ControlEntry(path: Path, name: String, status: OutputStatusHandle, rcv: Receiver[OutputParams, OutputResult])

case class RawDnpOutputConfig(status: OutputStatusHandle, rcv: Receiver[OutputParams, OutputResult], typedConfig: DnpOutputTypeConfig)

sealed trait DnpOutputTypeConfig
case class DnpControlConfig(index: Int, function: FunctionType, options: ControlOptions) extends DnpOutputTypeConfig
case class DnpSetpointConfig(index: Int, function: FunctionType) extends DnpOutputTypeConfig

object RawDnpEndpoint {
  def build(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices, config: DNP3Gateway): (RawDnpEndpoint, DNPKeyedControlAdapter) = {

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

    val handleConfig = Vector.newBuilder[(String, DnpOutputTypeConfig)]
    val controlEntries = Vector.newBuilder[ControlEntry]

    config.outputModel.controls.foreach { control =>

      val path = Path(control.name)
      val status: OutputStatusHandle = b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.SimpleIndication))))
      val rcv: Receiver[OutputParams, OutputResult] = b.registerOutput(path)

      controlEntries += ControlEntry(path, control.name, status, rcv)
      handleConfig += (control.name -> DnpControlConfig(control.index, control.function, control.controlOptions))
    }

    config.outputModel.setpoints.map { setpoint =>

      val path = Path(setpoint.name)
      val status: OutputStatusHandle = b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.AnalogSetpoint))))
      val rcv: Receiver[OutputParams, OutputResult] = b.registerOutput(path)

      controlEntries += ControlEntry(path, setpoint.name, status, rcv)
      handleConfig += (setpoint.name -> DnpSetpointConfig(setpoint.index, setpoint.function))
    }

    val built = b.build(100, 100)

    cfgKey.update(DNP3Gateway.write(config))

    val controlAdapter = new DNPKeyedControlAdapterImpl(handleConfig.result().toMap)
    val endpoint = new RawDnpEndpoint(eventThread, built, controlAdapter, dataKeyMap, controlEntries.result())

    (endpoint, controlAdapter)
  }
}
class RawDnpEndpoint(eventThread: CallMarshaller,
    handle: ProducerHandle,
    controlAdapter: DNPKeyedControlAdapter,
    mapping: Map[String, SeriesValueHandle],
    outputs: Seq[ControlEntry]) extends MeasObserver with FrontendOutputDelegate with LazyLogging {

  private val session = UUID.randomUUID()
  private val sequenceMap = mutable.Map.empty[String, Long]
  private var outputMap = Map.empty[String, ControlEntry]
  eventThread.marshal {
    init()
  }

  def init(): Unit = {
    val mappings = outputs.map { entry =>
      sequenceMap.put(entry.name, 0)
      entry.status.update(OutputKeyStatus(session, 0, None))

      entry.rcv.bind(new flow.Responder[OutputParams, OutputResult] {
        def handle(params: OutputParams, respond: (OutputResult) => Unit): Unit = {
          eventThread.marshal {
            handleSelfOutput(entry.name, params, entry.status, respond)
          }
        }
      })

      (entry.name, entry)
    }

    outputMap = mappings.toMap
  }

  def handleOutput(name: String, params: OutputParams, respond: (OutputResult) => Unit): Unit = {
    eventThread.marshal {
      outputMap.get(name) match {
        case None => respond(OutputFailure(s"No mapped output for $name"))
        case Some(entry) =>
          handleSelfOutput(name, params, entry.status, respond)
      }
    }
  }

  private def handleSelfOutput(name: String, params: OutputParams, handle: OutputStatusHandle, respond: OutputResult => Unit): Unit = {
    logger.debug(s"Handling output for $name: $params")

    val setpointValueOpt = params.outputValueOpt.flatMap {
      case vi: IntegerValue => Some(IntegerSetpointValue(vi.toLong))
      case vi: FloatingPointValue => Some(DoubleSetpointValue(vi.toDouble))
      case _ => None
    }

    val currentSeq = sequenceMap.getOrElseUpdate(name, 0)

    if ((params.sessionOpt.isEmpty || params.sessionOpt.contains(session)) &&
      (params.sequenceOpt.isEmpty || params.sequenceOpt.contains(currentSeq))) {

      // TODO: check sequence
      controlAdapter.handle(name, setpointValueOpt, respond)
      sequenceMap.put(name, currentSeq + 1)
      handle.update(OutputKeyStatus(session, currentSeq + 1, None))

    } else {

      respond(OutputFailure(s"Parameters did not match"))
    }
  }

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

