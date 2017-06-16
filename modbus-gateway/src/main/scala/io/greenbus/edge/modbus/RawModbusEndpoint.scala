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

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.data.{ FloatingPointValue, IntegerValue, SampleValue }
import io.greenbus.edge.edm.core.EdgeCoreModel
import io.greenbus.edge.fep.{ ControlEntry, FrontendOutputDelegate, MeasObserver }
import io.greenbus.edge.flow
import io.greenbus.edge.flow.Receiver
import io.greenbus.edge.modbus.config.model.{ BooleanInput, CommandType, ModbusGateway, NumericInput }
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable

object RawModbusEndpoint {
  def build(eventThread: CallMarshaller, localId: String, producerServices: ProducerService, config: ModbusGateway): (RawModbusEndpoint, CommandAdapter) = {

    val path = Path(Seq("modbus", localId, s"${config.modbus.tcpClient.host}_${config.modbus.tcpClient.port}"))
    val b = producerServices.endpointBuilder(EndpointId(path))

    def mapBi(bi: BooleanInput) = {
      (bi.name, b.seriesValue(Path(bi.name), KeyMetadata(metadata = Map(EdgeCoreModel.seriesType(EdgeCoreModel.SeriesType.BooleanStatus)))))
    }
    def mapNi(ni: NumericInput) = {
      (ni.name, b.seriesValue(Path(ni.name), KeyMetadata(metadata = Map(EdgeCoreModel.seriesType(EdgeCoreModel.SeriesType.AnalogStatus)))))
    }

    val dataKeys = config.modbus.discreteInputs.map(mapBi) ++
      config.modbus.coilStatuses.map(mapBi) ++
      config.modbus.inputRegisters.map(mapNi) ++
      config.modbus.holdingRegisters.map(mapNi)

    val dataKeyMap: Map[String, SeriesValueHandle] = dataKeys.toMap

    val cfgKey = b.latestKeyValue(Path("config"))

    val controlEntries = Vector.newBuilder[ControlEntry]

    config.modbus.commandMappings.foreach { outMap =>
      val path = Path(outMap.name)

      val status: OutputStatusHandle = outMap.commandType match {
        case CommandType.Coil =>
          b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.BooleanSetpoint))))
        case CommandType.Register =>
          b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.AnalogSetpoint))))
        case CommandType.MaskRegister =>
          b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.AnalogSetpoint))))
        case CommandType.MultipleRegisters =>
          b.outputStatus(path, KeyMetadata(metadata = Map(EdgeCoreModel.outputType(EdgeCoreModel.OutputType.AnalogSetpoint))))
      }

      val rcv: Receiver[OutputParams, OutputResult] = b.registerOutput(path)
      controlEntries += ControlEntry(path, outMap.name, status, rcv)
    }

    val built = b.build()

    cfgKey.update(ModbusGateway.write(config))

    val commandAdapter = new CommandAdapter(config.modbus.commandMappings)

    (new RawModbusEndpoint(eventThread, built, dataKeyMap, commandAdapter, controlEntries.result()), commandAdapter)
  }
}
class RawModbusEndpoint(eventThread: CallMarshaller,
    handle: ProducerHandle,
    mapping: Map[String, SeriesValueHandle],
    commandAdapter: CommandAdapter,
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

    val currentSeq = sequenceMap.getOrElseUpdate(name, 0)

    if ((params.sessionOpt.isEmpty || params.sessionOpt.contains(session)) &&
      (params.sequenceOpt.isEmpty || params.sequenceOpt.contains(currentSeq))) {

      commandAdapter.handleOutput(name, params, respond)
      sequenceMap.put(name, currentSeq + 1)
      handle.update(OutputKeyStatus(session, currentSeq + 1, None))

    } else {

      respond(OutputFailure(s"Parameters did not match"))
    }
  }

  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    logger.trace(s"Raw batch: $batch")
    val now = System.currentTimeMillis()
    batch.foreach {
      case (key, sample) =>
        mapping.get(key).foreach(handle => handle.update(sample, now))
    }
    handle.flush()
  }

  def close(): Unit = {
    handle.close()
  }
}
