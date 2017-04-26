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
package io.greenbus.edge.fep

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.api.stream.{ KeyMetadata, OutputStatusHandle, ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.edm.core.EdgeCoreModel
import io.greenbus.edge.fep.config.model.{ FrontendConfiguration, IntegerLabelSet, OutputType, SeriesType }
import io.greenbus.edge.flow.{ Receiver, Sink }
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller

import scala.collection.mutable

case class KeyRecord(handle: SeriesValueHandle, processor: KeyProcessor)
case class ControlEntry(path: Path, name: String, status: OutputStatusHandle, rcv: Receiver[OutputParams, OutputResult])

trait FrontendOutputDelegate {
  def handleOutput(name: String, params: OutputParams, respond: OutputResult => Unit): Unit
}

object FrontendPublisher {

  private def readLabels(labels: IntegerLabelSet): Map[Long, String] = {
    labels.value.map(l => (l.value.toLong, l.label)).toMap
  }

  def load(eventThread: CallMarshaller, services: ProducerServices, delegate: FrontendOutputDelegate, config: FrontendConfiguration): FrontendPublisher = {

    val builder = services.endpointBuilder(EndpointId(Path(config.endpointId.value)))

    val dataKeyMap = config.dataKeys.map { fdk =>

      val seriesType = fdk.descriptor.seriesType match {
        case SeriesType.AnalogStatus => EdgeCoreModel.SeriesType.AnalogStatus
        case SeriesType.AnalogSample => EdgeCoreModel.SeriesType.AnalogSample
        case SeriesType.CounterStatus => EdgeCoreModel.SeriesType.CounterStatus
        case SeriesType.CounterSample => EdgeCoreModel.SeriesType.CounterSample
        case SeriesType.BooleanStatus => EdgeCoreModel.SeriesType.BooleanStatus
        case SeriesType.IntegerEnum => EdgeCoreModel.SeriesType.IntegerEnum
      }
      val seriesMeta = EdgeCoreModel.seriesType(seriesType)
      val unitMetaOpt = fdk.descriptor.unit.map(EdgeCoreModel.unitMetadata)
      val boolLabelMetaOpt = fdk.descriptor.booleanLabels.map(l => EdgeCoreModel.labeledBooleanMetadata(l.trueLabel, l.falseLabel))
      val intLabelMetaOpt = fdk.descriptor.integerLabels.map(labels => readLabels(labels)).map(EdgeCoreModel.labeledIntegerMetadata)
      val decimalOpt = fdk.descriptor.decimalPoints.map(EdgeCoreModel.analogDecimalPoints)

      val metadata = (Seq(seriesMeta) ++ Seq(unitMetaOpt, boolLabelMetaOpt, intLabelMetaOpt, decimalOpt).flatten).toMap

      val keyMetadata = KeyMetadata(Map(), metadata)

      val sink: SeriesValueHandle = builder.seriesValue(Path(fdk.path.value), keyMetadata)

      val proc = KeyProcessor.load(fdk.transforms, fdk.filter)

      (fdk.gatewayKey, KeyRecord(sink, proc))

    }.toMap

    val outputEntries = config.outputKeys.map { fok =>

      val outputType = fok.descriptor.outputType match {
        case OutputType.AnalogSetpoint => EdgeCoreModel.OutputType.AnalogSetpoint
        case OutputType.SimpleIndication => EdgeCoreModel.OutputType.SimpleIndication
        case OutputType.BooleanSetpoint => EdgeCoreModel.OutputType.BooleanSetpoint
        case OutputType.EnumerationSetpoint => EdgeCoreModel.OutputType.EnumerationSetpoint
      }

      val typMeta = EdgeCoreModel.outputType(outputType)
      val boolLabelOpt = fok.descriptor.requestBooleanLabels.map(l => EdgeCoreModel.requestBooleanLabels(l.trueLabel, l.falseLabel))
      val intLabelOpt = fok.descriptor.requestIntegerLabels.map(readLabels).map(EdgeCoreModel.requestIntegerLabels)
      val reqScaleOpt = fok.descriptor.requestScale.map(EdgeCoreModel.requestScale)
      val reqOffsetOpt = fok.descriptor.requestOffset.map(EdgeCoreModel.requestOffset)

      val metadata = (Seq(typMeta) ++ Seq(boolLabelOpt, intLabelOpt, reqScaleOpt, reqOffsetOpt).flatten).toMap

      val keyMetadata = KeyMetadata(Map(), metadata)

      val keyHandle = builder.outputStatus(Path(fok.path.value), keyMetadata)
      val rcv = builder.registerOutput(Path(fok.path.value))

      ControlEntry(Path(fok.path.value), fok.gatewayKey, keyHandle, rcv)
    }

    val handle: ProducerHandle = builder.build(100, 100)

    new FrontendPublisher(eventThread, handle, delegate, dataKeyMap, outputEntries)
  }

}
class FrontendPublisher(eventThread: CallMarshaller, handle: ProducerHandle, delegate: FrontendOutputDelegate, map: Map[String, KeyRecord], outputEntries: Seq[ControlEntry]) extends LazyLogging {

  private val session = UUID.randomUUID()
  private val sequenceMap = mutable.Map.empty[String, Long]
  eventThread.marshal {
    init()
  }

  def init(): Unit = {
    outputEntries.foreach { entry =>
      sequenceMap.put(entry.name, 0)
      entry.status.update(OutputKeyStatus(session, 0, None))

      entry.rcv.bind(new io.greenbus.edge.flow.Responder[OutputParams, OutputResult] {
        def handle(params: OutputParams, respond: (OutputResult) => Unit): Unit = {
          eventThread.marshal {
            handleOutput(entry.name, params, entry.status, respond)
          }
        }
      })
    }
  }

  private def handleOutput(name: String, params: OutputParams, handle: OutputStatusHandle, respond: OutputResult => Unit): Unit = {
    logger.debug(s"Handling output for $name: $params")

    val currentSeq = sequenceMap.getOrElseUpdate(name, 0)

    if ((params.sessionOpt.isEmpty || params.sessionOpt.contains(session)) &&
      (params.sequenceOpt.isEmpty || params.sequenceOpt.contains(currentSeq))) {

      delegate.handleOutput(name, params, respond)
      sequenceMap.put(name, currentSeq + 1)
      handle.update(OutputKeyStatus(session, currentSeq + 1, None))

    } else {
      respond(OutputFailure(s"Parameters did not match"))
    }
  }

  def batch(batch: Seq[(String, SampleValue)]): Unit = {
    eventThread.marshal {
      logger.trace(s"Saw batch: " + batch)
      val now = System.currentTimeMillis()
      var dirty = false
      batch.foreach {
        case (key, v) =>
          map.get(key).foreach {
            case (keyRecord) => {
              keyRecord.processor.process(v).foreach { update =>
                keyRecord.handle.update(update, now)
                dirty = true
              }
            }
          }
      }
      if (dirty) {
        handle.flush()
      }
    }
  }
}
