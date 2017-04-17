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

import io.greenbus.edge.api.Path
import io.greenbus.edge.api.stream.{ KeyMetadata, ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.edm.core.EdgeCoreModel
import io.greenbus.edge.fep.model.FrontendEndpointConfiguration
import io.greenbus.edge.flow.Sink
import io.greenbus.edge.peer.ProducerServices

case class KeyRecord(handle: SeriesValueHandle, processor: KeyProcessor)

object PublisherHandle {

  def load(services: ProducerServices, config: FrontendEndpointConfiguration): PublisherHandle = {

    val builder = services.endpointBuilder(config.endpointId)

    val dataKeyMap = config.dataKeys.map { fdk =>

      val unitMetaOpt = fdk.seriesDescriptor.unit.map(EdgeCoreModel.unitMetadata)
      val boolLabelMetaOpt = fdk.seriesDescriptor.labeledBoolean.map(l => EdgeCoreModel.labeledBooleanMetadata(l.trueLabel, l.falseLabel))
      val intLabelMetaOpt = fdk.seriesDescriptor.labeledInteger.map(EdgeCoreModel.labeledIntegerMetadata)

      val indexes = fdk.indexes
      val metadata = fdk.metadata ++ Seq(unitMetaOpt, boolLabelMetaOpt, intLabelMetaOpt).flatten

      val keyMetadata = KeyMetadata(indexes, metadata)

      val sink: SeriesValueHandle = builder.seriesValue(fdk.path, keyMetadata)

      val proc = KeyProcessor.load(fdk.transforms, fdk.filter)

      (fdk.gatewayKey, KeyRecord(sink, proc))

    }.toMap

    val handle: ProducerHandle = builder.build(100, 100)

    new PublisherHandle(handle, dataKeyMap)
  }

}
class PublisherHandle(handle: ProducerHandle, map: Map[String, KeyRecord]) {

  def batch(batch: Seq[(String, SampleValue)]): Unit = {
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
