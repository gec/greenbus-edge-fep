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
package io.greenbus.edge.fep.model

import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.data.schema._
import io.greenbus.edge.data.{ IndexableValue, Value }
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
  dataKeys: Seq[FrontendDataKey],
  outputKeys: Seq[FrontendOutputKey])

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

sealed trait SeriesType
object SeriesType {
  case object AnalogStatus extends SeriesType
  case object AnalogSample extends SeriesType
  case object CounterStatus extends SeriesType
  case object CounterSample extends SeriesType
  case object BooleanStatus extends SeriesType
  case object IntegerEnum extends SeriesType
}

sealed trait TransformDescriptor
case class TypeCast(target: SampleType) extends TransformDescriptor
case class LinearTransform(scale: Double, offset: Double) extends TransformDescriptor
case object Negate extends TransformDescriptor

case class FilterDescriptor(suppressDuplicates: Option[Boolean], deadband: Option[Double])

case class BooleanLabels(trueLabel: String, falseLabel: String)

case class SeriesDescriptor(
  seriesType: SeriesType,
  unit: Option[String],
  decimalPoints: Option[Int],
  labeledInteger: Option[Map[Long, String]],
  labeledBoolean: Option[BooleanLabels])

sealed trait OutputType
object OutputType {
  case object SimpleIndication extends OutputType
  //case object ParameterizedIndication extends OutputType
  case object AnalogSetpoint extends OutputType
  case object BooleanSetpoint extends OutputType
  case object EnumerationSetpoint extends OutputType
}

case class OutputDescriptor(
  outputType: OutputType,
  requestScale: Option[Double],
  requestOffset: Option[Double],
  requestIntegerLabels: Option[Map[Long, String]],
  requestBooleanLabels: Option[BooleanLabels])

case class FrontendOutputKey(
  gatewayKey: String,
  path: Path,
  outputDescriptor: OutputDescriptor)

*/
