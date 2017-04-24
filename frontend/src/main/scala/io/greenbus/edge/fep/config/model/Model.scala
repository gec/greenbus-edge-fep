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
package io.greenbus.edge.fep.config.model

import io.greenbus.edge.data.mapping._
import io.greenbus.edge.data._

object BooleanLabels {

  def read(element: Value, ctx: ReaderContext): Either[String, BooleanLabels] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type BooleanLabels did not recognize value type " + other)
        }
      case other => Left("Type BooleanLabels did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, BooleanLabels] = {
    val trueLabel = MappingLibrary.getMapField("trueLabel", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val falseLabel = MappingLibrary.getMapField("falseLabel", element).flatMap(elem => MappingLibrary.readString(elem, ctx))

    if (trueLabel.isRight && falseLabel.isRight) {
      Right(BooleanLabels(trueLabel.right.get, falseLabel.right.get))
    } else {
      Left(Seq(trueLabel.left.toOption, falseLabel.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: BooleanLabels): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("trueLabel"), ValueString(obj.trueLabel)),
      (ValueString("falseLabel"), ValueString(obj.falseLabel))))

    TaggedValue("BooleanLabels", built)
  }
}
case class BooleanLabels(trueLabel: String, falseLabel: String)

object DataKeyConfig {

  def read(element: Value, ctx: ReaderContext): Either[String, DataKeyConfig] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type DataKeyConfig did not recognize value type " + other)
        }
      case other => Left("Type DataKeyConfig did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, DataKeyConfig] = {
    val gatewayKey = MappingLibrary.getMapField("gatewayKey", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val path = MappingLibrary.getMapField("path", element).flatMap(elem => MappingLibrary.readFieldSubStruct("path", elem, "Path", io.greenbus.edge.fep.config.model.Path.read, ctx))
    val descriptor = MappingLibrary.getMapField("descriptor", element).flatMap(elem => MappingLibrary.readFieldSubStruct("descriptor", elem, "SeriesDescriptor", io.greenbus.edge.fep.config.model.SeriesDescriptor.read, ctx))
    val transforms = MappingLibrary.getMapField("transforms", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.fep.config.model.TransformDescriptor](elem, io.greenbus.edge.fep.config.model.TransformDescriptor.read, ctx))
    val filter = MappingLibrary.optMapField("filter", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => io.greenbus.edge.fep.config.model.FilterDescriptor.read(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (gatewayKey.isRight && path.isRight && descriptor.isRight && transforms.isRight && filter.isRight) {
      Right(DataKeyConfig(gatewayKey.right.get, path.right.get, descriptor.right.get, transforms.right.get, filter.right.get))
    } else {
      Left(Seq(gatewayKey.left.toOption, path.left.toOption, descriptor.left.toOption, transforms.left.toOption, filter.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: DataKeyConfig): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("gatewayKey"), ValueString(obj.gatewayKey)),
      (ValueString("path"), io.greenbus.edge.fep.config.model.Path.write(obj.path)),
      (ValueString("descriptor"), io.greenbus.edge.fep.config.model.SeriesDescriptor.write(obj.descriptor)),
      (ValueString("transforms"), MappingLibrary.writeList(obj.transforms, io.greenbus.edge.fep.config.model.TransformDescriptor.write)),
      (ValueString("filter"), obj.filter.map(p => io.greenbus.edge.fep.config.model.FilterDescriptor.write(p)).getOrElse(ValueNone))))

    TaggedValue("DataKeyConfig", built)
  }
}
case class DataKeyConfig(gatewayKey: String, path: io.greenbus.edge.fep.config.model.Path, descriptor: io.greenbus.edge.fep.config.model.SeriesDescriptor, transforms: Seq[io.greenbus.edge.fep.config.model.TransformDescriptor], filter: Option[io.greenbus.edge.fep.config.model.FilterDescriptor])

object FilterDescriptor {

  def read(element: Value, ctx: ReaderContext): Either[String, FilterDescriptor] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type FilterDescriptor did not recognize value type " + other)
        }
      case other => Left("Type FilterDescriptor did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, FilterDescriptor] = {
    val suppressDuplicates = MappingLibrary.optMapField("suppressDuplicates", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readBool(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val deadband = MappingLibrary.optMapField("deadband", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readDouble(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (suppressDuplicates.isRight && deadband.isRight) {
      Right(FilterDescriptor(suppressDuplicates.right.get, deadband.right.get))
    } else {
      Left(Seq(suppressDuplicates.left.toOption, deadband.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: FilterDescriptor): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("suppressDuplicates"), obj.suppressDuplicates.map(p => ValueBool(p)).getOrElse(ValueNone)),
      (ValueString("deadband"), obj.deadband.map(p => ValueDouble(p)).getOrElse(ValueNone))))

    TaggedValue("FilterDescriptor", built)
  }
}
case class FilterDescriptor(suppressDuplicates: Option[Boolean], deadband: Option[Double])

object FrontendConfiguration {

  def read(element: Value, ctx: ReaderContext): Either[String, FrontendConfiguration] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type FrontendConfiguration did not recognize value type " + other)
        }
      case other => Left("Type FrontendConfiguration did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, FrontendConfiguration] = {
    val endpointId = MappingLibrary.getMapField("endpointId", element).flatMap(elem => MappingLibrary.readFieldSubStruct("endpointId", elem, "Path", io.greenbus.edge.fep.config.model.Path.read, ctx))
    val dataKeys = MappingLibrary.getMapField("dataKeys", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.fep.config.model.DataKeyConfig](elem, io.greenbus.edge.fep.config.model.DataKeyConfig.read, ctx))
    val outputKeys = MappingLibrary.getMapField("outputKeys", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.fep.config.model.OutputKeyConfig](elem, io.greenbus.edge.fep.config.model.OutputKeyConfig.read, ctx))

    if (endpointId.isRight && dataKeys.isRight && outputKeys.isRight) {
      Right(FrontendConfiguration(endpointId.right.get, dataKeys.right.get, outputKeys.right.get))
    } else {
      Left(Seq(endpointId.left.toOption, dataKeys.left.toOption, outputKeys.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: FrontendConfiguration): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("endpointId"), io.greenbus.edge.fep.config.model.Path.write(obj.endpointId)),
      (ValueString("dataKeys"), MappingLibrary.writeList(obj.dataKeys, io.greenbus.edge.fep.config.model.DataKeyConfig.write)),
      (ValueString("outputKeys"), MappingLibrary.writeList(obj.outputKeys, io.greenbus.edge.fep.config.model.OutputKeyConfig.write))))

    TaggedValue("FrontendConfiguration", built)
  }
}
case class FrontendConfiguration(endpointId: io.greenbus.edge.fep.config.model.Path, dataKeys: Seq[io.greenbus.edge.fep.config.model.DataKeyConfig], outputKeys: Seq[io.greenbus.edge.fep.config.model.OutputKeyConfig])

object IntegerLabel {

  def read(element: Value, ctx: ReaderContext): Either[String, IntegerLabel] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type IntegerLabel did not recognize value type " + other)
        }
      case other => Left("Type IntegerLabel did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, IntegerLabel] = {
    val value = MappingLibrary.getMapField("value", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val label = MappingLibrary.getMapField("label", element).flatMap(elem => MappingLibrary.readString(elem, ctx))

    if (value.isRight && label.isRight) {
      Right(IntegerLabel(value.right.get, label.right.get))
    } else {
      Left(Seq(value.left.toOption, label.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: IntegerLabel): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("value"), ValueInt32(obj.value)),
      (ValueString("label"), ValueString(obj.label))))

    TaggedValue("IntegerLabel", built)
  }
}
case class IntegerLabel(value: Int, label: String)

object IntegerLabelSet {

  def read(element: Value, ctx: ReaderContext): Either[String, IntegerLabelSet] = {
    element match {
      case data: ValueList => readRepr(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueList => readRepr(data, ctx)
          case other => Left("Type IntegerLabelSet did not recognize value type " + other)
        }
      case other => Left("Type IntegerLabelSet did not recognize value type " + other)
    }
  }
  def readRepr(element: ValueList, ctx: ReaderContext): Either[String, IntegerLabelSet] = {
    MappingLibrary.readList[io.greenbus.edge.fep.config.model.IntegerLabel](element, io.greenbus.edge.fep.config.model.IntegerLabel.read, ctx).map(result => IntegerLabelSet(result))
  }
  def write(obj: IntegerLabelSet): TaggedValue = {
    val built = MappingLibrary.writeList(obj.value, io.greenbus.edge.fep.config.model.IntegerLabel.write)

    TaggedValue("IntegerLabelSet", built)
  }
}
case class IntegerLabelSet(value: Seq[io.greenbus.edge.fep.config.model.IntegerLabel])

object LinearTransform {

  def read(element: Value, ctx: ReaderContext): Either[String, LinearTransform] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type LinearTransform did not recognize value type " + other)
        }
      case other => Left("Type LinearTransform did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, LinearTransform] = {
    val scale = MappingLibrary.getMapField("scale", element).flatMap(elem => MappingLibrary.readDouble(elem, ctx))
    val offset = MappingLibrary.getMapField("offset", element).flatMap(elem => MappingLibrary.readDouble(elem, ctx))

    if (scale.isRight && offset.isRight) {
      Right(LinearTransform(scale.right.get, offset.right.get))
    } else {
      Left(Seq(scale.left.toOption, offset.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: LinearTransform): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("scale"), ValueDouble(obj.scale)),
      (ValueString("offset"), ValueDouble(obj.offset))))

    TaggedValue("LinearTransform", built)
  }
}
case class LinearTransform(scale: Double, offset: Double) extends TransformDescriptor

object OutputDescriptor {

  def read(element: Value, ctx: ReaderContext): Either[String, OutputDescriptor] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type OutputDescriptor did not recognize value type " + other)
        }
      case other => Left("Type OutputDescriptor did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, OutputDescriptor] = {
    val outputType = MappingLibrary.getMapField("outputType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("outputType", elem, "OutputType", io.greenbus.edge.fep.config.model.OutputType.read, ctx))
    val requestScale = MappingLibrary.optMapField("requestScale", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readDouble(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val requestOffset = MappingLibrary.optMapField("requestOffset", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readDouble(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val requestIntegerLabels = MappingLibrary.optMapField("requestIntegerLabels", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => io.greenbus.edge.fep.config.model.IntegerLabelSet.read(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val requestBooleanLabels = MappingLibrary.optMapField("requestBooleanLabels", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => io.greenbus.edge.fep.config.model.BooleanLabels.read(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (outputType.isRight && requestScale.isRight && requestOffset.isRight && requestIntegerLabels.isRight && requestBooleanLabels.isRight) {
      Right(OutputDescriptor(outputType.right.get, requestScale.right.get, requestOffset.right.get, requestIntegerLabels.right.get, requestBooleanLabels.right.get))
    } else {
      Left(Seq(outputType.left.toOption, requestScale.left.toOption, requestOffset.left.toOption, requestIntegerLabels.left.toOption, requestBooleanLabels.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: OutputDescriptor): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("outputType"), io.greenbus.edge.fep.config.model.OutputType.write(obj.outputType)),
      (ValueString("requestScale"), obj.requestScale.map(p => ValueDouble(p)).getOrElse(ValueNone)),
      (ValueString("requestOffset"), obj.requestOffset.map(p => ValueDouble(p)).getOrElse(ValueNone)),
      (ValueString("requestIntegerLabels"), obj.requestIntegerLabels.map(p => io.greenbus.edge.fep.config.model.IntegerLabelSet.write(p)).getOrElse(ValueNone)),
      (ValueString("requestBooleanLabels"), obj.requestBooleanLabels.map(p => io.greenbus.edge.fep.config.model.BooleanLabels.write(p)).getOrElse(ValueNone))))

    TaggedValue("OutputDescriptor", built)
  }
}
case class OutputDescriptor(outputType: io.greenbus.edge.fep.config.model.OutputType, requestScale: Option[Double], requestOffset: Option[Double], requestIntegerLabels: Option[io.greenbus.edge.fep.config.model.IntegerLabelSet], requestBooleanLabels: Option[io.greenbus.edge.fep.config.model.BooleanLabels])

object OutputKeyConfig {

  def read(element: Value, ctx: ReaderContext): Either[String, OutputKeyConfig] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type OutputKeyConfig did not recognize value type " + other)
        }
      case other => Left("Type OutputKeyConfig did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, OutputKeyConfig] = {
    val gatewayKey = MappingLibrary.getMapField("gatewayKey", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val path = MappingLibrary.getMapField("path", element).flatMap(elem => MappingLibrary.readFieldSubStruct("path", elem, "Path", io.greenbus.edge.fep.config.model.Path.read, ctx))
    val descriptor = MappingLibrary.getMapField("descriptor", element).flatMap(elem => MappingLibrary.readFieldSubStruct("descriptor", elem, "OutputDescriptor", io.greenbus.edge.fep.config.model.OutputDescriptor.read, ctx))

    if (gatewayKey.isRight && path.isRight && descriptor.isRight) {
      Right(OutputKeyConfig(gatewayKey.right.get, path.right.get, descriptor.right.get))
    } else {
      Left(Seq(gatewayKey.left.toOption, path.left.toOption, descriptor.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: OutputKeyConfig): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("gatewayKey"), ValueString(obj.gatewayKey)),
      (ValueString("path"), io.greenbus.edge.fep.config.model.Path.write(obj.path)),
      (ValueString("descriptor"), io.greenbus.edge.fep.config.model.OutputDescriptor.write(obj.descriptor))))

    TaggedValue("OutputKeyConfig", built)
  }
}
case class OutputKeyConfig(gatewayKey: String, path: io.greenbus.edge.fep.config.model.Path, descriptor: io.greenbus.edge.fep.config.model.OutputDescriptor)

object OutputType {

  case object SimpleIndication extends OutputType("SimpleIndication", 0)
  case object AnalogSetpoint extends OutputType("AnalogSetpoint", 1)
  case object BooleanSetpoint extends OutputType("BooleanSetpoint", 2)
  case object EnumerationSetpoint extends OutputType("EnumerationSetpoint", 3)

  def read(element: Value, ctx: ReaderContext): Either[String, OutputType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type OutputType did not recognize value type " + other)
        }
      case other => Left("Type OutputType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, OutputType] = {
    element.toInt match {
      case 0 => Right(SimpleIndication)
      case 1 => Right(AnalogSetpoint)
      case 2 => Right(BooleanSetpoint)
      case 3 => Right(EnumerationSetpoint)
      case other => Left("Enum OutputType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, OutputType] = {
    element.value match {
      case "SimpleIndication" => Right(SimpleIndication)
      case "AnalogSetpoint" => Right(AnalogSetpoint)
      case "BooleanSetpoint" => Right(BooleanSetpoint)
      case "EnumerationSetpoint" => Right(EnumerationSetpoint)
      case other => Left("Enum OutputType did not recognize string value " + other)
    }
  }
  def write(obj: OutputType): TaggedValue = {
    TaggedValue("OutputType", ValueUInt32(obj.value))
  }
}
sealed abstract class OutputType(val name: String, val value: Int)

object Path {

  def read(element: Value, ctx: ReaderContext): Either[String, Path] = {
    element match {
      case data: ValueList => readRepr(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueList => readRepr(data, ctx)
          case other => Left("Type Path did not recognize value type " + other)
        }
      case other => Left("Type Path did not recognize value type " + other)
    }
  }
  def readRepr(element: ValueList, ctx: ReaderContext): Either[String, Path] = {
    MappingLibrary.readList[String](element, MappingLibrary.readString, ctx).map(result => Path(result))
  }
  def write(obj: Path): TaggedValue = {
    val built = MappingLibrary.writeList(obj.value, ValueString)

    TaggedValue("Path", built)
  }
}
case class Path(value: Seq[String])

object SampleType {

  case object Float extends SampleType("Float", 0)
  case object Double extends SampleType("Double", 1)
  case object Int32 extends SampleType("Int32", 2)
  case object UInt32 extends SampleType("UInt32", 3)
  case object Int64 extends SampleType("Int64", 4)
  case object UInt64 extends SampleType("UInt64", 5)
  case object Bool extends SampleType("Bool", 6)
  case object Byte extends SampleType("Byte", 7)

  def read(element: Value, ctx: ReaderContext): Either[String, SampleType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type SampleType did not recognize value type " + other)
        }
      case other => Left("Type SampleType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, SampleType] = {
    element.toInt match {
      case 0 => Right(Float)
      case 1 => Right(Double)
      case 2 => Right(Int32)
      case 3 => Right(UInt32)
      case 4 => Right(Int64)
      case 5 => Right(UInt64)
      case 6 => Right(Bool)
      case 7 => Right(Byte)
      case other => Left("Enum SampleType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, SampleType] = {
    element.value match {
      case "Float" => Right(Float)
      case "Double" => Right(Double)
      case "Int32" => Right(Int32)
      case "UInt32" => Right(UInt32)
      case "Int64" => Right(Int64)
      case "UInt64" => Right(UInt64)
      case "Bool" => Right(Bool)
      case "Byte" => Right(Byte)
      case other => Left("Enum SampleType did not recognize string value " + other)
    }
  }
  def write(obj: SampleType): TaggedValue = {
    TaggedValue("SampleType", ValueUInt32(obj.value))
  }
}
sealed abstract class SampleType(val name: String, val value: Int)

object SeriesDescriptor {

  def read(element: Value, ctx: ReaderContext): Either[String, SeriesDescriptor] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type SeriesDescriptor did not recognize value type " + other)
        }
      case other => Left("Type SeriesDescriptor did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, SeriesDescriptor] = {
    val seriesType = MappingLibrary.getMapField("seriesType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("seriesType", elem, "SeriesType", io.greenbus.edge.fep.config.model.SeriesType.read, ctx))
    val unit = MappingLibrary.optMapField("unit", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readString(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val decimalPoints = MappingLibrary.optMapField("decimalPoints", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val integerLabels = MappingLibrary.optMapField("integerLabels", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => io.greenbus.edge.fep.config.model.IntegerLabelSet.read(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val booleanLabels = MappingLibrary.optMapField("booleanLabels", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => io.greenbus.edge.fep.config.model.BooleanLabels.read(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (seriesType.isRight && unit.isRight && decimalPoints.isRight && integerLabels.isRight && booleanLabels.isRight) {
      Right(SeriesDescriptor(seriesType.right.get, unit.right.get, decimalPoints.right.get, integerLabels.right.get, booleanLabels.right.get))
    } else {
      Left(Seq(seriesType.left.toOption, unit.left.toOption, decimalPoints.left.toOption, integerLabels.left.toOption, booleanLabels.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: SeriesDescriptor): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("seriesType"), io.greenbus.edge.fep.config.model.SeriesType.write(obj.seriesType)),
      (ValueString("unit"), obj.unit.map(p => ValueString(p)).getOrElse(ValueNone)),
      (ValueString("decimalPoints"), obj.decimalPoints.map(p => ValueUInt32(p)).getOrElse(ValueNone)),
      (ValueString("integerLabels"), obj.integerLabels.map(p => io.greenbus.edge.fep.config.model.IntegerLabelSet.write(p)).getOrElse(ValueNone)),
      (ValueString("booleanLabels"), obj.booleanLabels.map(p => io.greenbus.edge.fep.config.model.BooleanLabels.write(p)).getOrElse(ValueNone))))

    TaggedValue("SeriesDescriptor", built)
  }
}
case class SeriesDescriptor(seriesType: io.greenbus.edge.fep.config.model.SeriesType, unit: Option[String], decimalPoints: Option[Int], integerLabels: Option[io.greenbus.edge.fep.config.model.IntegerLabelSet], booleanLabels: Option[io.greenbus.edge.fep.config.model.BooleanLabels])

object SeriesType {

  case object AnalogStatus extends SeriesType("AnalogStatus", 0)
  case object AnalogSample extends SeriesType("AnalogSample", 1)
  case object CounterStatus extends SeriesType("CounterStatus", 2)
  case object CounterSample extends SeriesType("CounterSample", 3)
  case object BooleanStatus extends SeriesType("BooleanStatus", 4)
  case object IntegerEnum extends SeriesType("IntegerEnum", 5)

  def read(element: Value, ctx: ReaderContext): Either[String, SeriesType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type SeriesType did not recognize value type " + other)
        }
      case other => Left("Type SeriesType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, SeriesType] = {
    element.toInt match {
      case 0 => Right(AnalogStatus)
      case 1 => Right(AnalogSample)
      case 2 => Right(CounterStatus)
      case 3 => Right(CounterSample)
      case 4 => Right(BooleanStatus)
      case 5 => Right(IntegerEnum)
      case other => Left("Enum SeriesType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, SeriesType] = {
    element.value match {
      case "AnalogStatus" => Right(AnalogStatus)
      case "AnalogSample" => Right(AnalogSample)
      case "CounterStatus" => Right(CounterStatus)
      case "CounterSample" => Right(CounterSample)
      case "BooleanStatus" => Right(BooleanStatus)
      case "IntegerEnum" => Right(IntegerEnum)
      case other => Left("Enum SeriesType did not recognize string value " + other)
    }
  }
  def write(obj: SeriesType): TaggedValue = {
    TaggedValue("SeriesType", ValueUInt32(obj.value))
  }
}
sealed abstract class SeriesType(val name: String, val value: Int)

object SimpleTransform {

  def read(element: Value, ctx: ReaderContext): Either[String, SimpleTransform] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type SimpleTransform did not recognize value type " + other)
        }
      case other => Left("Type SimpleTransform did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, SimpleTransform] = {
    val transformType = MappingLibrary.getMapField("transformType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("transformType", elem, "SimpleTransformType", io.greenbus.edge.fep.config.model.SimpleTransformType.read, ctx))

    if (transformType.isRight) {
      Right(SimpleTransform(transformType.right.get))
    } else {
      Left(Seq(transformType.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: SimpleTransform): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("transformType"), io.greenbus.edge.fep.config.model.SimpleTransformType.write(obj.transformType))))

    TaggedValue("SimpleTransform", built)
  }
}
case class SimpleTransform(transformType: io.greenbus.edge.fep.config.model.SimpleTransformType) extends TransformDescriptor

object SimpleTransformType {

  case object Negate extends SimpleTransformType("Negate", 0)

  def read(element: Value, ctx: ReaderContext): Either[String, SimpleTransformType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type SimpleTransformType did not recognize value type " + other)
        }
      case other => Left("Type SimpleTransformType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, SimpleTransformType] = {
    element.toInt match {
      case 0 => Right(Negate)
      case other => Left("Enum SimpleTransformType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, SimpleTransformType] = {
    element.value match {
      case "Negate" => Right(Negate)
      case other => Left("Enum SimpleTransformType did not recognize string value " + other)
    }
  }
  def write(obj: SimpleTransformType): TaggedValue = {
    TaggedValue("SimpleTransformType", ValueUInt32(obj.value))
  }
}
sealed abstract class SimpleTransformType(val name: String, val value: Int)

object TransformDescriptor {

  def read(element: Value, ctx: ReaderContext): Either[String, TransformDescriptor] = {
    element match {
      case t: TaggedValue =>
        t.tag match {
          case "TypeCast" => io.greenbus.edge.fep.config.model.TypeCast.read(element, ctx)
          case "LinearTransform" => io.greenbus.edge.fep.config.model.LinearTransform.read(element, ctx)
          case "SimpleTransform" => io.greenbus.edge.fep.config.model.SimpleTransform.read(element, ctx)
          case other => throw new IllegalArgumentException("Type TransformDescriptor did not union type tag " + other)
        }
      case other => throw new IllegalArgumentException("Type TransformDescriptor did not recognize " + other)
    }
  }
  def write(obj: TransformDescriptor): TaggedValue = {
    obj match {
      case data: io.greenbus.edge.fep.config.model.TypeCast => io.greenbus.edge.fep.config.model.TypeCast.write(data)
      case data: io.greenbus.edge.fep.config.model.LinearTransform => io.greenbus.edge.fep.config.model.LinearTransform.write(data)
      case data: io.greenbus.edge.fep.config.model.SimpleTransform => io.greenbus.edge.fep.config.model.SimpleTransform.write(data)
      case other => throw new IllegalArgumentException("Type TransformDescriptor did not recognize " + other)
    }
  }
}
sealed trait TransformDescriptor

object TypeCast {

  def read(element: Value, ctx: ReaderContext): Either[String, TypeCast] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type TypeCast did not recognize value type " + other)
        }
      case other => Left("Type TypeCast did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, TypeCast] = {
    val target = MappingLibrary.getMapField("target", element).flatMap(elem => MappingLibrary.readFieldSubStruct("target", elem, "SampleType", io.greenbus.edge.fep.config.model.SampleType.read, ctx))

    if (target.isRight) {
      Right(TypeCast(target.right.get))
    } else {
      Left(Seq(target.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: TypeCast): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("target"), io.greenbus.edge.fep.config.model.SampleType.write(obj.target))))

    TaggedValue("TypeCast", built)
  }
}
case class TypeCast(target: io.greenbus.edge.fep.config.model.SampleType) extends TransformDescriptor

