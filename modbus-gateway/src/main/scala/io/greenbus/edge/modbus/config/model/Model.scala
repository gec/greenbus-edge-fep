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
package io.greenbus.edge.modbus.config.model

import io.greenbus.edge.data.mapping._
import io.greenbus.edge.data._

object BooleanInput {

  def read(element: Value, ctx: ReaderContext): Either[String, BooleanInput] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type BooleanInput did not recognize value type " + other)
        }
      case other => Left("Type BooleanInput did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, BooleanInput] = {
    val index = MappingLibrary.getMapField("index", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val name = MappingLibrary.getMapField("name", element).flatMap(elem => MappingLibrary.readString(elem, ctx))

    if (index.isRight && name.isRight) {
      Right(BooleanInput(index.right.get, name.right.get))
    } else {
      Left(Seq(index.left.toOption, name.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: BooleanInput): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("index"), ValueUInt32(obj.index)),
      (ValueString("name"), ValueString(obj.name))))

    TaggedValue("BooleanInput", built)
  }
}
case class BooleanInput(index: Int, name: String)

object CommandType {

  case object Coil extends CommandType("Coil", 0)
  case object Register extends CommandType("Register", 1)
  case object MaskRegister extends CommandType("MaskRegister", 2)
  case object MultipleRegisters extends CommandType("MultipleRegisters", 3)

  def read(element: Value, ctx: ReaderContext): Either[String, CommandType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type CommandType did not recognize value type " + other)
        }
      case other => Left("Type CommandType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, CommandType] = {
    element.toInt match {
      case 0 => Right(Coil)
      case 1 => Right(Register)
      case 2 => Right(MaskRegister)
      case 3 => Right(MultipleRegisters)
      case other => Left("Enum CommandType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, CommandType] = {
    element.value match {
      case "Coil" => Right(Coil)
      case "Register" => Right(Register)
      case "MaskRegister" => Right(MaskRegister)
      case "MultipleRegisters" => Right(MultipleRegisters)
      case other => Left("Enum CommandType did not recognize string value " + other)
    }
  }
  def write(obj: CommandType): TaggedValue = {
    TaggedValue("CommandType", ValueUInt32(obj.value))
  }
}
sealed abstract class CommandType(val name: String, val value: Int)

object Conversion {

  case object SInt16 extends Conversion("SInt16", 0)
  case object UInt16 extends Conversion("UInt16", 1)
  case object SInt32LE extends Conversion("SInt32LE", 2)
  case object UInt32LE extends Conversion("UInt32LE", 3)
  case object SInt32BE extends Conversion("SInt32BE", 4)
  case object UInt32BE extends Conversion("UInt32BE", 5)
  case object Float32LE extends Conversion("Float32LE", 6)
  case object Float32BE extends Conversion("Float32BE", 7)
  case object Float64LE extends Conversion("Float64LE", 8)
  case object Float64BE extends Conversion("Float64BE", 9)

  def read(element: Value, ctx: ReaderContext): Either[String, Conversion] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type Conversion did not recognize value type " + other)
        }
      case other => Left("Type Conversion did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, Conversion] = {
    element.toInt match {
      case 0 => Right(SInt16)
      case 1 => Right(UInt16)
      case 2 => Right(SInt32LE)
      case 3 => Right(UInt32LE)
      case 4 => Right(SInt32BE)
      case 5 => Right(UInt32BE)
      case 6 => Right(Float32LE)
      case 7 => Right(Float32BE)
      case 8 => Right(Float64LE)
      case 9 => Right(Float64BE)
      case other => Left("Enum Conversion did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, Conversion] = {
    element.value match {
      case "SInt16" => Right(SInt16)
      case "UInt16" => Right(UInt16)
      case "SInt32LE" => Right(SInt32LE)
      case "UInt32LE" => Right(UInt32LE)
      case "SInt32BE" => Right(SInt32BE)
      case "UInt32BE" => Right(UInt32BE)
      case "Float32LE" => Right(Float32LE)
      case "Float32BE" => Right(Float32BE)
      case "Float64LE" => Right(Float64LE)
      case "Float64BE" => Right(Float64BE)
      case other => Left("Enum Conversion did not recognize string value " + other)
    }
  }
  def write(obj: Conversion): TaggedValue = {
    TaggedValue("Conversion", ValueUInt32(obj.value))
  }
}
sealed abstract class Conversion(val name: String, val value: Int)

object DataType {

  case object DiscreteInput extends DataType("DiscreteInput", 0)
  case object CoilStatus extends DataType("CoilStatus", 1)
  case object InputRegister extends DataType("InputRegister", 2)
  case object HoldingRegister extends DataType("HoldingRegister", 3)

  def read(element: Value, ctx: ReaderContext): Either[String, DataType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type DataType did not recognize value type " + other)
        }
      case other => Left("Type DataType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, DataType] = {
    element.toInt match {
      case 0 => Right(DiscreteInput)
      case 1 => Right(CoilStatus)
      case 2 => Right(InputRegister)
      case 3 => Right(HoldingRegister)
      case other => Left("Enum DataType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, DataType] = {
    element.value match {
      case "DiscreteInput" => Right(DiscreteInput)
      case "CoilStatus" => Right(CoilStatus)
      case "InputRegister" => Right(InputRegister)
      case "HoldingRegister" => Right(HoldingRegister)
      case other => Left("Enum DataType did not recognize string value " + other)
    }
  }
  def write(obj: DataType): TaggedValue = {
    TaggedValue("DataType", ValueUInt32(obj.value))
  }
}
sealed abstract class DataType(val name: String, val value: Int)

object Master {

  def read(element: Value, ctx: ReaderContext): Either[String, Master] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Master did not recognize value type " + other)
        }
      case other => Left("Type Master did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Master] = {
    val tcpClient = MappingLibrary.getMapField("tcpClient", element).flatMap(elem => MappingLibrary.readFieldSubStruct("tcpClient", elem, "TCPClient", io.greenbus.edge.modbus.config.model.TCPClient.read, ctx))
    val protocol = MappingLibrary.getMapField("protocol", element).flatMap(elem => MappingLibrary.readFieldSubStruct("protocol", elem, "ProtocolType", io.greenbus.edge.modbus.config.model.ProtocolType.read, ctx))
    val address = MappingLibrary.getMapField("address", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val polls = MappingLibrary.optMapField("polls", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.Poll](elem, io.greenbus.edge.modbus.config.model.Poll.read, ctx)).getOrElse(Right(Seq()))
    val discreteInputs = MappingLibrary.optMapField("discreteInputs", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.BooleanInput](elem, io.greenbus.edge.modbus.config.model.BooleanInput.read, ctx)).getOrElse(Right(Seq()))
    val coilStatuses = MappingLibrary.optMapField("coilStatuses", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.BooleanInput](elem, io.greenbus.edge.modbus.config.model.BooleanInput.read, ctx)).getOrElse(Right(Seq()))
    val inputRegisters = MappingLibrary.optMapField("inputRegisters", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.NumericInput](elem, io.greenbus.edge.modbus.config.model.NumericInput.read, ctx)).getOrElse(Right(Seq()))
    val holdingRegisters = MappingLibrary.optMapField("holdingRegisters", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.BooleanInput](elem, io.greenbus.edge.modbus.config.model.BooleanInput.read, ctx)).getOrElse(Right(Seq()))
    val commandMappings = MappingLibrary.optMapField("commandMappings", element).map(elem => MappingLibrary.readList[io.greenbus.edge.modbus.config.model.OutputMapping](elem, io.greenbus.edge.modbus.config.model.OutputMapping.read, ctx)).getOrElse(Right(Seq()))

    if (tcpClient.isRight && protocol.isRight && address.isRight && polls.isRight && discreteInputs.isRight && coilStatuses.isRight && inputRegisters.isRight && holdingRegisters.isRight && commandMappings.isRight) {
      Right(Master(tcpClient.right.get, protocol.right.get, address.right.get, polls.right.get, discreteInputs.right.get, coilStatuses.right.get, inputRegisters.right.get, holdingRegisters.right.get, commandMappings.right.get))
    } else {
      Left(Seq(tcpClient.left.toOption, protocol.left.toOption, address.left.toOption, polls.left.toOption, discreteInputs.left.toOption, coilStatuses.left.toOption, inputRegisters.left.toOption, holdingRegisters.left.toOption, commandMappings.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Master): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("tcpClient"), io.greenbus.edge.modbus.config.model.TCPClient.write(obj.tcpClient)),
      (ValueString("protocol"), io.greenbus.edge.modbus.config.model.ProtocolType.write(obj.protocol)),
      (ValueString("address"), ValueUInt32(obj.address)),
      (ValueString("polls"), MappingLibrary.writeList(obj.polls, io.greenbus.edge.modbus.config.model.Poll.write)),
      (ValueString("discreteInputs"), MappingLibrary.writeList(obj.discreteInputs, io.greenbus.edge.modbus.config.model.BooleanInput.write)),
      (ValueString("coilStatuses"), MappingLibrary.writeList(obj.coilStatuses, io.greenbus.edge.modbus.config.model.BooleanInput.write)),
      (ValueString("inputRegisters"), MappingLibrary.writeList(obj.inputRegisters, io.greenbus.edge.modbus.config.model.NumericInput.write)),
      (ValueString("holdingRegisters"), MappingLibrary.writeList(obj.holdingRegisters, io.greenbus.edge.modbus.config.model.BooleanInput.write)),
      (ValueString("commandMappings"), MappingLibrary.writeList(obj.commandMappings, io.greenbus.edge.modbus.config.model.OutputMapping.write))))

    TaggedValue("Master", built)
  }
}
case class Master(tcpClient: io.greenbus.edge.modbus.config.model.TCPClient, protocol: io.greenbus.edge.modbus.config.model.ProtocolType, address: Int, polls: Seq[io.greenbus.edge.modbus.config.model.Poll], discreteInputs: Seq[io.greenbus.edge.modbus.config.model.BooleanInput], coilStatuses: Seq[io.greenbus.edge.modbus.config.model.BooleanInput], inputRegisters: Seq[io.greenbus.edge.modbus.config.model.NumericInput], holdingRegisters: Seq[io.greenbus.edge.modbus.config.model.BooleanInput], commandMappings: Seq[io.greenbus.edge.modbus.config.model.OutputMapping])

object ModbusGateway {

  def read(element: Value, ctx: ReaderContext): Either[String, ModbusGateway] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type ModbusGateway did not recognize value type " + other)
        }
      case other => Left("Type ModbusGateway did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, ModbusGateway] = {
    val modbus = MappingLibrary.getMapField("modbus", element).flatMap(elem => MappingLibrary.readFieldSubStruct("modbus", elem, "Master", io.greenbus.edge.modbus.config.model.Master.read, ctx))
    val endpoint = MappingLibrary.getMapField("endpoint", element).flatMap(elem => MappingLibrary.readFieldSubStruct("endpoint", elem, "FrontendConfiguration", io.greenbus.edge.fep.config.model.FrontendConfiguration.read, ctx))

    if (modbus.isRight && endpoint.isRight) {
      Right(ModbusGateway(modbus.right.get, endpoint.right.get))
    } else {
      Left(Seq(modbus.left.toOption, endpoint.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: ModbusGateway): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("modbus"), io.greenbus.edge.modbus.config.model.Master.write(obj.modbus)),
      (ValueString("endpoint"), io.greenbus.edge.fep.config.model.FrontendConfiguration.write(obj.endpoint))))

    TaggedValue("ModbusGateway", built)
  }
}
case class ModbusGateway(modbus: io.greenbus.edge.modbus.config.model.Master, endpoint: io.greenbus.edge.fep.config.model.FrontendConfiguration)

object NumericInput {

  def read(element: Value, ctx: ReaderContext): Either[String, NumericInput] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type NumericInput did not recognize value type " + other)
        }
      case other => Left("Type NumericInput did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, NumericInput] = {
    val index = MappingLibrary.getMapField("index", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val name = MappingLibrary.getMapField("name", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val conversionType = MappingLibrary.getMapField("conversionType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("conversionType", elem, "Conversion", io.greenbus.edge.modbus.config.model.Conversion.read, ctx))
    val bitMask = MappingLibrary.optMapField("bitMask", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readString(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val shiftRight = MappingLibrary.optMapField("shiftRight", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (index.isRight && name.isRight && conversionType.isRight && bitMask.isRight && shiftRight.isRight) {
      Right(NumericInput(index.right.get, name.right.get, conversionType.right.get, bitMask.right.get, shiftRight.right.get))
    } else {
      Left(Seq(index.left.toOption, name.left.toOption, conversionType.left.toOption, bitMask.left.toOption, shiftRight.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: NumericInput): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("index"), ValueUInt32(obj.index)),
      (ValueString("name"), ValueString(obj.name)),
      (ValueString("conversionType"), io.greenbus.edge.modbus.config.model.Conversion.write(obj.conversionType)),
      (ValueString("bitMask"), obj.bitMask.map(p => ValueString(p)).getOrElse(ValueNone)),
      (ValueString("shiftRight"), obj.shiftRight.map(p => ValueUInt32(p)).getOrElse(ValueNone))))

    TaggedValue("NumericInput", built)
  }
}
case class NumericInput(index: Int, name: String, conversionType: io.greenbus.edge.modbus.config.model.Conversion, bitMask: Option[String], shiftRight: Option[Int])

object OutputMapping {

  def read(element: Value, ctx: ReaderContext): Either[String, OutputMapping] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type OutputMapping did not recognize value type " + other)
        }
      case other => Left("Type OutputMapping did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, OutputMapping] = {
    val index = MappingLibrary.getMapField("index", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val name = MappingLibrary.getMapField("name", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val commandType = MappingLibrary.getMapField("commandType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("commandType", elem, "CommandType", io.greenbus.edge.modbus.config.model.CommandType.read, ctx))
    val constBooleanValue = MappingLibrary.optMapField("constBooleanValue", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readBool(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val constIntValue = MappingLibrary.optMapField("constIntValue", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readLong(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val bitMaskToUpdate = MappingLibrary.optMapField("bitMaskToUpdate", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readString(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val shiftLeft = MappingLibrary.optMapField("shiftLeft", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val registerCount = MappingLibrary.optMapField("registerCount", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (index.isRight && name.isRight && commandType.isRight && constBooleanValue.isRight && constIntValue.isRight && bitMaskToUpdate.isRight && shiftLeft.isRight && registerCount.isRight) {
      Right(OutputMapping(index.right.get, name.right.get, commandType.right.get, constBooleanValue.right.get, constIntValue.right.get, bitMaskToUpdate.right.get, shiftLeft.right.get, registerCount.right.get))
    } else {
      Left(Seq(index.left.toOption, name.left.toOption, commandType.left.toOption, constBooleanValue.left.toOption, constIntValue.left.toOption, bitMaskToUpdate.left.toOption, shiftLeft.left.toOption, registerCount.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: OutputMapping): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("index"), ValueUInt32(obj.index)),
      (ValueString("name"), ValueString(obj.name)),
      (ValueString("commandType"), io.greenbus.edge.modbus.config.model.CommandType.write(obj.commandType)),
      (ValueString("constBooleanValue"), obj.constBooleanValue.map(p => ValueBool(p)).getOrElse(ValueNone)),
      (ValueString("constIntValue"), obj.constIntValue.map(p => ValueInt64(p)).getOrElse(ValueNone)),
      (ValueString("bitMaskToUpdate"), obj.bitMaskToUpdate.map(p => ValueString(p)).getOrElse(ValueNone)),
      (ValueString("shiftLeft"), obj.shiftLeft.map(p => ValueUInt32(p)).getOrElse(ValueNone)),
      (ValueString("registerCount"), obj.registerCount.map(p => ValueUInt32(p)).getOrElse(ValueNone))))

    TaggedValue("OutputMapping", built)
  }
}
case class OutputMapping(index: Int, name: String, commandType: io.greenbus.edge.modbus.config.model.CommandType, constBooleanValue: Option[Boolean], constIntValue: Option[Long], bitMaskToUpdate: Option[String], shiftLeft: Option[Int], registerCount: Option[Int])

object Poll {

  def read(element: Value, ctx: ReaderContext): Either[String, Poll] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Poll did not recognize value type " + other)
        }
      case other => Left("Type Poll did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Poll] = {
    val dataType = MappingLibrary.getMapField("dataType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("dataType", elem, "DataType", io.greenbus.edge.modbus.config.model.DataType.read, ctx))
    val start = MappingLibrary.getMapField("start", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val count = MappingLibrary.getMapField("count", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val intervalMs = MappingLibrary.getMapField("intervalMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))
    val timeoutMs = MappingLibrary.getMapField("timeoutMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))

    if (dataType.isRight && start.isRight && count.isRight && intervalMs.isRight && timeoutMs.isRight) {
      Right(Poll(dataType.right.get, start.right.get, count.right.get, intervalMs.right.get, timeoutMs.right.get))
    } else {
      Left(Seq(dataType.left.toOption, start.left.toOption, count.left.toOption, intervalMs.left.toOption, timeoutMs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Poll): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("dataType"), io.greenbus.edge.modbus.config.model.DataType.write(obj.dataType)),
      (ValueString("start"), ValueUInt32(obj.start)),
      (ValueString("count"), ValueUInt32(obj.count)),
      (ValueString("intervalMs"), ValueUInt64(obj.intervalMs)),
      (ValueString("timeoutMs"), ValueUInt64(obj.timeoutMs))))

    TaggedValue("Poll", built)
  }
}
case class Poll(dataType: io.greenbus.edge.modbus.config.model.DataType, start: Int, count: Int, intervalMs: Long, timeoutMs: Long)

object ProtocolType {

  case object RTU extends ProtocolType("RTU", 0)
  case object TCPIP extends ProtocolType("TCPIP", 1)

  def read(element: Value, ctx: ReaderContext): Either[String, ProtocolType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type ProtocolType did not recognize value type " + other)
        }
      case other => Left("Type ProtocolType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, ProtocolType] = {
    element.toInt match {
      case 0 => Right(RTU)
      case 1 => Right(TCPIP)
      case other => Left("Enum ProtocolType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, ProtocolType] = {
    element.value match {
      case "RTU" => Right(RTU)
      case "TCPIP" => Right(TCPIP)
      case other => Left("Enum ProtocolType did not recognize string value " + other)
    }
  }
  def write(obj: ProtocolType): TaggedValue = {
    TaggedValue("ProtocolType", ValueUInt32(obj.value))
  }
}
sealed abstract class ProtocolType(val name: String, val value: Int)

object TCPClient {

  def read(element: Value, ctx: ReaderContext): Either[String, TCPClient] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type TCPClient did not recognize value type " + other)
        }
      case other => Left("Type TCPClient did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, TCPClient] = {
    val host = MappingLibrary.getMapField("host", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val port = MappingLibrary.getMapField("port", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val retryMs = MappingLibrary.getMapField("retryMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))

    if (host.isRight && port.isRight && retryMs.isRight) {
      Right(TCPClient(host.right.get, port.right.get, retryMs.right.get))
    } else {
      Left(Seq(host.left.toOption, port.left.toOption, retryMs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: TCPClient): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("host"), ValueString(obj.host)),
      (ValueString("port"), ValueUInt32(obj.port)),
      (ValueString("retryMs"), ValueUInt64(obj.retryMs))))

    TaggedValue("TCPClient", built)
  }
}
case class TCPClient(host: String, port: Int, retryMs: Long)

