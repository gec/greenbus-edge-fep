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
package io.greenbus.edge.modbus.config

import io.greenbus.edge.data.schema._
import io.greenbus.edge.fep.model.FrontendSchema

object ModbusGatewaySchema {

  val ns = TypeNamespace("io.greenbus.edge.modbus.config.model",
    Map("scalaPackage" -> "io.greenbus.edge.modbus.config.model",
      "xmlns" -> "io.greenbus.edge.modbus.config.model"))

  val tcpClient: TExt = {
    TExt(ns, "TCPClient", TStruct(Vector(
      StructFieldDef("host", TString, 0),
      StructFieldDef("port", TUInt32, 1),
      StructFieldDef("retryMs", TUInt64, 2))))
  }

  val dataTypeEnum: TExt = {
    TExt(ns, "DataType", TEnum(Seq(
      EnumDef("DiscreteInput", 0),
      EnumDef("CoilStatus", 1),
      EnumDef("InputRegister", 2),
      EnumDef("HoldingRegister", 3))))
  }

  val commandTypeEnum: TExt = {
    TExt(ns, "CommandType", TEnum(Seq(
      EnumDef("Coil", 0),
      EnumDef("Register", 1),
      EnumDef("MaskRegister", 2),
      EnumDef("MultipleRegisters", 3))))
  }

  val conversionEnum: TExt = {
    TExt(ns, "Conversion", TEnum(Seq(
      EnumDef("SInt16", 0),
      EnumDef("UInt16", 1),
      EnumDef("SInt32LE", 2),
      EnumDef("UInt32LE", 3),
      EnumDef("SInt32BE", 4),
      EnumDef("UInt32BE", 5),
      EnumDef("Float32LE", 6),
      EnumDef("Float32BE", 7),
      EnumDef("Float64LE", 8),
      EnumDef("Float64BE", 9))))
  }

  val protocolTypeEnum: TExt = {
    TExt(ns, "ProtocolType", TEnum(Seq(
      EnumDef("RTU", 0),
      EnumDef("TCPIP", 1))))
  }

  val numericInput: TExt = {
    TExt(ns, "NumericInput", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0),
      StructFieldDef("name", TString, 1),
      StructFieldDef("conversionType", conversionEnum, 2),
      StructFieldDef("bitMask", TOption(TString), 3),
      StructFieldDef("shiftRight", TOption(TUInt32), 4))))
  }

  val booleanInput: TExt = {
    TExt(ns, "BooleanInput", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0),
      StructFieldDef("name", TString, 1))))
  }

  val poll: TExt = {
    TExt(ns, "Poll", TStruct(Vector(
      StructFieldDef("dataType", dataTypeEnum, 0),
      StructFieldDef("start", TUInt32, 1),
      StructFieldDef("count", TUInt32, 2),
      StructFieldDef("intervalMs", TUInt64, 3),
      StructFieldDef("timeoutMs", TUInt64, 4))))
  }

  val commandMapping: TExt = {
    TExt(ns, "OutputMapping", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0),
      StructFieldDef("name", TString, 1),
      StructFieldDef("commandType", commandTypeEnum, 2),
      StructFieldDef("constBooleanValue", TOption(TBool), 3),
      StructFieldDef("constIntValue", TOption(TInt64), 4),
      StructFieldDef("bitMaskToUpdate", TOption(TString), 4),
      StructFieldDef("shiftLeft", TOption(TUInt32), 4),
      StructFieldDef("registerCount", TOption(TUInt32), 4))))
  }

  val master: TExt = {
    TExt(ns, "Master", TStruct(Vector(
      StructFieldDef("tcpClient", tcpClient, 0),
      StructFieldDef("protocol", protocolTypeEnum, 1),
      StructFieldDef("address", TUInt32, 2),
      StructFieldDef("polls", TList(poll), 3),
      StructFieldDef("discreteInputs", TList(booleanInput), 4),
      StructFieldDef("coilStatuses", TList(booleanInput), 5),
      StructFieldDef("inputRegisters", TList(numericInput), 6),
      StructFieldDef("holdingRegisters", TList(booleanInput), 7),
      StructFieldDef("commandMappings", TList(commandMapping), 8))))
  }

  val gateway: TExt = {
    TExt(ns, "ModbusGateway", TStruct(Vector(
      StructFieldDef("modbus", master, 0),
      StructFieldDef("endpoint", FrontendSchema.frontendConfiguration, 2))))
  }

  val all = Seq(
    dataTypeEnum,
    commandTypeEnum,
    conversionEnum,
    tcpClient,
    protocolTypeEnum,
    poll,
    booleanInput,
    numericInput,
    commandMapping,
    master,
    gateway)
}
