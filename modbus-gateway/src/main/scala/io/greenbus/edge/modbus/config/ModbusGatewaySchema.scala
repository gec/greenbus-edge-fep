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
      StructFieldDef("host", TString, 0, """IP address of device."""),
      StructFieldDef("port", TUInt32, 1, """IP port of device"""),
      StructFieldDef("retryMs", TUInt64, 2, """Retry time, in milliseconds."""))),
      """Specifies TCP address of Modbus device.""")
  }

  val dataTypeEnum: TExt = {
    TExt(ns, "DataType", TEnum(Seq(
      EnumDef("DiscreteInput", 0, "Modbus discrete input data type."),
      EnumDef("CoilStatus", 1, "Modbus coil status data type."),
      EnumDef("InputRegister", 2, "Modbus input register data type."),
      EnumDef("HoldingRegister", 3, "Modbus holding register data type."))),
      """Specifieds the Modbus data type.""")
  }

  val commandTypeEnum: TExt = {
    TExt(ns, "CommandType", TEnum(Seq(
      EnumDef("Coil", 0),
      EnumDef("Register", 1),
      EnumDef("MaskRegister", 2),
      EnumDef("MultipleRegisters", 3))),
      """Specifieds the Modbus command type.""")
  }

  val conversionEnum: TExt = {
    TExt(ns, "Conversion", TEnum(Seq(
      EnumDef("SInt16", 0, """A single register is interpreted as a signed integer value."""),
      EnumDef("UInt16", 1, """A single register is interpreted as an unsigned integer value."""),
      EnumDef("SInt32LE", 2, """Two registers are combined with the least significant in the lower index, and interpreted as a signed integer value."""),
      EnumDef("UInt32LE", 3, """Two registers are combined with the least significant in the lower index, and interpreted as an unsigned integer value."""),
      EnumDef("SInt32BE", 4, """Two registers are combined with the most significant in the lower index, and interpreted as a signed integer value."""),
      EnumDef("UInt32BE", 5, """Two registers are combined with the most significant in the lower index, and interpreted as an unsigned integer value."""),
      EnumDef("Float32LE", 6, """Two registers are combined with the least significant in the lower index, and interpreted as a 32-bit floating-point value."""),
      EnumDef("Float32BE", 7, """Two registers are combined with the most significant in the lower index, and interpreted as a 32-bit floating-point value."""),
      EnumDef("Float64LE", 8, """Four registers are combined in order from least significant to most, and interpreted as a 64-bit floating-point value."""),
      EnumDef("Float64BE", 9, """Four registers are combined in order from most significant to least, and interpreted as a 64-bit floating-point value."""))),
      "Indexes into the input registers and holding registers refer to 16-bit (2 byte) blocks of data. Depending " +
        "on the device description, these values may need to be interpreted as signed or unsigned integers or floating " +
        "points of varying lengths. Mapping entries with conversion types greater than 16-bits are assumed to extend " +
        "into the next highest registers (i.e. a UInt32BE at index 2 is stored in 2 and 3, the next distinct value is " +
        "then at index 4)." + "\n\n" +
        "The endian-ness of conversions describes in what order multiple registers are combined. All 16-bit blocks are " +
        "assumed to be big-endian (the first, lower, index refers to the most significant byte of the register).")
  }

  val protocolTypeEnum: TExt = {
    TExt(ns, "ProtocolType", TEnum(Seq(
      EnumDef("RTU", 0, "Tunnels the Modbus serial protocol over TCP/IP."),
      EnumDef("TCPIP", 1, "Uses the Modbus TCP/IP headers."))),
      """Specifies the framing protocol for the Modbus connection.""")
  }

  val numericInput: TExt = {
    TExt(ns, "NumericInput", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0, "Modbus index."),
      StructFieldDef("name", TString, 1, """Named used to correlate with endpoint model."""),
      StructFieldDef("conversionType", conversionEnum, 2, "Specifies how one or more 16-bit registers at this index should be converted to values."),
      StructFieldDef("bitMask", TOption(TString), 3, "Specifies a 16-bit mask to be logically 'and'ed to the register value."),
      StructFieldDef("shiftRight", TOption(TUInt32), 4, "When 'BitMask' is used, the resulting value then is transformed by the binary shift right operation this many times."))),
      """Mapping of Modbus analog (integer registers) points.""")
  }

  val booleanInput: TExt = {
    TExt(ns, "BooleanInput", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0, "Modbus index."),
      StructFieldDef("name", TString, 1, """Named used to correlate with endpoint model."""))),
      """Mapping of Modbus boolean points.""")
  }

  val poll: TExt = {
    TExt(ns, "Poll", TStruct(Vector(
      StructFieldDef("dataType", dataTypeEnum, 0, """Specifies the Modbus data type."""),
      StructFieldDef("start", TUInt32, 1, "First index in the range to be read"),
      StructFieldDef("count", TUInt32, 2, "Number of indexes, from the start index, to be read"),
      StructFieldDef("intervalMs", TUInt64, 3, "Time in milliseconds between polls."),
      StructFieldDef("timeoutMs", TUInt64, 4, "Time in milliseconds to wait for a response until a request is considered a failure."))),
      """Specifies a poll to retrieve data.""")
  }

  val commandMapping: TExt = {
    TExt(ns, "OutputMapping", TStruct(Vector(
      StructFieldDef("index", TUInt32, 0, "Modbus index."),
      StructFieldDef("name", TString, 1, """Named used to correlate with endpoint model."""),
      StructFieldDef("commandType", commandTypeEnum, 2),
      StructFieldDef("constBooleanValue", TOption(TBool), 3, "Specifies a constant value for coil writes."),
      StructFieldDef("constIntValue", TOption(TInt64), 4, "Specifies a constant value for register writes."),
      StructFieldDef("bitMaskToUpdate", TOption(TString), 4, "Specifies a 16-bit mask that defines a subset of the register to write. " +
        "The resulting write will be the bits of the setpoint in the mask combined with the bits of the previous holding register value " +
        "not in the mask. A read of the holding register is performed to retrieve the previous value."),
      StructFieldDef("shiftLeft", TOption(TUInt32), 4, "If using a bitmask, the setpoint value is first transformed by the binary shift left operation " +
        "this many times before being combined with the previous value."),
      StructFieldDef("registerCount", TOption(TUInt32), 4, "Number of registers to write when using multi write."))),
      """Configuration of Modbus outputs.""")
  }

  val master: TExt = {
    TExt(ns, "ModbusMaster", TStruct(Vector(
      StructFieldDef("tcpClient", tcpClient, 0, """Specifies TCP address of Modbus device."""),
      StructFieldDef("protocol", protocolTypeEnum, 1, """Specifies the framing protocol for the Modbus connection."""),
      StructFieldDef("address", TUInt32, 2, "Modbus address of slave."),
      StructFieldDef("polls", TList(poll), 3, "List of polls to retrieve data."),
      StructFieldDef("discreteInputs", TList(booleanInput), 4, "Configuration of Modbus discrete inputs."),
      StructFieldDef("coilStatuses", TList(booleanInput), 5, "Configuration of Modbus coil statuses."),
      StructFieldDef("inputRegisters", TList(numericInput), 6, "Configuration of Modbus input registers."),
      StructFieldDef("holdingRegisters", TList(numericInput), 7, "Configuration of Modbus holding registers."),
      StructFieldDef("commandMappings", TList(commandMapping), 8, "Configuration of Modbus command mappings."))))
  }

  val gateway: TExt = {
    TExt(ns, "ModbusGateway", TStruct(Vector(
      StructFieldDef("modbus", master, 0, "Configuration for Modbus master protocol."),
      StructFieldDef("endpoint", FrontendSchema.frontendConfiguration, 2, """Endpoint mapping configuration."""))),
      """Configuration of a Modbus master mapped to an endpoint.""")
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
