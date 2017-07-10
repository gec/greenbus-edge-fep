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
package io.greenbus.edge.dnp3.config

import io.greenbus.edge.data.schema._
import io.greenbus.edge.fep.model.FrontendSchema

object DnpGatewaySchema {

  val ns = TypeNamespace("io.greenbus.edge.dnp3.config.model", Map("scalaPackage" -> "io.greenbus.edge.dnp3.config.model", "xmlns" -> "io.greenbus.edge.dnp3.config.model"))

  val linkLayer: TExt = {
    TExt(ns, "LinkLayer", TStruct(Vector(
      StructFieldDef("isMaster", TBool, 0, """Whether the link layer identifies itself as a master device. Should always be 'true'."""),
      StructFieldDef("localAddress", TUInt32, 1, """Master (local) DNP3 link layer address."""),
      StructFieldDef("remoteAddress", TUInt32, 2, """Slave (remote) DNP3 link layer address."""),
      StructFieldDef("userConfirmations", TBool, 3, """Whether to use DNP3 link layer confirmations."""),
      StructFieldDef("ackTimeoutMs", TUInt64, 4, """Timeout for waiting for link layer acknowledgements. In milliseconds."""),
      StructFieldDef("numRetries", TUInt32, 5, """Retries for link layer packets before they are considered a failure. Integer value."""))),
      """Configuration parameters for the DNP3 link layer.""")
  }
  val appLayer: TExt = {
    TExt(ns, "AppLayer", TStruct(Vector(
      StructFieldDef("timeoutMs", TUInt64, 0, """Timeout in milliseconds for application layer transactions."""),
      StructFieldDef("maxFragSize", TUInt32, 1, """Maximum fragment size in bytes for outgoing application layer fragments."""),
      StructFieldDef("numRetries", TUInt32, 2, """Number of DNP3 application layer retries before a transaction is considered a failure."""))),
      """Configuration parameters for the DNP3 application layer.""")
  }
  val stackConfig: TExt = {
    TExt(ns, "StackConfig", TStruct(Vector(
      StructFieldDef("linkLayer", linkLayer, 0, """Configuration parameters for the DNP3 link layer."""),
      StructFieldDef("appLayer", appLayer, 1, """Configuration parameters for the DNP3 application layer."""))),
      """Configuration parameters for the layers of the DNP3 stack.""")
  }
  val masterSettings: TExt = {
    TExt(ns, "MasterSettings", TStruct(Vector(
      StructFieldDef("allowTimeSync", TBool, 0, """If true, the master will do time syncs when it sees the time IIN bit from the slave."""),
      StructFieldDef("taskRetryMs", TUInt64, 1, """Period in milliseconds between retries of failed DNP3 requests."""),
      StructFieldDef("integrityPeriodMs", TUInt64, 2, """Period in milliseconds between integrity (class 0) polls."""))),
      """Configuration parameters for the DNP3 master user layer.""")
  }
  val scan: TExt = {
    TExt(ns, "Scan", TStruct(Vector(
      StructFieldDef("enableClass1", TBool, 0, """Whether the scan polls for class 1 events."""),
      StructFieldDef("enableClass2", TBool, 1, """Whether the scan polls for class 2 events."""),
      StructFieldDef("enableClass3", TBool, 2, """Whether the scan polls for class 3 events."""),
      StructFieldDef("periodMs", TUInt64, 3, """Time in milliseconds between scans."""))),
      """Configuration of a DNP3 exception scan. Multiple DNP3 classes can be polled at once.""")
  }
  val unsol: TExt = {
    TExt(ns, "Unsol", TStruct(Vector(
      StructFieldDef("doTask", TBool, 0, """If true, the master will enable/disable unsol on startup."""),
      StructFieldDef("enable", TBool, 1, """If the task is enabled, this will determine whether the master sets the unsol enabled bit for the slave."""),
      StructFieldDef("enableClass1", TBool, 2, """Whether unsolicited updates are enabled for class 1 objects."""),
      StructFieldDef("enableClass2", TBool, 3, """Whether unsolicited updates are enabled for class 2 objects."""),
      StructFieldDef("enableClass3", TBool, 4, """Whether unsolicited updates are enabled for class 3 objects."""))),
      """Configuration of DNP3 unsolicited data.""")
  }

  val master: TExt = {
    TExt(ns, "Master", TStruct(Vector(
      StructFieldDef("stack", stackConfig, 0, """Configuration parameters for the layers of the DNP3 stack."""),
      StructFieldDef("masterSettings", masterSettings, 1, """Configuration parameters for the DNP3 master user layer."""),
      StructFieldDef("scanList", TList(scan), 2, """Configures as list of exception scans to pull data from the DNP3 slave. Can be left empty to not use exception scans."""),
      StructFieldDef("unsol", unsol, 3))),
      """Configuration parameters for the behavior of the DNP3 master.""")
  }

  val tcpClient: TExt = {
    TExt(ns, "TCPClient", TStruct(Vector(
      StructFieldDef("host", TString, 0, """Hostname/IP address."""),
      StructFieldDef("port", TUInt32, 1, """IP port."""),
      StructFieldDef("retryMs", TUInt64, 2, """Retry time in milliseconds."""))),
      """Parameters for TCP client connection to DNP3 device.""")
  }
  val selectIndex: TExt = {
    TExt(ns, "IndexSelect", TUInt32)
  }
  val selectRange: TExt = {
    TExt(ns, "IndexRange", TStruct(Vector(
      StructFieldDef("start", TUInt32, 0, """Start of index range."""),
      StructFieldDef("count", TUInt32, 1, """Number of points after start of range."""))),
      """Specifies a set of indexes with a start and offset count.""")
  }
  val indexSet: TExt = {
    //TExt("IndexSet", TList(TUnion(Set(selectIndex, selectRange))))
    TExt(ns, "IndexSet", TList(selectRange))
  }

  val inputModel: TExt = {
    TExt(ns, "InputModel", TStruct(Vector(
      StructFieldDef("binaryInputs", indexSet, 0, """List of index ranges of DNP3 binary inputs to be mapped."""),
      StructFieldDef("analogInputs", indexSet, 1, """List of index ranges of DNP3 analog inputs to be mapped."""),
      StructFieldDef("counterInputs", indexSet, 2, """List of index ranges of DNP3 counter inputs to be mapped."""),
      StructFieldDef("binaryOutputs", indexSet, 3, """List of index ranges of DNP3 binary outputs to be mapped."""),
      StructFieldDef("analogOutputs", indexSet, 4, """List of index ranges of DNP3 analog outputs to be mapped."""))),
      """Ranges of DNP3 data objects made available for endpoint mapping.""")
  }

  val controlTypeEnum: TExt = {
    TExt(ns, "ControlType", TEnum(Seq(
      EnumDef("PULSE", 0, """Pulse control type."""),
      EnumDef("PULSE_CLOSE", 1, """Pulse/close control type."""),
      EnumDef("PULSE_TRIP", 2, """Pulse/trip control type."""),
      EnumDef("LATCH_ON", 3, """Latch on control type."""),
      EnumDef("LATCH_OFF", 4, """Latch off control type."""))),
      """Options that define how the DNP3 control is issued.""")
  }

  val functionTypeEnum: TExt = {
    TExt(ns, "FunctionType", TEnum(Seq(
      EnumDef("SelectBeforeOperate", 0, """Select before operate DNP3 function."""),
      EnumDef("DirectOperate", 1, """Direct operate DNP3 function."""))),
      """DNP3 function type for outputs.""")
  }

  val controlOptions: TExt = {
    TExt(ns, "ControlOptions", TStruct(Vector(
      StructFieldDef("controlType", controlTypeEnum, 0, """Options that define how the DNP3 control is issued."""),
      StructFieldDef("onTime", TOption(TUInt32), 1, """Time, in milliseconds, output is active."""),
      StructFieldDef("offTime", TOption(TUInt32), 2, """Time, in milliseconds, output is non-active."""),
      StructFieldDef("count", TOption(TUInt32), 3, """Number of times outstation should execute the operation. 0-255 are valid. Defaults to 1."""))),
      "Options that define how the DNP3 control is issued." + "\n\n" +
        "Count must be 1 or greater to have an effect. 'onTime' / 'offTime' will be ignored if they do not apply to the control type.")
  }

  val controlItem: TExt = {
    TExt(ns, "Control", TStruct(Vector(
      StructFieldDef("name", TString, 0, """Named used to correlate with endpoint model."""),
      StructFieldDef("index", TUInt32, 1, """DNP3 index of control."""),
      StructFieldDef("function", functionTypeEnum, 2, """Function type of control."""),
      StructFieldDef("controlOptions", controlOptions, 3, """DNP3 control options."""))),
      """Configuration of DNP3 control available for endpoint mapping.""")
  }
  val setpointItem: TExt = {
    TExt(ns, "Setpoint", TStruct(Vector(
      StructFieldDef("name", TString, 0, """Named used to correlate with endpoint model."""),
      StructFieldDef("index", TUInt32, 1, """DNP3 index of setpoint."""),
      StructFieldDef("function", functionTypeEnum, 2, """Function type of control."""))),
      """Configuration of DNP3 setpoint available for endpoint mapping.""")
  }

  val outputModel: TExt = {
    TExt(ns, "OutputModel", TStruct(Vector(
      StructFieldDef("controls", TList(controlItem), 0, """Configuration of DNP3 control available for endpoint mapping."""),
      StructFieldDef("setpoints", TList(setpointItem), 1, """Configuration of DNP3 setpoint available for endpoint mapping."""))),
      """Configuration of DNP3 controls and setpoints for endpoint mapping.""")
  }

  val gateway: TExt = {
    TExt(ns, "DNP3Gateway", TStruct(Vector(
      StructFieldDef("master", master, 0, """Configuration parameters for the behavior of the DNP3 master."""),
      StructFieldDef("client", tcpClient, 1, """Parameters for TCP client connection to DNP3 device."""),
      StructFieldDef("inputModel", inputModel, 2, """Ranges of DNP3 data objects made available for endpoint mapping."""),
      StructFieldDef("outputModel", outputModel, 3, """Configuration of DNP3 controls and setpoints for endpoint mapping."""),
      StructFieldDef("endpoint", FrontendSchema.frontendConfiguration, 4, """Endpoint mapping configuration."""))),
      """Configuration of a DNP3 master mapped to an endpoint.""")
  }

  def all = Seq(
    linkLayer, appLayer, stackConfig, masterSettings, scan, unsol, master,
    tcpClient,
    /*selectIndex,*/
    selectRange,
    indexSet,
    inputModel,
    controlTypeEnum,
    functionTypeEnum,
    controlOptions,
    controlItem,
    setpointItem,
    outputModel,
    gateway)
}