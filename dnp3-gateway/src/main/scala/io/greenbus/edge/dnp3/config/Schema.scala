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

object Schema {

  val linkLayer: TExt = {
    TExt("LinkLayer", TStruct(Vector(
      StructFieldDef("isMaster", TBool, 0),
      StructFieldDef("localAddress", TUInt32, 1),
      StructFieldDef("remoteAddress", TUInt32, 2),
      StructFieldDef("userConfirmations", TBool, 3),
      StructFieldDef("ackTimeoutMs", TUInt64, 4),
      StructFieldDef("numRetries", TUInt32, 5))))
  }
  val appLayer: TExt = {
    TExt("AppLayer", TStruct(Vector(
      StructFieldDef("timeoutMs", TUInt64, 0),
      StructFieldDef("maxFragSize", TUInt32, 1),
      StructFieldDef("numRetries", TUInt32, 2))))
  }
  val stackConfig: TExt = {
    TExt("StackConfig", TStruct(Vector(
      StructFieldDef("linkLayer", linkLayer, 0),
      StructFieldDef("appLayer", appLayer, 1))))
  }
  val masterSettings: TExt = {
    TExt("MasterSettings", TStruct(Vector(
      StructFieldDef("allowTimeSync", TBool, 0),
      StructFieldDef("taskRetryMs", TUInt64, 1),
      StructFieldDef("integrityPeriodMs", TUInt64, 2))))
  }
  val scan: TExt = {
    TExt("Scan", TStruct(Vector(
      StructFieldDef("enableClass1", TBool, 0),
      StructFieldDef("enableClass2", TBool, 1),
      StructFieldDef("enableClass3", TBool, 2),
      StructFieldDef("periodMs", TUInt64, 3))))
  }
  val unsol: TExt = {
    TExt("Unsol", TStruct(Vector(
      StructFieldDef("doTask", TBool, 0),
      StructFieldDef("enable", TBool, 1),
      StructFieldDef("enableClass1", TBool, 2),
      StructFieldDef("enableClass2", TBool, 3),
      StructFieldDef("enableClass3", TBool, 4))))
  }

  val master: TExt = {
    TExt("Master", TStruct(Vector(
      StructFieldDef("stack", stackConfig, 0),
      StructFieldDef("masterSettings", masterSettings, 1),
      StructFieldDef("scanList", TList(scan), 2),
      StructFieldDef("unsol", unsol, 3))))
  }

  def all = Seq(linkLayer, appLayer, stackConfig, masterSettings, scan, unsol, master)
}

object DnpGatewaySchema {

  val tcpClient: TExt = {
    TExt("TCPClient", TStruct(Vector(
      StructFieldDef("host", TString, 0),
      StructFieldDef("port", TUInt32, 1))))
  }
  val selectIndex: TExt = {
    TExt("IndexSelect", TUInt32)
  }
  val selectRange: TExt = {
    TExt("IndexRange", TStruct(Vector(
      StructFieldDef("start", TUInt32, 0),
      StructFieldDef("count", TUInt32, 1))))
  }
  val indexSet: TExt = {
    //TExt("IndexSet", TList(TUnion(Set(selectIndex, selectRange))))
    TExt("IndexSet", TList(selectRange))
  }

  val inputModel: TExt = {
    TExt("InputModel", TStruct(Vector(
      StructFieldDef("binaryInputs", indexSet, 0),
      StructFieldDef("analogInputs", indexSet, 1),
      StructFieldDef("counterInputs", indexSet, 2),
      StructFieldDef("binaryOutputs", indexSet, 3),
      StructFieldDef("analogOutputs", indexSet, 4))))
  }

  val outputModel: TExt = {
    TExt("OutputModel", TStruct(Vector(
      StructFieldDef("binaries", indexSet, 0),
      StructFieldDef("setpoints", indexSet, 1))))
  }

  val gateway: TExt = {
    TExt("DNP3Gateway", TStruct(Vector(
      StructFieldDef("master", Schema.master, 0),
      StructFieldDef("client", tcpClient, 1),
      StructFieldDef("inputModel", inputModel, 2),
      StructFieldDef("outputModel", outputModel, 3))))
  }

  def all = Seq(tcpClient, selectIndex, selectRange, indexSet, inputModel, outputModel, gateway)
}

