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

import java.io.{ File, FileOutputStream, PrintWriter }

import com.google.common.io.Files
import io.greenbus.edge.tag._

object Schema {

  val linkLayer: VTExtType = {
    VTExtType("LinkLayer", VTTuple(Vector(
      FieldDef("isMaster", VTBool),
      FieldDef("localAddress", VTUInt32),
      FieldDef("remoteAddress", VTUInt32),
      FieldDef("userConfirmations", VTBool),
      FieldDef("ackTimeoutMs", VTUInt64),
      FieldDef("numRetries", VTUInt32))))
  }
  val appLayer: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      FieldDef("timeoutMs", VTUInt64),
      FieldDef("maxFragSize", VTUInt32),
      FieldDef("numRetries", VTUInt32))))
  }
  val stackConfig: VTExtType = {
    VTExtType("StackConfig", VTTuple(Vector(
      FieldDef("linkLayer", linkLayer),
      FieldDef("appLayer", appLayer))))
  }
  val masterSettings: VTExtType = {
    VTExtType("MasterSettings", VTTuple(Vector(
      FieldDef("allowTimeSync", VTBool),
      FieldDef("taskRetryMs", VTUInt64),
      FieldDef("integrityPeriodMs", VTUInt64))))
  }
  val scan: VTExtType = {
    VTExtType("Scan", VTTuple(Vector(
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool),
      FieldDef("periodMs", VTUInt64))))
  }
  val unsol: VTExtType = {
    VTExtType("Unsol", VTTuple(Vector(
      FieldDef("doTask", VTBool),
      FieldDef("enable", VTBool),
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool))))
  }

  val master: VTExtType = {
    VTExtType("Master", VTTuple(Vector(
      FieldDef("stack", stackConfig),
      FieldDef("masterSettings", masterSettings),
      FieldDef("scanList", VTList(scan)),
      FieldDef("unsol", unsol))))
  }

}

object Builder {

  def main(args: Array[String]): Unit = {
    val all = Gen.collectTypes(Schema.master, Map())

    val f = new File("dnp3-gateway/src/main/scala/io/greenbus/edge/dnp3/config/model/Model.scala")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    //val fw = new PrintWriter("dnp3-gateway/fakesrc/testfile.scala" /*new FileOutputStream(new File("testdir/testfile.scala"))*/ )
    val fw = new PrintWriter(new FileOutputStream(f))
    Gen.output("io.greenbus.edge.dnp3.config.model", all, fw)
    fw.flush()
  }

}