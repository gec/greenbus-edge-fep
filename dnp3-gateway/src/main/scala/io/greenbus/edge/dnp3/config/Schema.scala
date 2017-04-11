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

import java.io._

import com.google.common.io.Files
import io.greenbus.edge.dnp3.config.model.Master
import io.greenbus.edge.tag._
import io.greenbus.edge.tag.xml.{ Writer, XmlReader, XmlReader2 }

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
    TExt("InputModel", TStruct(Vector(
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

}

object Example {
  import io.greenbus.edge.dnp3.config.model._

  def buildMaster: Master = {
    Master(
      StackConfig(
        LinkLayer(isMaster = true, localAddress = 1, remoteAddress = 100, userConfirmations = false, ackTimeoutMs = 1000, numRetries = 3),
        AppLayer(timeoutMs = 5000, maxFragSize = 2048, numRetries = 0)),
      MasterSettings(allowTimeSync = true, integrityPeriodMs = 300000, taskRetryMs = 5000),
      Seq(Scan(
        enableClass1 = true,
        enableClass2 = true,
        enableClass3 = true,
        periodMs = 2000)),
      Unsol(doTask = true, enable = true, enableClass1 = true, enableClass2 = true, enableClass3 = true))
  }

  def main(args: Array[String]): Unit = {

    val example = buildMaster
    val written = Master.write(example)
    println(written)

    val readEither = written match {
      case t: TaggedValue =>
        t.value match {
          case tt: VMap => Master.read(tt, SimpleReaderContext(Vector(RootCtx("Master"))))
          case _ => throw new IllegalArgumentException(s"Written was not a tagged tuple type")
        }
      case _ => throw new IllegalArgumentException(s"Written was not a tagged tuple type")
    }

    readEither match {
      case Left(err) => println("ERROR: " + err)
      case Right(master) =>
        println(master)
        println(example == master)
    }

  }

}

object XmlWriterTester {

  def main(args: Array[String]): Unit = {

    //Writer.write(VBool(false), System.out)

    val example = Example.buildMaster
    val obj = Master.write(example)

    val stringOut = new ByteArrayOutputStream()

    Writer.write(obj, stringOut)
    println(stringOut.toString("UTF-8"))
    val array = stringOut.toByteArray

    val in = new ByteArrayInputStream(array)

    XmlReader2.read(in, Schema.master)
    /*val xmlRead = XmlReader.read(in, Schema.master)

    //println(stringOut.toString("UTF-8"))
    println(obj)
    println(xmlRead)

    val theyMatch = obj == xmlRead
    println("match? " + theyMatch)*/
  }
}

object Builder {

  def main(args: Array[String]): Unit = {
    //val all = Gen.collectTypes(DnpGatewaySchema.gateway, Map())
    val all = Gen.collectObjDefs(DnpGatewaySchema.gateway, Map())

    println(all)

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