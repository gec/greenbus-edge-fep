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
import io.greenbus.edge.tag.xml.{ Writer, XmlReader }

object Schema {

  val linkLayer: VTExtType = {
    VTExtType("LinkLayer", VTTuple(Vector(
      VTField("isMaster", VTBool),
      VTField("localAddress", VTUInt32),
      VTField("remoteAddress", VTUInt32),
      VTField("userConfirmations", VTBool),
      VTField("ackTimeoutMs", VTUInt64),
      VTField("numRetries", VTUInt32))))
  }
  val appLayer: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      VTField("timeoutMs", VTUInt64),
      VTField("maxFragSize", VTUInt32),
      VTField("numRetries", VTUInt32))))
  }
  val stackConfig: VTExtType = {
    VTExtType("StackConfig", VTTuple(Vector(
      VTField("linkLayer", linkLayer),
      VTField("appLayer", appLayer))))
  }
  val masterSettings: VTExtType = {
    VTExtType("MasterSettings", VTTuple(Vector(
      VTField("allowTimeSync", VTBool),
      VTField("taskRetryMs", VTUInt64),
      VTField("integrityPeriodMs", VTUInt64))))
  }
  val scan: VTExtType = {
    VTExtType("Scan", VTTuple(Vector(
      VTField("enableClass1", VTBool),
      VTField("enableClass2", VTBool),
      VTField("enableClass3", VTBool),
      VTField("periodMs", VTUInt64))))
  }
  val unsol: VTExtType = {
    VTExtType("Unsol", VTTuple(Vector(
      VTField("doTask", VTBool),
      VTField("enable", VTBool),
      VTField("enableClass1", VTBool),
      VTField("enableClass2", VTBool),
      VTField("enableClass3", VTBool))))
  }

  val master: VTExtType = {
    VTExtType("Master", VTTuple(Vector(
      VTField("stack", stackConfig),
      VTField("masterSettings", masterSettings),
      VTField("scanList", VTList(scan)),
      VTField("unsol", unsol))))
  }
}

object DnpGatewaySchema {

  val tcpClient: VTExtType = {
    VTExtType("TCPClient", VTTuple(Vector(
      VTField("host", VTString),
      VTField("port", VTUInt32))))
  }
  val selectIndex: VTExtType = {
    VTExtType("IndexSelect", VTUInt32)
  }
  val selectRange: VTExtType = {
    VTExtType("IndexRange", VTTuple(Vector(
      VTField("start", VTUInt32),
      VTField("count", VTUInt32))))
  }
  val indexSet: VTExtType = {
    VTExtType("IndexSet", VTList(VTUnion(Set(selectIndex, selectRange))))
  }

  val inputModel: VTExtType = {
    VTExtType("InputModel", VTTuple(Vector(
      VTField("binaryInputs", indexSet),
      VTField("analogInputs", indexSet),
      VTField("counterInputs", indexSet),
      VTField("binaryOutputs", indexSet),
      VTField("analogOutputs", indexSet))))
  }

  val outputModel: VTExtType = {
    VTExtType("InputModel", VTTuple(Vector(
      VTField("binaries", indexSet),
      VTField("setpoints", indexSet))))
  }

  val gateway: VTExtType = {
    VTExtType("DNP3Gateway", VTTuple(Vector(
      VTField("master", Schema.master),
      VTField("client", tcpClient),
      VTField("inputModel", inputModel),
      VTField("outputModel", outputModel))))
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
          case tt: VTuple => Master.read(tt, SimpleReaderContext(Vector(RootCtx("Master"))))
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
    val array = stringOut.toByteArray

    val in = new ByteArrayInputStream(array)
    val xmlRead = XmlReader.read(in, Schema.master)

    println(stringOut.toString("UTF-8"))
    println(obj)
    println(xmlRead)

    val theyMatch = obj == xmlRead
    println("match? " + theyMatch)
  }
}

object Builder {

  def main(args: Array[String]): Unit = {
    val all = Gen.collectTypes(DnpGatewaySchema.gateway, Map())

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