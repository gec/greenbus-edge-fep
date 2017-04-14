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

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import io.greenbus.edge.data.mapping.{ RootCtx, SimpleReaderContext }
import io.greenbus.edge.data.xml.{ XmlReader, XmlWriter }
import io.greenbus.edge.data.{ TaggedValue, ValueMap }
import io.greenbus.edge.dnp3.config.model.{ DNP3Gateway, Master }

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

  def buildGateway: DNP3Gateway = {
    DNP3Gateway(buildMaster,
      TCPClient("127.0.0.1", 20000),
      InputModel(
        binaryInputs = IndexSet(Seq(IndexRange(0, 20))),
        analogInputs = IndexSet(Seq(IndexRange(0, 20))),
        counterInputs = IndexSet(Seq(IndexRange(0, 20))),
        binaryOutputs = IndexSet(Seq(IndexRange(0, 20))),
        analogOutputs = IndexSet(Seq(IndexRange(0, 20)))),
      OutputModel(
        binaries = IndexSet(Seq(IndexRange(0, 20))),
        setpoints = IndexSet(Seq(IndexRange(0, 20)))))
  }

  def runGateway(): Unit = {

    val example = buildGateway
    val written = DNP3Gateway.write(example)
    println(written)

    val readEither = written match {
      case t: TaggedValue =>
        t.value match {
          case tt: ValueMap => DNP3Gateway.read(tt, SimpleReaderContext(Vector(RootCtx("DNP3Gateway"))))
          case _ => throw new IllegalArgumentException(s"Written was not a tagged tuple type")
        }
      case _ => throw new IllegalArgumentException(s"Written was not a tagged tuple type")
    }

    readEither match {
      case Left(err) => println("ERROR: " + err)
      case Right(read) =>
        println(example)
        println(read)
        println(example == read)
    }

  }

  def runMaster(): Unit = {

    val example = buildMaster
    val written = Master.write(example)
    println(written)

    val readEither = written match {
      case t: TaggedValue =>
        t.value match {
          case tt: ValueMap => Master.read(tt, SimpleReaderContext(Vector(RootCtx("Master"))))
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

  def main(args: Array[String]): Unit = {
    runGateway()
  }

}

object XmlWriterTester {

  def runGateway(): Unit = {

    val example = Example.buildGateway
    val obj = DNP3Gateway.write(example)

    println(obj)
    val stringOut = new ByteArrayOutputStream()

    XmlWriter.write(obj, stringOut, Some("io.greenbus.edge.dnp3.config"))
    println(stringOut.toString("UTF-8"))
    val array = stringOut.toByteArray

    val in = new ByteArrayInputStream(array)

    val readOpt = XmlReader.read(in, DnpGatewaySchema.gateway)

    val xmlRead = readOpt.get
    println(obj)
    println(xmlRead)

    val theyMatch = obj == xmlRead
    println("match? " + theyMatch)
  }

  def runMaster(): Unit = {

    val example = Example.buildMaster
    val obj = Master.write(example)

    val stringOut = new ByteArrayOutputStream()

    XmlWriter.write(obj, stringOut)
    println(stringOut.toString("UTF-8"))
    val array = stringOut.toByteArray

    val in = new ByteArrayInputStream(array)

    val readOpt = XmlReader.read(in, Schema.master)

    val xmlRead = readOpt.get
    println(obj)
    println(xmlRead)

    val theyMatch = obj == xmlRead
    println("match? " + theyMatch)
  }

  def main(args: Array[String]): Unit = {

    runGateway()
  }
}
