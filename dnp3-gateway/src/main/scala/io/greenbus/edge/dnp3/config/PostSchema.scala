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
import io.greenbus.edge.data.mapping.{ RootCtx, SimpleReaderContext }
import io.greenbus.edge.data.xml._
import io.greenbus.edge.dnp3.EdgeDNP3Gateway
import io.greenbus.edge.dnp3.config.model.DNP3Gateway
import io.greenbus.edge.fep.model.FrontendSchema

object XmlWriterTester {

  def main(args: Array[String]): Unit = {
    runGateway()
  }

  def runGateway(): Unit = {

    val example = EdgeDNP3Gateway.buildGateway
    val obj = DNP3Gateway.write(example)

    println(obj)

    val filename = "testschemas/dnp.xml"

    val f = new File(filename)
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    val stringOut = new FileOutputStream(f)
    //XmlWriter.write(obj, stringOut, Some("io.greenbus.edge.fep.dnp3.config.model"))

    val xmlNs = XmlNamespaceInfo(DnpGatewaySchema.gateway.ns.name,
      Map(
        (DnpGatewaySchema.ns.name, XmlNsDecl("dnp3", DnpGatewaySchema.ns.name)),
        (FrontendSchema.ns.name, XmlNsDecl("fep", FrontendSchema.ns.name))))

    SchemaGuidedXmlWriter.write(obj, DnpGatewaySchema.gateway, stringOut, xmlNs)
    stringOut.close()

    val stringIn = new FileInputStream(f)

    val readOpt = XmlReader.read(stringIn, DnpGatewaySchema.gateway)

    val xmlRead = readOpt.get
    println(obj)
    println(xmlRead)

    val theyMatch = obj == xmlRead
    println("match? " + theyMatch)

    val result = DNP3Gateway.read(xmlRead, SimpleReaderContext(Vector(RootCtx("DNP3Gateway"))))

    println(result.right.get)
    println(example)
    println(result.right.get == example)
  }
}

