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

import java.io.{ File, FileOutputStream, PrintWriter }

import com.google.common.io.Files
import io.greenbus.edge.data.codegen.ScalaGen
import io.greenbus.edge.data.xml.{ SchemaWriter, XmlNamespaceInfo, XmlNsDecl }
import io.greenbus.edge.fep.model.FrontendSchema

/*object ModbusXmlSchemaWriter {

  def main(args: Array[String]): Unit = {

    val f = new File("testschemas/dnp.xsd")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    val xmlNs = XmlNamespaceInfo(ModbusGatewaySchema.gateway.ns.name,
      Map(
        (ModbusGatewaySchema.ns.name, XmlNsDecl("modbus", ModbusGatewaySchema.ns.name)),
        (FrontendSchema.ns.name, XmlNsDecl("fep", FrontendSchema.ns.name))))

    SchemaWriter.write(DnpGatewaySchema.all, Seq(DnpGatewaySchema.gateway), xmlNs, new FileOutputStream(f))
  }
}*/

object ModbusScalaWriter {

  def main(args: Array[String]): Unit = {
    val all = ScalaGen.collectObjDefs(ModbusGatewaySchema.ns.name, ModbusGatewaySchema.gateway, Map())

    println(all.map(_._1).mkString("\n"))

    val f = new File("modbus-gateway/src/main/scala/io/greenbus/edge/modbus/config/model/Model.scala")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    val fw = new PrintWriter(new FileOutputStream(f))
    ScalaGen.output("io.greenbus.edge.modbus.config.model", ModbusGatewaySchema.ns.name, all, fw)
    fw.flush()
  }
}