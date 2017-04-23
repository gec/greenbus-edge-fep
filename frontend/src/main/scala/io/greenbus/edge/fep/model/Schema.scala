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
package io.greenbus.edge.fep.model

import java.io.{ File, FileOutputStream, PrintWriter }

import com.google.common.io.Files
import io.greenbus.edge.data.codegen.Gen
import io.greenbus.edge.data.schema._
import io.greenbus.edge.data.xml.SchemaWriter

object FrontendSchema {

  val sampleTypeEnum: TExt = {
    TExt("SampleType", TEnum(Seq(
      EnumDef("Float", 0),
      EnumDef("Double", 1),
      EnumDef("Int32", 2),
      EnumDef("UInt32", 3),
      EnumDef("Int64", 4),
      EnumDef("UInt64", 5),
      EnumDef("Bool", 6),
      EnumDef("Byte", 7))))
  }
  val seriesTypeEnum: TExt = {
    TExt("SeriesType", TEnum(Seq(
      EnumDef("AnalogStatus", 0),
      EnumDef("AnalogSample", 1),
      EnumDef("CounterStatus", 2),
      EnumDef("CounterSample", 3),
      EnumDef("BooleanStatus", 4),
      EnumDef("IntegerEnum", 5))))
  }
  val outputTypeEnum: TExt = {
    TExt("OutputType", TEnum(Seq(
      EnumDef("SimpleIndication", 0),
      EnumDef("AnalogSetpoint", 1),
      EnumDef("BooleanSetpoint", 2),
      EnumDef("EnumerationSetpoint", 3))))
  }

  val path: TExt = {
    TExt("Path", TList(TString))
  }

  val typeCast: TExt = {
    TExt("TypeCast", TStruct(Vector(
      StructFieldDef("target", sampleTypeEnum, 0))))
  }

  val linearTransform: TExt = {
    TExt("LinearTransform", TStruct(Vector(
      StructFieldDef("scale", TDouble, 0),
      StructFieldDef("offset", TDouble, 1))))
  }

  val simpleTransformTypeEnum: TExt = {
    TExt("SimpleTransformType", TEnum(Seq(
      EnumDef("Negate", 0))))
  }

  val simpleTransform: TExt = {
    TExt("SimpleTransform", TStruct(Vector(
      StructFieldDef("transformType", simpleTransformTypeEnum, 0))))
  }

  val integerLabel: TExt = {
    TExt("IntegerLabel", TStruct(Vector(
      StructFieldDef("value", TInt32, 0),
      StructFieldDef("label", TString, 1))))
  }

  val booleanLabels: TExt = {
    TExt("BooleanLabels", TStruct(Vector(
      StructFieldDef("trueLabel", TString, 0),
      StructFieldDef("falseLabel", TString, 1))))
  }

  val integerLabelSet: TExt = {
    TExt("IntegerLabelSet", TList(integerLabel))
  }

  val seriesDescriptor: TExt = {
    TExt("SeriesDescriptor", TStruct(Vector(
      StructFieldDef("seriesType", seriesTypeEnum, 0),
      StructFieldDef("unit", TOption(TString), 1),
      StructFieldDef("decimalPoints", TOption(TUInt32), 2),
      StructFieldDef("integerLabels", TOption(integerLabelSet), 3),
      StructFieldDef("booleanLabels", TOption(booleanLabels), 4))))
  }

  val transformDescriptor: TExt = {
    TExt("TransformDescriptor", TUnion(Set(typeCast, linearTransform, simpleTransform)))
  }

  val filterDescriptor: TExt = {
    TExt("FilterDescriptor", TStruct(Vector(
      StructFieldDef("suppressDuplicates", TOption(TBool), 0),
      StructFieldDef("deadband", TOption(TDouble), 1))))
  }

  val frontendDataKey: TExt = {
    TExt("DataKeyConfig", TStruct(Vector(
      StructFieldDef("gatewayKey", TString, 0),
      StructFieldDef("path", path, 1),
      StructFieldDef("descriptor", seriesDescriptor, 2),
      StructFieldDef("transforms", TList(transformDescriptor), 3),
      StructFieldDef("filter", TOption(filterDescriptor), 4))))
  }

  val outputDescriptor: TExt = {
    TExt("OutputDescriptor", TStruct(Vector(
      StructFieldDef("outputType", outputTypeEnum, 0),
      StructFieldDef("requestScale", TOption(TDouble), 1),
      StructFieldDef("requestOffset", TOption(TDouble), 2),
      StructFieldDef("requestIntegerLabels", TOption(integerLabelSet), 3),
      StructFieldDef("requestBooleanLabels", TOption(booleanLabels), 4))))
  }

  val frontendOutputKey: TExt = {
    TExt("OutputKeyConfig", TStruct(Vector(
      StructFieldDef("gatewayKey", TString, 0),
      StructFieldDef("path", path, 1),
      StructFieldDef("descriptor", outputDescriptor, 2))))
  }

  val frontendConfiguration: TExt = {
    TExt("FrontendConfiguration", TStruct(Vector(
      StructFieldDef("endpointId", path, 0),
      StructFieldDef("dataKeys", TList(frontendDataKey), 1),
      StructFieldDef("outputKeys", TList(frontendOutputKey), 2))))
  }

  val all = Seq(
    sampleTypeEnum,
    seriesTypeEnum,
    outputTypeEnum,
    path,
    typeCast,
    linearTransform,
    simpleTransformTypeEnum,
    simpleTransform,
    integerLabel,
    integerLabelSet,
    booleanLabels,
    seriesDescriptor,
    transformDescriptor,
    filterDescriptor,
    frontendDataKey,
    outputDescriptor,
    frontendOutputKey,
    frontendConfiguration)
}

object FepXmlSchemaWriter {

  def main(args: Array[String]): Unit = {
    SchemaWriter.write(FrontendSchema.all, Seq(FrontendSchema.path), "io.greenbus.edge.fep.config.model", System.out)
  }
}

object FepSchemaBuilder {

  def main(args: Array[String]): Unit = {
    val all = Gen.collectObjDefs(FrontendSchema.frontendConfiguration, Map())

    println(all.map(_._1).toVector)

    val f = new File("frontend/src/main/scala/io/greenbus/edge/fep/config/model/Model.scala")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    //val fw = new PrintWriter("dnp3-gateway/fakesrc/testfile.scala" /*new FileOutputStream(new File("testdir/testfile.scala"))*/ )
    val fw = new PrintWriter(new FileOutputStream(f))
    Gen.output("io.greenbus.edge.fep.config.model", all, fw)
    fw.flush()
  }

}
