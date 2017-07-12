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
import io.greenbus.edge.data.codegen.ScalaGen
import io.greenbus.edge.data.schema._
import io.greenbus.edge.data.xml.{ SchemaWriter, XmlNamespaceInfo, XmlNsDecl }

object FrontendSchema {

  val ns = TypeNamespace("io.greenbus.edge.fep.config.model", Map("scalaPackage" -> "io.greenbus.edge.fep.config.model", "xmlns" -> "io.greenbus.edge.fep.config.model"))

  val sampleTypeEnum: TExt = {
    TExt(ns, "SampleType", TEnum(Seq(
      EnumDef("Float", 0, "32-bit floating point."),
      EnumDef("Double", 1, "64-bit floating point."),
      EnumDef("Int32", 2, "32-bit signed integer."),
      EnumDef("UInt32", 3, "32-bit unsigned integer."),
      EnumDef("Int64", 4, "64-bit signed integer."),
      EnumDef("UInt64", 5, "64-bit unsigned integer."),
      EnumDef("Bool", 6, "Boolean value."),
      EnumDef("Byte", 7, "8-bit unsigned value."))),
    """Sample value type.""")
  }
  val seriesTypeEnum: TExt = {
    TExt(ns, "SeriesType", TEnum(Seq(
      EnumDef("AnalogStatus", 0, "Analog value where values are assumed to be the same until updated."),
      EnumDef("AnalogSample", 1, "Analog value where values are discrete samples."),
      EnumDef("CounterStatus", 2, "Counter value where values are assumed to be the same until updated."),
      EnumDef("CounterSample", 3, "Counter value where values are discrete samples."),
      EnumDef("BooleanStatus", 4, "Boolean value where values are assumed to be the same until updated."),
      EnumDef("IntegerEnum", 5, "Analog integer value where a discrete set of possible values map to enumerated labels."))),
    """Semantics of time-series values.""")
  }
  val outputTypeEnum: TExt = {
    TExt(ns, "OutputType", TEnum(Seq(
      EnumDef("SimpleIndication", 0, "An indication that does not carry a value."),
      EnumDef("AnalogSetpoint", 1, "A setpoint that carries and analog (integer or floating-point) value."),
      EnumDef("BooleanSetpoint", 2, "A setpoint that carries a boolean value."),
      EnumDef("EnumerationSetpoint", 3, "A setpoint that carries an integer that represents a discrete enumerated set of states."))),
      """Core type of output.""")
  }

  val path: TExt = {
    TExt(ns, "Path", TList(TString))
  }

  val typeCast: TExt = {
    TExt(ns, "TypeCast", TStruct(Vector(
      StructFieldDef("target", sampleTypeEnum, 0, "Sample value type."))),
    """Specifies a transform that casts a value to a target value type.""")
  }

  val linearTransform: TExt = {
    TExt(ns, "LinearTransform", TStruct(Vector(
      StructFieldDef("scale", TDouble, 0, "Scale applied to input value."),
      StructFieldDef("offset", TDouble, 1, "Offset applied to input value after value is scaled."))),
      """Specifies a linear transform, applied to input."""
    )
  }

  val simpleTransformTypeEnum: TExt = {
    TExt(ns, "SimpleTransformType", TEnum(Seq(
      EnumDef("Negate", 0, "Simple transform to negate a boolean value."))))
  }

  val simpleTransform: TExt = {
    TExt(ns, "SimpleTransform", TStruct(Vector(
      StructFieldDef("transformType", simpleTransformTypeEnum, 0, "Simple transform type to apply."))),
    """Transform specified by an enumeration of simple operations.""")
  }

  val integerLabel: TExt = {
    TExt(ns, "IntegerLabel", TStruct(Vector(
      StructFieldDef("value", TInt32, 0, "Integer value to map."),
      StructFieldDef("label", TString, 1, "Label of integer value."))),
    """Mapping of an integer value to a string label.""")
  }

  val integerLabelSet: TExt = {
    TExt(ns, "IntegerLabelSet", TList(integerLabel), """List of mappings between integer values and string labels.""")
  }

  val booleanLabels: TExt = {
    TExt(ns, "BooleanLabels", TStruct(Vector(
      StructFieldDef("trueLabel", TString, 0, "Label applied when a boolean value is true."),
      StructFieldDef("falseLabel", TString, 1, "Label applied when a boolean value is false."))),
    """Mapping of boolean values to string labels.""")
  }

  val seriesDescriptor: TExt = {
    TExt(ns, "SeriesDescriptor", TStruct(Vector(
      StructFieldDef("seriesType", seriesTypeEnum, 0, "Semantics of time-series values."),
      StructFieldDef("unit", TOption(TString), 1, "Unit of values."),
      StructFieldDef("decimalPoints", TOption(TUInt32), 2, "Decimal points valid for floating-point values."),
      StructFieldDef("integerLabels", TOption(integerLabelSet), 3, "Mapping of integer values to string labels."),
      StructFieldDef("booleanLabels", TOption(booleanLabels), 4, "Mapping of boolean values to string labels."))),
    """Descriptor of data key value type.""")
  }

  val transformDescriptor: TExt = {
    TExt(ns, "TransformDescriptor", TUnion(Set(typeCast, linearTransform, simpleTransform)))
  }

  val filterDescriptor: TExt = {
    TExt(ns, "FilterDescriptor", TStruct(Vector(
      StructFieldDef("suppressDuplicates", TOption(TBool), 0, "Whether to suppress duplicate values. Defaults to true."),
      StructFieldDef("deadband", TOption(TDouble), 1, "Deadband that much be exceeded for a new value to be published."))),
    """Filter settings for publishing values.""")
  }

  val metadataStringValue: TExt = {
    TExt(ns, "MetadataStringValue", TStruct(Vector(
      StructFieldDef("value", TString, 0, "Metadata value."))),
    "String metadata value.")
  }
  val metadataIntegerValue: TExt = {
    TExt(ns, "MetadataIntegerValue", TStruct(Vector(
      StructFieldDef("value", TInt64, 0, "Metadata value."))),
      "Integer metadata value.")
  }
  val metadataBoolValue: TExt = {
    TExt(ns, "MetadataBoolValue", TStruct(Vector(
      StructFieldDef("value", TBool, 0, "Metadata value."))),
      "Boolean metadata value.")
  }
  val metadataDoubleValue: TExt = {
    TExt(ns, "MetadataDoubleValue", TStruct(Vector(
      StructFieldDef("value", TDouble, 0, "Metadata value."))),
      "Double metadata value.")
  }

  val metadataValue: TExt = {
    TExt(ns, "MetadataValue", TUnion(Set(
      metadataStringValue,
      metadataIntegerValue,
      metadataBoolValue,
      metadataDoubleValue)))
  }

  val metadataItem: TExt = {
    TExt(ns, "MetadataKeyValue", TStruct(Vector(
      StructFieldDef("path", path, 0, "Metadata key."),
      StructFieldDef("value", metadataValue, 1, "Metadata value."))),
    """Metadata key-value pair.""")
  }

  val frontendDataKey: TExt = {
    TExt(ns, "DataKeyConfig", TStruct(Vector(
      StructFieldDef("gatewayKey", TString, 0, "Name of configured input to supply values for data key."),
      StructFieldDef("path", path, 1, "ID of data key."),
      StructFieldDef("metadata", TList(metadataItem), 2, "Metadata key values of data key."),
      StructFieldDef("descriptor", seriesDescriptor, 3, "Descriptor of data key value type."),
      StructFieldDef("transforms", TList(transformDescriptor), 4, "Transforms of data values performed before publishing."),
      StructFieldDef("filter", TOption(filterDescriptor), 5, "Filter settings for publishing values."))),
      """Configuration of an endpoint data key.""")
  }

  val outputDescriptor: TExt = {
    TExt(ns, "OutputDescriptor", TStruct(Vector(
      StructFieldDef("outputType", outputTypeEnum, 0, "Output type."),
      StructFieldDef("requestScale", TOption(TDouble), 1, "Scale to apply to human-specified output values."),
      StructFieldDef("requestOffset", TOption(TDouble), 2, "Offset to apply to human-specified output values."),
      StructFieldDef("requestIntegerLabels", TOption(integerLabelSet), 3, "Integer label mapping for output values."),
      StructFieldDef("requestBooleanLabels", TOption(booleanLabels), 4, "Boolean label mapping for output values."))),
    """Descriptor of output key.""")
  }

  val frontendOutputKey: TExt = {
    TExt(ns, "OutputKeyConfig", TStruct(Vector(
      StructFieldDef("gatewayKey", TString, 0, "Name of configured output to target."),
      StructFieldDef("path", path, 1, "ID of output key."),
      StructFieldDef("metadata", TList(metadataItem), 2, "Metadata key values of output key."),
      StructFieldDef("descriptor", outputDescriptor, 3, "Descriptor of output key."),
      StructFieldDef("associatedDataKeys", TList(path), 4, "Data keys associated with output key."))),
      """Configuration of an endpoint output key.""")
  }

  val frontendConfiguration: TExt = {
    TExt(ns, "FrontendConfiguration", TStruct(Vector(
      StructFieldDef("endpointId", path, 0, "ID of published endpoint."),
      StructFieldDef("metadata", TList(metadataItem), 1, "Metadata key values of published endpoint."),
      StructFieldDef("dataKeys", TList(frontendDataKey), 2, "Data keys of published endpoint."),
      StructFieldDef("outputKeys", TList(frontendOutputKey), 3, "Output keys of published endpoint."))),
      """Defines an endpoint to be published based on a set of configured inputs and outputs.""")
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
    metadataItem,
    metadataValue,
    metadataStringValue,
    metadataBoolValue,
    metadataIntegerValue,
    metadataDoubleValue,
    frontendDataKey,
    outputDescriptor,
    frontendOutputKey,
    frontendConfiguration)
}

object FepXmlSchemaWriter {

  def main(args: Array[String]): Unit = {

    val f = new File("testschemas/fep.xsd")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    val xmlNs = XmlNamespaceInfo(FrontendSchema.ns.name,
      Map(
        (FrontendSchema.ns.name, XmlNsDecl("fep", FrontendSchema.ns.name))))

    SchemaWriter.write(FrontendSchema.all, Seq(FrontendSchema.frontendConfiguration), xmlNs, new FileOutputStream(f))
  }
}

object FepScalaWriter {

  def main(args: Array[String]): Unit = {
    val all = ScalaGen.collectObjDefs(FrontendSchema.ns.name, FrontendSchema.frontendConfiguration, Map())

    println(all)

    val f = new File("frontend/src/main/scala/io/greenbus/edge/fep/config/model/Model.scala")
    Files.createParentDirs(f)
    if (!f.exists()) {
      f.createNewFile()
    }

    val fw = new PrintWriter(new FileOutputStream(f))
    ScalaGen.output("io.greenbus.edge.fep.config.model", FrontendSchema.ns.name, all, fw)
    fw.flush()
  }
}