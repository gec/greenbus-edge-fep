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
package io.greenbus.edge.dnp3

import io.greenbus.edge.fep.config.model._

object FrontendConfigExample {

  def build: FrontendConfiguration = {
    FrontendConfiguration(
      endpointId = Path(Seq("mthy", "mgrid", "ess01")),
      metadata = Seq(
        MetadataKeyValue(Path(Seq("myns", "endpointv01")), MetadataStringValue("my value")),
        MetadataKeyValue(Path(Seq("myns", "endpointv02")), MetadataStringValue("my second value"))
      ),
      dataKeys = Seq(
        DataKeyConfig(
          gatewayKey = "analog_0",
          path = Path(Seq("outputPower")),
          metadata = Seq(MetadataKeyValue(Path(Seq("myns", "dataKey01")), MetadataIntegerValue(5))),
          descriptor = SeriesDescriptor(
            seriesType = SeriesType.AnalogStatus,
            unit = Some("kW"),
            decimalPoints = Some(2),
            integerLabels = None,
            booleanLabels = None),
          transforms = Seq(
            LinearTransform(2.0, 5.0)),
          filter = Some(FilterDescriptor(suppressDuplicates = None, deadband = Some(0.01)))),
        DataKeyConfig(
          gatewayKey = "analog_1",
          path = Path(Seq("mode")),
          metadata = Seq(MetadataKeyValue(Path(Seq("myns", "dataKey02")), MetadataDoubleValue(3.14))),
          descriptor = SeriesDescriptor(
            seriesType = SeriesType.IntegerEnum,
            unit = None,
            decimalPoints = None,
            integerLabels = Some(IntegerLabelSet(Seq(
              IntegerLabel(0, "Constant"),
              IntegerLabel(1, "Smoothing"),
              IntegerLabel(2, "GridForming")))),
            booleanLabels = None),
          transforms = Seq(),
          filter = None),
        DataKeyConfig(
          gatewayKey = "binary_0",
          path = Path(Seq("faultStatus")),
          metadata = Seq(MetadataKeyValue(Path(Seq("myns", "dataKey03")), MetadataBoolValue(true))),
          descriptor = SeriesDescriptor(
            seriesType = SeriesType.BooleanStatus,
            unit = None,
            decimalPoints = None,
            integerLabels = None,
            booleanLabels = Some(BooleanLabels(trueLabel = "Fault", falseLabel = "Normal"))),
          transforms = Seq(),
          filter = None)),
      outputKeys = Seq(
        OutputKeyConfig(
          gatewayKey = "setpoint_0",
          path = Path(Seq("SetOutputPower")),
          metadata = Seq(MetadataKeyValue(Path(Seq("myns", "outputKey01")), MetadataBoolValue(true))),
          descriptor = OutputDescriptor(
            OutputType.AnalogSetpoint,
            requestScale = Some(100),
            requestOffset = None,
            requestIntegerLabels = None,
            requestBooleanLabels = None)),
        OutputKeyConfig(
          gatewayKey = "setpoint_1",
          path = Path(Seq("SetMode")),
          metadata = Seq(MetadataKeyValue(Path(Seq("myns", "outputKey02")), MetadataBoolValue(true))),
          descriptor = OutputDescriptor(
            OutputType.EnumerationSetpoint,
            requestScale = None,
            requestOffset = None,
            requestIntegerLabels = Some(IntegerLabelSet(Seq(
              IntegerLabel(0, "Constant"),
              IntegerLabel(1, "Smoothing"),
              IntegerLabel(2, "GridForming")))),
            requestBooleanLabels = None))))
  }

}
