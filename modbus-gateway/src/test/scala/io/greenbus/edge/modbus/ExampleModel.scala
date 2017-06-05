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
package io.greenbus.edge.modbus

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import io.greenbus.edge.data.json.{ EdgeJsonReader, EdgeJsonWriter }
import io.greenbus.edge.data.mapping.SimpleReaderContext
import io.greenbus.edge.fep.config.model._
import io.greenbus.edge.modbus.config.model._

object ExampleModel {

  def build: ModbusGateway = {
    ModbusGateway(
      ModbusMaster(
        tcpClient = TCPClient("127.0.0.1", 50001, 5000),
        protocol = ProtocolType.TCPIP,
        address = 0,
        polls = Seq(Poll(DataType.InputRegister, 0, 20, 2000, 2000)),
        discreteInputs = Seq(),
        coilStatuses = Seq(),
        inputRegisters = Seq(NumericInput(0, "analog_0", Conversion.UInt16, None, None)),
        holdingRegisters = Seq(),
        commandMappings = Seq(
          OutputMapping(
            index = 0,
            name = "writeReg01",
            commandType = CommandType.Register,
            constBooleanValue = None,
            constIntValue = None,
            bitMaskToUpdate = None,
            shiftLeft = None,
            registerCount = None))),
      FrontendConfiguration(
        endpointId = Path(Seq("my", "device", "01")),
        metadata = Seq(
          MetadataKeyValue(Path(Seq("myns", "endpointv01")), MetadataStringValue("my value")),
          MetadataKeyValue(Path(Seq("myns", "endpointv02")), MetadataStringValue("my second value"))),
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
            filter = Some(FilterDescriptor(suppressDuplicates = None, deadband = Some(0.01))))),
        outputKeys = Seq(
          OutputKeyConfig(
            gatewayKey = "writeReg01",
            path = Path(Seq("SetOutputPower")),
            metadata = Seq(MetadataKeyValue(Path(Seq("myns", "outputKey01")), MetadataBoolValue(true))),
            descriptor = OutputDescriptor(
              OutputType.AnalogSetpoint,
              requestScale = Some(100),
              requestOffset = None,
              requestIntegerLabels = None,
              requestBooleanLabels = None),
            associatedDataKeys = Seq(Path(Seq("outputPower")))))))
  }
}

object ReadWriteModel {

  def main(args: Array[String]): Unit = {
    val model = ExampleModel.build
    println(model)
    val value = ModbusGateway.write(model)
    println(value)
    val os = new ByteArrayOutputStream()
    EdgeJsonWriter.write(value, os)
    val content = new String(os.toByteArray, "UTF-8")
    println(content)

    val valueReadOpt = EdgeJsonReader.read(new ByteArrayInputStream(os.toByteArray))
    val valueRead = valueReadOpt.get
    println(valueRead)

    val roundTripped = ModbusGateway.read(valueRead, SimpleReaderContext(Vector()))
    println(roundTripped)
    roundTripped match {
      case Left(err) => println(s"error: $err")
      case Right(readModel) =>
        println(model)
        println(readModel)
        println(model == readModel)
    }
  }

}