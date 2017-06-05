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

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.fep.config.model._
import io.greenbus.edge.modbus.config.model._
import org.junit.runner.RunWith
import org.scalatest.{ BeforeAndAfterAll, FunSuite, Matchers }
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntegrationTest extends FunSuite with Matchers with LazyLogging with BeforeAndAfterAll {

  val slave = new ModbusSlave(34001)

  override protected def afterAll(): Unit = {
    slave.close()
  }

  ignore("test") {

    val config = ModbusGateway(
      Master(
        TCPClient("127.0.0.1", 34001, 1000),
        ProtocolType.TCPIP,
        0,
        Seq(Poll(DataType.InputRegister, 0, 10, 1000, 2000)),
        discreteInputs = Seq(),
        coilStatuses = Seq(),
        inputRegisters = Seq(NumericInput(0, "ni00", Conversion.SInt16, None, None)),
        holdingRegisters = Seq(),
        commandMappings = Seq()),
      FrontendConfiguration(
        endpointId = Path(Seq("frontend", "device")),
        metadata = Seq(
          MetadataKeyValue(Path(Seq("myns", "endpointv01")), MetadataStringValue("my value")),
          MetadataKeyValue(Path(Seq("myns", "endpointv02")), MetadataStringValue("my second value"))),
        dataKeys = Seq(
          DataKeyConfig(
            gatewayKey = "ni00",
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
        outputKeys = Seq()))
  }

}
