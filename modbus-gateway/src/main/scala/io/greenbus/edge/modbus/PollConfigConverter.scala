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

import io.greenbus.edge.modbus.config.model.{ DataType, ModbusGateway }
import org.totalgrid.modbus.data.UInt16
import org.totalgrid.modbus.poll._

object PollConfigConverter {

  def load(config: ModbusGateway): Seq[Poll] = {
    config.modbus.polls.map { p =>
      p.dataType match {
        case DataType.DiscreteInput => ReadDiscreteInput(UInt16(p.start), UInt16(p.count), p.intervalMs, p.timeoutMs)
        case DataType.CoilStatus => ReadCoilStatus(UInt16(p.start), UInt16(p.count), p.intervalMs, p.timeoutMs)
        case DataType.InputRegister => ReadInputRegister(UInt16(p.start), UInt16(p.count), p.intervalMs, p.timeoutMs)
        case DataType.HoldingRegister => ReadHoldingRegister(UInt16(p.start), UInt16(p.count), p.intervalMs, p.timeoutMs)
      }
    }
  }
}
