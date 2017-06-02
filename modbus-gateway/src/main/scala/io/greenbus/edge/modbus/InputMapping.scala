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
import io.greenbus.edge.api.Path
import io.greenbus.edge.data.{ SampleValue, ValueBool, ValueDouble, ValueInt64 }
import io.greenbus.edge.modbus.config.model.{ BooleanInput, Conversion, NumericInput }
import org.totalgrid.modbus.{ ByteX2, ModbusBit, ModbusData, ModbusRegister }

object InputMapping extends LazyLogging {

  //val good = Quality.newBuilder.setValidity(Quality.Validity.GOOD).build

  def boolListToMap(list: Seq[BooleanInput]): Map[Int, Seq[BooleanInput]] = list.groupBy(_.index).toMap
  def numListToMap(list: Seq[NumericInput]): Map[Int, Seq[NumericInput]] = list.groupBy(_.index).toMap

  def build(value: Boolean) = ValueBool(value)

  def build(value: Long) = ValueInt64(value)

  def build(value: Double) = ValueDouble(value)

  case class ByteX4(first: ByteX2, second: ByteX2)

  def parseBitMask(s: String): Long = {
    val trimmed = s.trim
    if (trimmed.startsWith("0x")) {
      java.lang.Long.parseLong(trimmed.drop(2), 16)
    } else {
      java.lang.Long.parseLong(trimmed)
    }
  }

  class RegisterMap(registers: Traversable[ModbusRegister]) {
    private val map = registers.map(x => (x.index, x.value)).toMap

    def get(i: Int): Option[ByteX2] = map.get(i)
    def get32(i: Int): Option[ByteX4] = for (x <- map.get(i); y <- map.get(i + 1)) yield ByteX4(x, y)
    def get64(i: Int): Option[IndexedSeq[ByteX2]] = {
      for {
        v1 <- map.get(i)
        v2 <- map.get(i + 1)
        v3 <- map.get(i + 2)
        v4 <- map.get(i + 3)
      } yield {
        Vector(v1, v2, v3, v4)
      }
    }
  }
}
class InputMapping(di: Seq[BooleanInput], cs: Seq[BooleanInput], ir: Seq[NumericInput], hr: Seq[NumericInput]) extends LazyLogging {
  import InputMapping._

  def convertDiscreteInput(list: Traversable[ModbusBit]): Traversable[(String, SampleValue)] = {
    handleBinary(list, di)
  }
  def convertCoilStatus(list: Traversable[ModbusBit]): Traversable[(String, SampleValue)] = {
    handleBinary(list, cs)
  }

  private def handleBinary(list: Traversable[ModbusBit], mappings: Traversable[BooleanInput]) = {
    val map: Map[Int, Boolean] = list.map(mb => (mb.index, mb.value)).toMap
    mappings.flatMap { m =>
      map.get(m.index).map(v => (m.name, build(v)))
    }
  }

  def convertInputRegister(list: Traversable[ModbusRegister]): Traversable[(String, SampleValue)] = {
    handleNumeric(list, ir)
  }

  def convertHoldingRegister(list: Traversable[ModbusRegister]): Traversable[(String, SampleValue)] = {
    handleNumeric(list, hr)
  }

  private def handleNumeric(list: Traversable[ModbusRegister], mappings: Traversable[NumericInput]): Traversable[(String, SampleValue)] = {
    val regMap = new RegisterMap(list)
    mappings.flatMap { m =>
      val i = m.index
      val maskOpt = m.bitMask.map(InputMapping.parseBitMask)
      val shiftRightOpt = m.shiftRight.map(_.toInt)

      def handleMask(raw: Long): Long = {
        maskOpt match {
          case None => raw
          case Some(mask) =>
            val masked = raw & mask
            shiftRightOpt match {
              case None => masked
              case Some(shift) => masked >> shift
            }
        }
      }

      val measOpt: Option[SampleValue] = m.conversionType match {
        case Conversion.SInt16 => regMap.get(i).map(r => build(r.sInt16))
        case Conversion.UInt16 => regMap.get(i).map(r => build(handleMask(r.uInt16)))
        case Conversion.SInt32LE => regMap.get32(i).map(r => build(ModbusData.joinSInt32LE(r.first, r.second)))
        case Conversion.SInt32BE => regMap.get32(i).map(r => build(ModbusData.joinSInt32BE(r.first, r.second)))
        case Conversion.UInt32LE => regMap.get32(i).map(r => build(handleMask(ModbusData.joinUInt32LE(r.first, r.second))))
        case Conversion.UInt32BE => regMap.get32(i).map(r => build(handleMask(ModbusData.joinUInt32BE(r.first, r.second))))
        case Conversion.Float32LE => regMap.get32(i).map(r => build(ModbusData.joinFloat32LE(r.first, r.second)))
        case Conversion.Float32BE => regMap.get32(i).map(r => build(ModbusData.joinFloat32BE(r.first, r.second)))
        case Conversion.Float64LE => regMap.get64(i).map(r => build(ModbusData.joinFloat64LE(r(0), r(1), r(2), r(3))))
        case Conversion.Float64BE => regMap.get64(i).map(r => build(ModbusData.joinFloat64BE(r(0), r(1), r(2), r(3))))
        case _ =>
          logger.warn("Unknown toNumber: " + m.conversionType + " on index: " + i + " name: " + m.name)
          None
      }

      measOpt.map(meas => (m.name, meas))
    }
  }
}
