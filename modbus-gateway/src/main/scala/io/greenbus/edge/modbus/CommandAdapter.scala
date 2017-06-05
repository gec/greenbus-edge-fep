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
import io.greenbus.edge.api.{ OutputFailure, OutputParams, OutputResult, OutputSuccess }
import io.greenbus.edge.data.NumericConvertible
import io.greenbus.edge.fep.FrontendOutputDelegate
import io.greenbus.edge.modbus.config.model.{ CommandType, OutputMapping }
import org.totalgrid.modbus.ModbusOperations

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CommandAdapter(mappings: Seq[OutputMapping]) extends FrontendOutputDelegate with LazyLogging {

  private var opsOpt = Option.empty[ModbusOperations]

  private val nameMap = {
    mappings.map(m => (m.name, m)).toMap
  }

  def setOps(ops: ModbusOperations): Unit = {
    opsOpt = Some(ops)
  }

  def handleOutput(name: String, params: OutputParams, result: (OutputResult) => Unit): Unit = {

    opsOpt.foreach { ops =>

      def badRequest(err: String): Unit = {
        result(OutputFailure(err))
      }

      def mapFuture(fut: Future[Boolean]): Unit = {
        fut.foreach {
          case true => result(OutputSuccess(None))
          case false => result(OutputFailure("Modbus stack could not complete request"))
        }
        fut.failed.foreach { ex => result(OutputFailure(ex.getMessage)) }
      }

      def simpleRegisterWrite(index: Int, value: Long): Unit = {
        if ((value >= 0 && value < 65536) || (value < 0 && value >= Short.MinValue)) {
          mapFuture(ops.writeSingleRegister(index, value.toInt))
        } else {
          badRequest("Setpoint value out of range for signed or unsigned 16-bit integer")
        }
      }

      def multiRegisterWrite(index: Int, count: Int, value: Long): Unit = {
        val values: Seq[Int] = Range(0, count).map { i =>
          ((value >> (i * 16)) & 0xFFFF).toInt
        }

        mapFuture(ops.writeMultipleRegisters(index, values))
      }

      def maskedRegisterWrite(index: Int, value: Long, maskStr: String, leftShift: Option[Int]): Unit = {
        val mask = InputMapping.parseBitMask(maskStr)

        ops.readHoldingRegisters(index, 1).foreach {
          case Seq(orig) => {
            val origValue = orig.value.uInt16

            val shiftedInputValue: Int = leftShift.map(l => value.toInt << l).getOrElse(value.toInt)

            val targetValue = (origValue & ~mask) | (shiftedInputValue & mask)

            mapFuture(ops.writeSingleRegister(index, targetValue.toInt))
          }
          case _ => badRequest("Did not recognize current state of holding register")
        }
      }

      def intValueOpt(mapping: OutputMapping): Option[Long] = {
        mapping.constIntValue match {
          case Some(constIntVal) => Some(constIntVal.toLong)
          case None =>
            params.outputValueOpt.flatMap {
              case v: NumericConvertible => Some(v.toLong)
              case _ => None
            }
        }
      }

      nameMap.get(name) match {
        case None => badRequest("No mapping for command")
        case Some(mapping) =>
          mapping.commandType match {
            case CommandType.Coil => {
              mapping.constBooleanValue match {
                case None => badRequest("No constant coil value provided to translate control to")
                case Some(v) => mapFuture(ops.writeSingleCoil(mapping.index, v))
              }
            }
            case CommandType.Register => {
              val integerValueOpt: Option[Long] = intValueOpt(mapping)

              integerValueOpt match {
                case None => badRequest("Setpoint value not present or invalid and no constant value provided")
                case Some(value) => {
                  mapping.bitMaskToUpdate match {
                    case None => simpleRegisterWrite(mapping.index, value)
                    case Some(maskStr) => maskedRegisterWrite(mapping.index, value, maskStr, mapping.shiftLeft)
                  }
                }
              }
            }
            case CommandType.MultipleRegisters => {
              val integerValueOpt: Option[Long] = intValueOpt(mapping)

              integerValueOpt match {
                case None => badRequest("Setpoint value not present or invalid and no constant value provided")
                case Some(value) => {
                  val regCount = mapping.registerCount.map(_.toInt).getOrElse(1)
                  if (regCount > 4 || regCount < 1) {
                    badRequest("Bad configuration, write multiple registers must have count 1-4")
                  }
                  mapping.bitMaskToUpdate match {
                    case None => multiRegisterWrite(mapping.index, regCount, value)
                    case Some(maskStr) =>
                      badRequest("Masked writes of multiple registers not supported")
                  }
                }
              }
            }
            case CommandType.MaskRegister => {
              val integerValueOpt: Option[Long] = intValueOpt(mapping)

              integerValueOpt match {
                case None => badRequest("Setpoint value not present or invalid and no constant value provided")
                case Some(value) => {
                  val maskToUpdate: Long = mapping.bitMaskToUpdate match {
                    case None => 0xFFFF
                    case Some(maskStr) => InputMapping.parseBitMask(maskStr)
                  }

                  val shiftLeftAmount = mapping.shiftLeft

                  val orMask = shiftLeftAmount.map(i => value << i).getOrElse(value)
                  val andMask = (~maskToUpdate & 0xFF) | (~maskToUpdate & 0xFF00) // for the Modbus mask write function, mask is what of the current value to *preserve*

                  mapFuture(ops.maskWriteRegister(mapping.index, andMask.toInt, orMask.toInt))
                }
              }
            }
          }
      }

    }
  }
}
