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

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api.{ OutputFailure, OutputResult, OutputSuccess }
import io.greenbus.edge.dnp3.config.model.{ ControlType, FunctionType }
import org.totalgrid.dnp3.{ CommandResponse, CommandStatus, ControlCode }

sealed trait DNPSetpointValue
case class IntegerSetpointValue(value: Long) extends DNPSetpointValue
case class DoubleSetpointValue(value: Double) extends DNPSetpointValue

trait DNPKeyedControlAdapter {
  def setHandle(handle: DNP3ControlHandle): Unit
  def handle(name: String, valueOpt: Option[DNPSetpointValue], result: OutputResult => Unit): Boolean
}

object DNPKeyedControlAdapterImpl {
  def translateResponse(commandStatus: CommandStatus): OutputResult = {
    commandStatus match {
      case CommandStatus.CS_SUCCESS => OutputSuccess(None)
      case CommandStatus.CS_TIMEOUT => OutputFailure("DNP3 TIMEOUT")
      case CommandStatus.CS_NO_SELECT => OutputFailure("DNP3 NO SELECT")
      case CommandStatus.CS_FORMAT_ERROR => OutputFailure("DNP3 FORMAT ERROR")
      case CommandStatus.CS_NOT_SUPPORTED => OutputFailure("DNP3 NOT SUPPORTED")
      case CommandStatus.CS_ALREADY_ACTIVE => OutputFailure("DNP3 ALREADY ACTIVE")
      case CommandStatus.CS_HARDWARE_ERROR => OutputFailure("DNP3 HARDWARE ERROR")
      case CommandStatus.CS_LOCAL => OutputFailure("DNP3 LOCAL")
      case CommandStatus.CS_TOO_MANY_OPS => OutputFailure("DNP3 TOO MANY OPERATIONS")
      case CommandStatus.CS_NOT_AUTHORIZED => OutputFailure("DNP3 UNAUTHORIZED")
      case _ => OutputFailure("DNP3 Unknown")
    }
  }
}
class DNPKeyedControlAdapterImpl(config: Map[String, DnpOutputTypeConfig]) extends DNPKeyedControlAdapter with LazyLogging {
  import DNPKeyedControlAdapterImpl._

  private var handleOpt = Option.empty[DNP3ControlHandle]

  def setHandle(handle: DNP3ControlHandle): Unit = {
    handleOpt = Some(handle)
  }

  def handle(name: String, valueOpt: Option[DNPSetpointValue], result: OutputResult => Unit): Boolean = {
    handleOpt match {
      case None => false
      case Some(dnp) =>
        config.get(name) match {
          case None =>
            logger.info(s"Unmapped DNP control request: $name")
            false
          case Some(cfg) => {
            cfg match {
              case control: DnpControlConfig => {
                val isDirectOperate = control.function match {
                  case FunctionType.SelectBeforeOperate => false
                  case FunctionType.DirectOperate => true
                }

                val cmdType = control.options.controlType match {
                  case ControlType.PULSE => ControlCode.CC_PULSE
                  case ControlType.PULSE_CLOSE => ControlCode.CC_PULSE_CLOSE
                  case ControlType.PULSE_TRIP => ControlCode.CC_PULSE_TRIP
                  case ControlType.LATCH_ON => ControlCode.CC_LATCH_ON
                  case ControlType.LATCH_OFF => ControlCode.CC_LATCH_OFF
                }

                def onResult(cmd: CommandResponse): Unit = {
                  result(translateResponse(cmd.getMResult))
                }

                logger.info(s"Issuing DNP control to ${control.index}")
                dnp.issueControl(control.index,
                  cmdType,
                  control.options.count.map(_.toShort),
                  control.options.onTime.map(_.toLong),
                  control.options.offTime.map(_.toLong),
                  isDirectOperate,
                  onResult)

                true
              }
              case setpoint: DnpSetpointConfig => {
                valueOpt match {
                  case None =>
                    logger.info(s"DNP setpoint request without value: $name")
                    false
                  case Some(value) => {

                    val isDirectOperate = setpoint.function match {
                      case FunctionType.SelectBeforeOperate => false
                      case FunctionType.DirectOperate => true
                    }

                    def onResult(cmd: CommandResponse): Unit = {
                      result(translateResponse(cmd.getMResult))
                    }

                    logger.info(s"Issuing DNP setpoint to ${setpoint.index}")
                    dnp.issueSetpoint(setpoint.index, value, isDirectOperate, onResult)
                    true
                  }
                }
              }
            }
          }
        }
    }
  }
}
