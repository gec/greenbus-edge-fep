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

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.api.stream.{ KeyMetadata, OutputStatusHandle, ProducerHandle, SeriesValueHandle }
import io.greenbus.edge.data.{ SampleValue, Value }
import io.greenbus.edge.dnp3.config.model._
import io.greenbus.edge.flow
import io.greenbus.edge.flow.Receiver
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller
import org.totalgrid.dnp3.{ CommandResponse, CommandStatus, ControlCode }

case class ControlEntry(path: Path, name: String, status: OutputStatusHandle, rcv: Receiver[OutputParams, OutputResult])

case class RawDnpOutputConfig(status: OutputStatusHandle, rcv: Receiver[OutputParams, OutputResult], typedConfig: DnpOutputTypeConfig)

sealed trait DnpOutputTypeConfig
case class DnpControlConfig(index: Int, function: FunctionType, options: ControlOptions) extends DnpOutputTypeConfig
case class DnpSetpointConfig(index: Int, function: FunctionType) extends DnpOutputTypeConfig

object RawDnpEndpoint {
  def build(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices, config: DNP3Gateway): (MeasObserver, DNPKeyedControlAdapter) = {

    val path = Path(Seq("dnp", localId, s"${config.client.host}_${config.client.port}"))
    val b = producerServices.endpointBuilder(EndpointId(path))

    def loadRange(prefix: String, range: IndexRange): Seq[(String, SeriesValueHandle)] = {
      Range(range.start, range.count).map { i =>
        val key = MeasAdapter.id(prefix, i)
        (key, b.seriesValue(Path(Seq(key))))
      }
    }

    def loadSet(prefix: String, set: IndexSet): Seq[(String, SeriesValueHandle)] = {
      set.value.flatMap(loadRange(prefix, _))
    }

    val dataKeys = loadSet(MeasAdapter.binaryPrefix, config.inputModel.binaryInputs) ++
      loadSet(MeasAdapter.analogPrefix, config.inputModel.analogInputs) ++
      loadSet(MeasAdapter.counterPrefix, config.inputModel.counterInputs) ++
      loadSet(MeasAdapter.controlStatusPrefix, config.inputModel.binaryOutputs) ++
      loadSet(MeasAdapter.setpointStatusPrefix, config.inputModel.analogOutputs)

    val dataKeyMap: Map[String, SeriesValueHandle] = dataKeys.toMap

    val cfgKey = b.latestKeyValue(Path("config"))

    val handleConfig = Vector.newBuilder[(String, DnpOutputTypeConfig)]
    val controlEntries = Vector.newBuilder[ControlEntry]

    config.outputModel.controls.foreach { control =>

      val path = Path(control.name)
      val status: OutputStatusHandle = b.outputStatus(path, KeyMetadata())
      val rcv: Receiver[OutputParams, OutputResult] = b.registerOutput(path)

      controlEntries += ControlEntry(path, control.name, status, rcv)
      handleConfig += (control.name -> DnpControlConfig(control.index, control.function, control.controlOptions))
    }

    config.outputModel.setpoints.map { setpoint =>

      val path = Path(setpoint.name)
      val status: OutputStatusHandle = b.outputStatus(path, KeyMetadata())
      val rcv: Receiver[OutputParams, OutputResult] = b.registerOutput(path)

      controlEntries += ControlEntry(path, setpoint.name, status, rcv)
      handleConfig += (setpoint.name -> DnpSetpointConfig(setpoint.index, setpoint.function))
    }

    val built = b.build(100, 100)

    cfgKey.update(DNP3Gateway.write(config))

    val controlAdapter = new DNPKeyedControlAdapterImpl(handleConfig.result().toMap)
    val endpoint = new RawDnpEndpoint(eventThread, built, controlAdapter, dataKeyMap, controlEntries.result())

    (endpoint, controlAdapter)
  }
}
class RawDnpEndpoint(eventThread: CallMarshaller,
    handle: ProducerHandle,
    controlAdapter: DNPKeyedControlAdapter,
    mapping: Map[String, SeriesValueHandle],
    outputs: Seq[ControlEntry]) extends MeasObserver with LazyLogging {

  private var cmdHandleOpt = Option.empty[DNP3ControlHandle]

  def init(): Unit = {
    outputs.foreach { entry =>
    }
  }

  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    //logger.debug(s"Raw batch: ${batch}")
    val now = System.currentTimeMillis()
    batch.foreach {
      case (key, sample) =>
        mapping.get(key).foreach(handle => handle.update(sample, now))
    }
    handle.flush()
  }

  def set(commandHandle: DNP3ControlHandle): Unit = {
    eventThread.marshal {
      cmdHandleOpt = Some(commandHandle)
    }
  }
}

/*
simple indication
parameterized indication
analog setpoint
boolean setpoint
enumeration setpoint


 */

/*class RawGatewayControlAdapter(eventThread: CallMarshaller, dnp: DNP3ControlHandle, session: UUID, config: Map[String, RawDnpOutputConfig], handle: ProducerHandle) extends LazyLogging {


  private def init(): Unit = {
    config.foreach {
      case (name, cfg) => {
        cfg.rcv.bind(new flow.Responder[OutputParams, OutputResult] {
          def handle(params: OutputParams, respond: (OutputResult) => Unit): Unit = {
            eventThread.marshal {
              handleOutput(name, cfg, params, respond)
            }
          }
        })
      }
    }
  }

  private def handleOutput(name: String, config: RawDnpOutputConfig, params: OutputParams, respond: OutputResult => Unit): Unit = {
    logger.debug(s"Handling output for $name: $params")

    config.typedConfig match {
      case control: RawDnpControlConfig => {
        dnp.issueControl(control.index, )
      }
      case setpoint: RawDnpSetpointConfig =>
    }

  }


  def handle(name: String, valueOpt: Option[Value], result: (OutputResult) => Unit): Unit = {
    config.get(name) match {
      case None => result(OutputFailure(s"Command $name not mapped"))
      case Some(cfg) =>
        //cfg.
    }
  }
}*/

sealed trait DNPSetpointValue
case class IntegerSetpointValue(value: Long) extends DNPSetpointValue
case class DoubleSetpointValue(value: Double) extends DNPSetpointValue

trait DNPKeyedControlAdapter {
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
class DNPKeyedControlAdapterImpl( /*dnp: DNP3ControlHandle,*/ config: Map[String, DnpOutputTypeConfig]) extends DNPKeyedControlAdapter with LazyLogging {
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

/*class ControlAdapter() {


  def issue(name: String, valueOpt: Value) = {
  }

}*/

/*
trait GatewayControlAdapter {
  def handle(name: String, valueOpt: Option[Value], result: OutputResult => Unit)
}*/
