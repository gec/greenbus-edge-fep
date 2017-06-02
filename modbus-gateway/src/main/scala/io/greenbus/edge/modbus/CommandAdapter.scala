package io.greenbus.edge.modbus

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api.{OutputParams, OutputResult}
import io.greenbus.edge.fep.FrontendOutputDelegate
import io.greenbus.edge.modbus.config.model.{CommandType, OutputMapping}
import org.totalgrid.modbus.ModbusOperations

import scala.concurrent.Future


class ModbusEndpoint extends FrontendOutputDelegate {

  def handleOutput(name: String, params: OutputParams, respond: (OutputResult) => Unit): Unit = {

  }
}

class CommandAdapter(mappings: Seq[OutputMapping], ops: ModbusOperations) extends LazyLogging {

  def errorStatus(status: CommandStatus, message: String) = Future.successful(CommandResult.newBuilder().setStatus(status).setErrorMessage(message).build())
  def badRequest(message: String) = errorStatus(CommandStatus.NOT_SUPPORTED, message)

  def mapFuture(fut: Future[Boolean]): Future[CommandResult] = {
    fut.map {
      case true => CommandResult.newBuilder().setStatus(CommandStatus.SUCCESS).build()
      case false => CommandResult.newBuilder().setStatus(CommandStatus.UNDEFINED).setErrorMessage("Modbus stack could not complete request").build()
    }
  }

  private val nameMap = {
    mappings.map(m => (m.name, m)).toMap
  }

  private def simpleRegisterWrite(index: Int, value: Long): Future[CommandResult] = {
    if ((value >= 0 && value < 65536) || (value < 0 && value >= Short.MinValue)) {
      mapFuture(ops.writeSingleRegister(index, value.toInt))
    } else {
      badRequest("Setpoint value out of range for signed or unsigned 16-bit integer")
    }
  }

  private def multiRegisterWrite(index: Int, count: Int, value: Long): Future[CommandResult] = {
    val values: Seq[Int] = Range(0, count).map { i =>
      ((value >> (i * 16)) & 0xFFFF).toInt
    }

    mapFuture(ops.writeMultipleRegisters(index, values))
  }

  private def maskedRegisterWrite(index: Int, value: Long, maskStr: String, leftShift: Option[Int]): Future[CommandResult] = {
    val mask = InputMapping.parseBitMask(maskStr)

    ops.readHoldingRegisters(index, 1).flatMap {
      case Seq(orig) => {
        val origValue = orig.value.uInt16

        val shiftedInputValue: Int = leftShift.map(l => value.toInt << l).getOrElse(value.toInt)

        val targetValue = (origValue & ~mask) | (shiftedInputValue & mask)

        mapFuture(ops.writeSingleRegister(index, targetValue.toInt))
      }
      case _ => errorStatus(CommandStatus.UNDEFINED, "Did not recognize current state of holding register")
    }
  }


  def handle(name: String, valueOpt: Option[DNPSetpointValue], result: OutputResult => Unit): Boolean = {


    def intValueOpt(mapping: OutputMapping): Option[Long] = {
      mapping.constIntValue match {
        case Some(constIntVal) => Some(constIntVal.toLong)
        case None =>
          if (request.hasType && request.getType == CommandRequest.ValType.INT) {
            Some(request.getIntVal)
          } else if (request.hasType && request.getType == CommandRequest.ValType.DOUBLE) {
            Some(request.getDoubleVal.toLong)
          } else {
            None
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