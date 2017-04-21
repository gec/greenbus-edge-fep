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

import io.greenbus.edge.thread.CallMarshaller
import org.totalgrid.dnp3._

trait DNP3ControlHandle {
  def issueControl(index: Int,
    cmdType: ControlCode,
    count: Option[Short],
    onTime: Option[Long],
    offTime: Option[Long],
    directOperate: Boolean,
    result: CommandResponse => Unit): Unit

  def issueSetpoint(index: Int, value: DNPSetpointValue, directOperate: Boolean, result: CommandResponse => Unit): Unit
}

class DNP3ControlHandleImpl(eventThread: CallMarshaller, cmdAcceptor: ICommandAcceptor) extends IResponseAcceptor with DNP3ControlHandle {

  private var sequence: Int = 0
  private var map = Map.empty[Int, CommandResponse => Unit]

  private def registerSeq(respond: CommandResponse => Unit): Int = {
    val seq = sequence
    sequence += 1
    map = map + (seq -> respond)
    seq
  }

  def issueControl(index: Int,
    cmdType: ControlCode,
    count: Option[Short],
    onTime: Option[Long],
    offTime: Option[Long],
    directOperate: Boolean,
    result: CommandResponse => Unit): Unit = {

    eventThread.marshal {
      val seq = registerSeq(result)
      val bo = new BinaryOutput(cmdType)
      count.foreach(bo.setMCount)
      onTime.foreach(bo.setMOnTimeMS)
      offTime.foreach(bo.setMOffTimeMS)
      cmdAcceptor.AcceptCommand(bo, index, seq, this, directOperate)
    }
  }

  def issueSetpoint(index: Int, value: DNPSetpointValue, directOperate: Boolean, result: CommandResponse => Unit): Unit = {
    eventThread.marshal {
      val seq = registerSeq(result)
      val sp = value match {
        case IntegerSetpointValue(v) => new Setpoint(v)
        case DoubleSetpointValue(v) => new Setpoint(v)
      }

      cmdAcceptor.AcceptCommand(sp, index, seq, this, directOperate)
    }
  }

  override def AcceptResponse(arResponse: CommandResponse, aSequence: Int): Unit = {
    eventThread.marshal {
      map.get(aSequence).foreach { onResp => onResp(arResponse) }
      map -= aSequence
    }
  }
}