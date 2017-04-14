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
package io.greenbus.edge.dnp3.config.model

import io.greenbus.edge.data.mapping._
import io.greenbus.edge.data._

object LinkLayer {

  def read(element: Value, ctx: ReaderContext): Either[String, LinkLayer] = {
    element match {
      case data: ValueMap =>
        val isMaster = MappingLibrary.getMapField("isMaster", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val localAddress = MappingLibrary.getMapField("localAddress", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))
        val remoteAddress = MappingLibrary.getMapField("remoteAddress", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))
        val userConfirmations = MappingLibrary.getMapField("userConfirmations", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val ackTimeoutMs = MappingLibrary.getMapField("ackTimeoutMs", data).flatMap(elem => MappingLibrary.readLong(elem, ctx))
        val numRetries = MappingLibrary.getMapField("numRetries", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))

        if (isMaster.isRight && localAddress.isRight && remoteAddress.isRight && userConfirmations.isRight && ackTimeoutMs.isRight && numRetries.isRight) {
          Right(LinkLayer(isMaster.right.get, localAddress.right.get, remoteAddress.right.get, userConfirmations.right.get, ackTimeoutMs.right.get, numRetries.right.get))
        } else {
          Left(Seq(isMaster.left.toOption, localAddress.left.toOption, remoteAddress.left.toOption, userConfirmations.left.toOption, ackTimeoutMs.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("LinkLayer must be ValueMap type")
    }
  }

  def write(obj: LinkLayer): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("isMaster"), ValueBool(obj.isMaster)),
      (ValueString("localAddress"), ValueUInt32(obj.localAddress)),
      (ValueString("remoteAddress"), ValueUInt32(obj.remoteAddress)),
      (ValueString("userConfirmations"), ValueBool(obj.userConfirmations)),
      (ValueString("ackTimeoutMs"), ValueUInt64(obj.ackTimeoutMs)),
      (ValueString("numRetries"), ValueUInt32(obj.numRetries))))

    TaggedValue("LinkLayer", built)
  }
}
case class LinkLayer(isMaster: Boolean, localAddress: Int, remoteAddress: Int, userConfirmations: Boolean, ackTimeoutMs: Long, numRetries: Int)

object Master {

  def read(element: Value, ctx: ReaderContext): Either[String, Master] = {
    element match {
      case data: ValueMap =>
        val stack = MappingLibrary.getMapField("stack", data).flatMap(elem => MappingLibrary.readFieldSubStruct("stack", elem, "StackConfig", StackConfig.read, ctx))
        val masterSettings = MappingLibrary.getMapField("masterSettings", data).flatMap(elem => MappingLibrary.readFieldSubStruct("masterSettings", elem, "MasterSettings", MasterSettings.read, ctx))
        val scanList = MappingLibrary.getMapField("scanList", data).flatMap(elem => MappingLibrary.readList[Scan](elem, MappingLibrary.readTup[Scan](_, _, Scan.read), ctx))
        val unsol = MappingLibrary.getMapField("unsol", data).flatMap(elem => MappingLibrary.readFieldSubStruct("unsol", elem, "Unsol", Unsol.read, ctx))

        if (stack.isRight && masterSettings.isRight && scanList.isRight && unsol.isRight) {
          Right(Master(stack.right.get, masterSettings.right.get, scanList.right.get, unsol.right.get))
        } else {
          Left(Seq(stack.left.toOption, masterSettings.left.toOption, scanList.left.toOption, unsol.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("Master must be ValueMap type")
    }
  }

  def write(obj: Master): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("stack"), StackConfig.write(obj.stack)),
      (ValueString("masterSettings"), MasterSettings.write(obj.masterSettings)),
      (ValueString("scanList"), MappingLibrary.writeList(obj.scanList, Scan.write)),
      (ValueString("unsol"), Unsol.write(obj.unsol))))

    TaggedValue("Master", built)
  }
}
case class Master(stack: StackConfig, masterSettings: MasterSettings, scanList: Seq[Scan], unsol: Unsol)

object MasterSettings {

  def read(element: Value, ctx: ReaderContext): Either[String, MasterSettings] = {
    element match {
      case data: ValueMap =>
        val allowTimeSync = MappingLibrary.getMapField("allowTimeSync", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val taskRetryMs = MappingLibrary.getMapField("taskRetryMs", data).flatMap(elem => MappingLibrary.readLong(elem, ctx))
        val integrityPeriodMs = MappingLibrary.getMapField("integrityPeriodMs", data).flatMap(elem => MappingLibrary.readLong(elem, ctx))

        if (allowTimeSync.isRight && taskRetryMs.isRight && integrityPeriodMs.isRight) {
          Right(MasterSettings(allowTimeSync.right.get, taskRetryMs.right.get, integrityPeriodMs.right.get))
        } else {
          Left(Seq(allowTimeSync.left.toOption, taskRetryMs.left.toOption, integrityPeriodMs.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("MasterSettings must be ValueMap type")
    }
  }

  def write(obj: MasterSettings): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("allowTimeSync"), ValueBool(obj.allowTimeSync)),
      (ValueString("taskRetryMs"), ValueUInt64(obj.taskRetryMs)),
      (ValueString("integrityPeriodMs"), ValueUInt64(obj.integrityPeriodMs))))

    TaggedValue("MasterSettings", built)
  }
}
case class MasterSettings(allowTimeSync: Boolean, taskRetryMs: Long, integrityPeriodMs: Long)

object IndexRange {

  def read(element: Value, ctx: ReaderContext): Either[String, IndexRange] = {
    element match {
      case data: ValueMap =>
        val start = MappingLibrary.getMapField("start", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))
        val count = MappingLibrary.getMapField("count", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))

        if (start.isRight && count.isRight) {
          Right(IndexRange(start.right.get, count.right.get))
        } else {
          Left(Seq(start.left.toOption, count.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("IndexRange must be ValueMap type")
    }
  }

  def write(obj: IndexRange): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("start"), ValueUInt32(obj.start)),
      (ValueString("count"), ValueUInt32(obj.count))))

    TaggedValue("IndexRange", built)
  }
}
case class IndexRange(start: Int, count: Int)

object IndexSet {

  def read(element: Value, ctx: ReaderContext): Either[String, IndexSet] = {
    element match {
      case data: ValueList =>
        MappingLibrary.readList[IndexRange](data, MappingLibrary.readTup[IndexRange](_, _, IndexRange.read), ctx).map(result => IndexSet(result))
      case _ => Left("IndexSet must be ValueList type")
    }
  }
  def write(obj: IndexSet): TaggedValue = {
    val built = MappingLibrary.writeList(obj.value, IndexRange.write)

    TaggedValue("IndexSet", built)
  }
}
case class IndexSet(value: Seq[IndexRange])

object OutputModel {

  def read(element: Value, ctx: ReaderContext): Either[String, OutputModel] = {
    element match {
      case data: ValueMap =>
        val binaries = MappingLibrary.getMapField("binaries", data).flatMap(elem => MappingLibrary.readFieldSubStruct("binaries", elem, "IndexSet", IndexSet.read, ctx))
        val setpoints = MappingLibrary.getMapField("setpoints", data).flatMap(elem => MappingLibrary.readFieldSubStruct("setpoints", elem, "IndexSet", IndexSet.read, ctx))

        if (binaries.isRight && setpoints.isRight) {
          Right(OutputModel(binaries.right.get, setpoints.right.get))
        } else {
          Left(Seq(binaries.left.toOption, setpoints.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("OutputModel must be ValueMap type")
    }
  }

  def write(obj: OutputModel): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("binaries"), IndexSet.write(obj.binaries)),
      (ValueString("setpoints"), IndexSet.write(obj.setpoints))))

    TaggedValue("OutputModel", built)
  }
}
case class OutputModel(binaries: IndexSet, setpoints: IndexSet)

object InputModel {

  def read(element: Value, ctx: ReaderContext): Either[String, InputModel] = {
    element match {
      case data: ValueMap =>
        val binaryInputs = MappingLibrary.getMapField("binaryInputs", data).flatMap(elem => MappingLibrary.readFieldSubStruct("binaryInputs", elem, "IndexSet", IndexSet.read, ctx))
        val analogInputs = MappingLibrary.getMapField("analogInputs", data).flatMap(elem => MappingLibrary.readFieldSubStruct("analogInputs", elem, "IndexSet", IndexSet.read, ctx))
        val counterInputs = MappingLibrary.getMapField("counterInputs", data).flatMap(elem => MappingLibrary.readFieldSubStruct("counterInputs", elem, "IndexSet", IndexSet.read, ctx))
        val binaryOutputs = MappingLibrary.getMapField("binaryOutputs", data).flatMap(elem => MappingLibrary.readFieldSubStruct("binaryOutputs", elem, "IndexSet", IndexSet.read, ctx))
        val analogOutputs = MappingLibrary.getMapField("analogOutputs", data).flatMap(elem => MappingLibrary.readFieldSubStruct("analogOutputs", elem, "IndexSet", IndexSet.read, ctx))

        if (binaryInputs.isRight && analogInputs.isRight && counterInputs.isRight && binaryOutputs.isRight && analogOutputs.isRight) {
          Right(InputModel(binaryInputs.right.get, analogInputs.right.get, counterInputs.right.get, binaryOutputs.right.get, analogOutputs.right.get))
        } else {
          Left(Seq(binaryInputs.left.toOption, analogInputs.left.toOption, counterInputs.left.toOption, binaryOutputs.left.toOption, analogOutputs.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("InputModel must be ValueMap type")
    }
  }

  def write(obj: InputModel): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("binaryInputs"), IndexSet.write(obj.binaryInputs)),
      (ValueString("analogInputs"), IndexSet.write(obj.analogInputs)),
      (ValueString("counterInputs"), IndexSet.write(obj.counterInputs)),
      (ValueString("binaryOutputs"), IndexSet.write(obj.binaryOutputs)),
      (ValueString("analogOutputs"), IndexSet.write(obj.analogOutputs))))

    TaggedValue("InputModel", built)
  }
}
case class InputModel(binaryInputs: IndexSet, analogInputs: IndexSet, counterInputs: IndexSet, binaryOutputs: IndexSet, analogOutputs: IndexSet)

object TCPClient {

  def read(element: Value, ctx: ReaderContext): Either[String, TCPClient] = {
    element match {
      case data: ValueMap =>
        val host = MappingLibrary.getMapField("host", data).flatMap(elem => MappingLibrary.readString(elem, ctx))
        val port = MappingLibrary.getMapField("port", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))

        if (host.isRight && port.isRight) {
          Right(TCPClient(host.right.get, port.right.get))
        } else {
          Left(Seq(host.left.toOption, port.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("TCPClient must be ValueMap type")
    }
  }

  def write(obj: TCPClient): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("host"), ValueString(obj.host)),
      (ValueString("port"), ValueUInt32(obj.port))))

    TaggedValue("TCPClient", built)
  }
}
case class TCPClient(host: String, port: Int)

object Unsol {

  def read(element: Value, ctx: ReaderContext): Either[String, Unsol] = {
    element match {
      case data: ValueMap =>
        val doTask = MappingLibrary.getMapField("doTask", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enable = MappingLibrary.getMapField("enable", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enableClass1 = MappingLibrary.getMapField("enableClass1", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enableClass2 = MappingLibrary.getMapField("enableClass2", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enableClass3 = MappingLibrary.getMapField("enableClass3", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))

        if (doTask.isRight && enable.isRight && enableClass1.isRight && enableClass2.isRight && enableClass3.isRight) {
          Right(Unsol(doTask.right.get, enable.right.get, enableClass1.right.get, enableClass2.right.get, enableClass3.right.get))
        } else {
          Left(Seq(doTask.left.toOption, enable.left.toOption, enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("Unsol must be ValueMap type")
    }
  }

  def write(obj: Unsol): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("doTask"), ValueBool(obj.doTask)),
      (ValueString("enable"), ValueBool(obj.enable)),
      (ValueString("enableClass1"), ValueBool(obj.enableClass1)),
      (ValueString("enableClass2"), ValueBool(obj.enableClass2)),
      (ValueString("enableClass3"), ValueBool(obj.enableClass3))))

    TaggedValue("Unsol", built)
  }
}
case class Unsol(doTask: Boolean, enable: Boolean, enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean)

object StackConfig {

  def read(element: Value, ctx: ReaderContext): Either[String, StackConfig] = {
    element match {
      case data: ValueMap =>
        val linkLayer = MappingLibrary.getMapField("linkLayer", data).flatMap(elem => MappingLibrary.readFieldSubStruct("linkLayer", elem, "LinkLayer", LinkLayer.read, ctx))
        val appLayer = MappingLibrary.getMapField("appLayer", data).flatMap(elem => MappingLibrary.readFieldSubStruct("appLayer", elem, "AppLayer", AppLayer.read, ctx))

        if (linkLayer.isRight && appLayer.isRight) {
          Right(StackConfig(linkLayer.right.get, appLayer.right.get))
        } else {
          Left(Seq(linkLayer.left.toOption, appLayer.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("StackConfig must be ValueMap type")
    }
  }

  def write(obj: StackConfig): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("linkLayer"), LinkLayer.write(obj.linkLayer)),
      (ValueString("appLayer"), AppLayer.write(obj.appLayer))))

    TaggedValue("StackConfig", built)
  }
}
case class StackConfig(linkLayer: LinkLayer, appLayer: AppLayer)

object AppLayer {

  def read(element: Value, ctx: ReaderContext): Either[String, AppLayer] = {
    element match {
      case data: ValueMap =>
        val timeoutMs = MappingLibrary.getMapField("timeoutMs", data).flatMap(elem => MappingLibrary.readLong(elem, ctx))
        val maxFragSize = MappingLibrary.getMapField("maxFragSize", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))
        val numRetries = MappingLibrary.getMapField("numRetries", data).flatMap(elem => MappingLibrary.readInt(elem, ctx))

        if (timeoutMs.isRight && maxFragSize.isRight && numRetries.isRight) {
          Right(AppLayer(timeoutMs.right.get, maxFragSize.right.get, numRetries.right.get))
        } else {
          Left(Seq(timeoutMs.left.toOption, maxFragSize.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("AppLayer must be ValueMap type")
    }
  }

  def write(obj: AppLayer): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("timeoutMs"), ValueUInt64(obj.timeoutMs)),
      (ValueString("maxFragSize"), ValueUInt32(obj.maxFragSize)),
      (ValueString("numRetries"), ValueUInt32(obj.numRetries))))

    TaggedValue("AppLayer", built)
  }
}
case class AppLayer(timeoutMs: Long, maxFragSize: Int, numRetries: Int)

object Scan {

  def read(element: Value, ctx: ReaderContext): Either[String, Scan] = {
    element match {
      case data: ValueMap =>
        val enableClass1 = MappingLibrary.getMapField("enableClass1", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enableClass2 = MappingLibrary.getMapField("enableClass2", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val enableClass3 = MappingLibrary.getMapField("enableClass3", data).flatMap(elem => MappingLibrary.readBool(elem, ctx))
        val periodMs = MappingLibrary.getMapField("periodMs", data).flatMap(elem => MappingLibrary.readLong(elem, ctx))

        if (enableClass1.isRight && enableClass2.isRight && enableClass3.isRight && periodMs.isRight) {
          Right(Scan(enableClass1.right.get, enableClass2.right.get, enableClass3.right.get, periodMs.right.get))
        } else {
          Left(Seq(enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption, periodMs.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("Scan must be ValueMap type")
    }
  }

  def write(obj: Scan): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("enableClass1"), ValueBool(obj.enableClass1)),
      (ValueString("enableClass2"), ValueBool(obj.enableClass2)),
      (ValueString("enableClass3"), ValueBool(obj.enableClass3)),
      (ValueString("periodMs"), ValueUInt64(obj.periodMs))))

    TaggedValue("Scan", built)
  }
}
case class Scan(enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean, periodMs: Long)

object DNP3Gateway {

  def read(element: Value, ctx: ReaderContext): Either[String, DNP3Gateway] = {
    element match {
      case data: ValueMap =>
        val master = MappingLibrary.getMapField("master", data).flatMap(elem => MappingLibrary.readFieldSubStruct("master", elem, "Master", Master.read, ctx))
        val client = MappingLibrary.getMapField("client", data).flatMap(elem => MappingLibrary.readFieldSubStruct("client", elem, "TCPClient", TCPClient.read, ctx))
        val inputModel = MappingLibrary.getMapField("inputModel", data).flatMap(elem => MappingLibrary.readFieldSubStruct("inputModel", elem, "InputModel", InputModel.read, ctx))
        val outputModel = MappingLibrary.getMapField("outputModel", data).flatMap(elem => MappingLibrary.readFieldSubStruct("outputModel", elem, "OutputModel", OutputModel.read, ctx))

        if (master.isRight && client.isRight && inputModel.isRight && outputModel.isRight) {
          Right(DNP3Gateway(master.right.get, client.right.get, inputModel.right.get, outputModel.right.get))
        } else {
          Left(Seq(master.left.toOption, client.left.toOption, inputModel.left.toOption, outputModel.left.toOption).flatten.mkString(", "))
        }

      case _ => Left("DNP3Gateway must be ValueMap type")
    }
  }

  def write(obj: DNP3Gateway): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("master"), Master.write(obj.master)),
      (ValueString("client"), TCPClient.write(obj.client)),
      (ValueString("inputModel"), InputModel.write(obj.inputModel)),
      (ValueString("outputModel"), OutputModel.write(obj.outputModel))))

    TaggedValue("DNP3Gateway", built)
  }
}
case class DNP3Gateway(master: Master, client: TCPClient, inputModel: InputModel, outputModel: OutputModel)

