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

import io.greenbus.edge.tag._

object LinkLayer {

  def read(data: VStruct, ctx: ReaderContext): Either[String, LinkLayer] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val isMaster = MappingLibrary.readField("isMaster", fieldMap, MappingLibrary.readBool, ctx)
    val localAddress = MappingLibrary.readField("localAddress", fieldMap, MappingLibrary.readInt, ctx)
    val remoteAddress = MappingLibrary.readField("remoteAddress", fieldMap, MappingLibrary.readInt, ctx)
    val userConfirmations = MappingLibrary.readField("userConfirmations", fieldMap, MappingLibrary.readBool, ctx)
    val ackTimeoutMs = MappingLibrary.readField("ackTimeoutMs", fieldMap, MappingLibrary.readLong, ctx)
    val numRetries = MappingLibrary.readField("numRetries", fieldMap, MappingLibrary.readInt, ctx)

    if (isMaster.isRight && localAddress.isRight && remoteAddress.isRight && userConfirmations.isRight && ackTimeoutMs.isRight && numRetries.isRight) {
      Right(LinkLayer(isMaster.right.get, localAddress.right.get, remoteAddress.right.get, userConfirmations.right.get, ackTimeoutMs.right.get, numRetries.right.get))
    } else {
      Left(Seq(isMaster.left.toOption, localAddress.left.toOption, remoteAddress.left.toOption, userConfirmations.left.toOption, ackTimeoutMs.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: LinkLayer): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("isMaster", VBool(obj.isMaster)),
      TaggedField("localAddress", VUInt32(obj.localAddress)),
      TaggedField("remoteAddress", VUInt32(obj.remoteAddress)),
      TaggedField("userConfirmations", VBool(obj.userConfirmations)),
      TaggedField("ackTimeoutMs", VUInt64(obj.ackTimeoutMs)),
      TaggedField("numRetries", VUInt32(obj.numRetries))))

    TaggedValue("LinkLayer", built)
  }
}
case class LinkLayer(isMaster: Boolean, localAddress: Int, remoteAddress: Int, userConfirmations: Boolean, ackTimeoutMs: Long, numRetries: Int)

object Master {

  def read(data: VStruct, ctx: ReaderContext): Either[String, Master] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val stack = MappingLibrary.readFieldSubStruct("stack", fieldMap, "StackConfig", StackConfig.read, ctx)
    val masterSettings = MappingLibrary.readFieldSubStruct("masterSettings", fieldMap, "MasterSettings", MasterSettings.read, ctx)
    val scanList = MappingLibrary.readListField[Scan]("scanList", fieldMap, MappingLibrary.readTup[Scan](_, _, Scan.read), ctx)
    val unsol = MappingLibrary.readFieldSubStruct("unsol", fieldMap, "Unsol", Unsol.read, ctx)

    if (stack.isRight && masterSettings.isRight && scanList.isRight && unsol.isRight) {
      Right(Master(stack.right.get, masterSettings.right.get, scanList.right.get, unsol.right.get))
    } else {
      Left(Seq(stack.left.toOption, masterSettings.left.toOption, scanList.left.toOption, unsol.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: Master): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("stack", StackConfig.write(obj.stack)),
      TaggedField("masterSettings", MasterSettings.write(obj.masterSettings)),
      TaggedField("scanList", MappingLibrary.writeList(obj.scanList, Scan.write)),
      TaggedField("unsol", Unsol.write(obj.unsol))))

    TaggedValue("Master", built)
  }
}
case class Master(stack: StackConfig, masterSettings: MasterSettings, scanList: Seq[Scan], unsol: Unsol)

object MasterSettings {

  def read(data: VStruct, ctx: ReaderContext): Either[String, MasterSettings] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val allowTimeSync = MappingLibrary.readField("allowTimeSync", fieldMap, MappingLibrary.readBool, ctx)
    val taskRetryMs = MappingLibrary.readField("taskRetryMs", fieldMap, MappingLibrary.readLong, ctx)
    val integrityPeriodMs = MappingLibrary.readField("integrityPeriodMs", fieldMap, MappingLibrary.readLong, ctx)

    if (allowTimeSync.isRight && taskRetryMs.isRight && integrityPeriodMs.isRight) {
      Right(MasterSettings(allowTimeSync.right.get, taskRetryMs.right.get, integrityPeriodMs.right.get))
    } else {
      Left(Seq(allowTimeSync.left.toOption, taskRetryMs.left.toOption, integrityPeriodMs.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: MasterSettings): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("allowTimeSync", VBool(obj.allowTimeSync)),
      TaggedField("taskRetryMs", VUInt64(obj.taskRetryMs)),
      TaggedField("integrityPeriodMs", VUInt64(obj.integrityPeriodMs))))

    TaggedValue("MasterSettings", built)
  }
}
case class MasterSettings(allowTimeSync: Boolean, taskRetryMs: Long, integrityPeriodMs: Long)

object Unsol {

  def read(data: VStruct, ctx: ReaderContext): Either[String, Unsol] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val doTask = MappingLibrary.readField("doTask", fieldMap, MappingLibrary.readBool, ctx)
    val enable = MappingLibrary.readField("enable", fieldMap, MappingLibrary.readBool, ctx)
    val enableClass1 = MappingLibrary.readField("enableClass1", fieldMap, MappingLibrary.readBool, ctx)
    val enableClass2 = MappingLibrary.readField("enableClass2", fieldMap, MappingLibrary.readBool, ctx)
    val enableClass3 = MappingLibrary.readField("enableClass3", fieldMap, MappingLibrary.readBool, ctx)

    if (doTask.isRight && enable.isRight && enableClass1.isRight && enableClass2.isRight && enableClass3.isRight) {
      Right(Unsol(doTask.right.get, enable.right.get, enableClass1.right.get, enableClass2.right.get, enableClass3.right.get))
    } else {
      Left(Seq(doTask.left.toOption, enable.left.toOption, enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: Unsol): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("doTask", VBool(obj.doTask)),
      TaggedField("enable", VBool(obj.enable)),
      TaggedField("enableClass1", VBool(obj.enableClass1)),
      TaggedField("enableClass2", VBool(obj.enableClass2)),
      TaggedField("enableClass3", VBool(obj.enableClass3))))

    TaggedValue("Unsol", built)
  }
}
case class Unsol(doTask: Boolean, enable: Boolean, enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean)

object StackConfig {

  def read(data: VStruct, ctx: ReaderContext): Either[String, StackConfig] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val linkLayer = MappingLibrary.readFieldSubStruct("linkLayer", fieldMap, "LinkLayer", LinkLayer.read, ctx)
    val appLayer = MappingLibrary.readFieldSubStruct("appLayer", fieldMap, "AppLayer", AppLayer.read, ctx)

    if (linkLayer.isRight && appLayer.isRight) {
      Right(StackConfig(linkLayer.right.get, appLayer.right.get))
    } else {
      Left(Seq(linkLayer.left.toOption, appLayer.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: StackConfig): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("linkLayer", LinkLayer.write(obj.linkLayer)),
      TaggedField("appLayer", AppLayer.write(obj.appLayer))))

    TaggedValue("StackConfig", built)
  }
}
case class StackConfig(linkLayer: LinkLayer, appLayer: AppLayer)

object AppLayer {

  def read(data: VStruct, ctx: ReaderContext): Either[String, AppLayer] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val timeoutMs = MappingLibrary.readField("timeoutMs", fieldMap, MappingLibrary.readLong, ctx)
    val maxFragSize = MappingLibrary.readField("maxFragSize", fieldMap, MappingLibrary.readInt, ctx)
    val numRetries = MappingLibrary.readField("numRetries", fieldMap, MappingLibrary.readInt, ctx)

    if (timeoutMs.isRight && maxFragSize.isRight && numRetries.isRight) {
      Right(AppLayer(timeoutMs.right.get, maxFragSize.right.get, numRetries.right.get))
    } else {
      Left(Seq(timeoutMs.left.toOption, maxFragSize.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: AppLayer): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("timeoutMs", VUInt64(obj.timeoutMs)),
      TaggedField("maxFragSize", VUInt32(obj.maxFragSize)),
      TaggedField("numRetries", VUInt32(obj.numRetries))))

    TaggedValue("AppLayer", built)
  }
}
case class AppLayer(timeoutMs: Long, maxFragSize: Int, numRetries: Int)

object Scan {

  def read(data: VStruct, ctx: ReaderContext): Either[String, Scan] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val enableClass1 = MappingLibrary.readField("enableClass1", fieldMap, MappingLibrary.readBool, ctx)
    val enableClass2 = MappingLibrary.readField("enableClass2", fieldMap, MappingLibrary.readBool, ctx)
    val enableClass3 = MappingLibrary.readField("enableClass3", fieldMap, MappingLibrary.readBool, ctx)
    val periodMs = MappingLibrary.readField("periodMs", fieldMap, MappingLibrary.readLong, ctx)

    if (enableClass1.isRight && enableClass2.isRight && enableClass3.isRight && periodMs.isRight) {
      Right(Scan(enableClass1.right.get, enableClass2.right.get, enableClass3.right.get, periodMs.right.get))
    } else {
      Left(Seq(enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption, periodMs.left.toOption).flatten.mkString(", "))
    }

  }

  def write(obj: Scan): TaggedValue = {
    val built = VStruct(Vector(
      TaggedField("enableClass1", VBool(obj.enableClass1)),
      TaggedField("enableClass2", VBool(obj.enableClass2)),
      TaggedField("enableClass3", VBool(obj.enableClass3)),
      TaggedField("periodMs", VUInt64(obj.periodMs))))

    TaggedValue("Scan", built)
  }
}
case class Scan(enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean, periodMs: Long)

