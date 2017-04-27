package io.greenbus.edge.dnp3.config.model

import io.greenbus.edge.data.mapping._
import io.greenbus.edge.data._

object AppLayer {

  def read(element: Value, ctx: ReaderContext): Either[String, AppLayer] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type AppLayer did not recognize value type " + other)
        }
      case other => Left("Type AppLayer did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, AppLayer] = {
    val timeoutMs = MappingLibrary.getMapField("timeoutMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))
    val maxFragSize = MappingLibrary.getMapField("maxFragSize", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val numRetries = MappingLibrary.getMapField("numRetries", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))

    if (timeoutMs.isRight && maxFragSize.isRight && numRetries.isRight) {
      Right(AppLayer(timeoutMs.right.get, maxFragSize.right.get, numRetries.right.get))
    } else {
      Left(Seq(timeoutMs.left.toOption, maxFragSize.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: AppLayer): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("timeoutMs"), ValueUInt64(obj.timeoutMs)),
      (ValueString("maxFragSize"), ValueUInt32(obj.maxFragSize)),
      (ValueString("numRetries"), ValueUInt32(obj.numRetries))
    ))

    TaggedValue("AppLayer", built)
  }
}
case class AppLayer(timeoutMs: Long, maxFragSize: Int, numRetries: Int)

object Control {

  def read(element: Value, ctx: ReaderContext): Either[String, Control] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Control did not recognize value type " + other)
        }
      case other => Left("Type Control did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Control] = {
    val name = MappingLibrary.getMapField("name", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val index = MappingLibrary.getMapField("index", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val function = MappingLibrary.getMapField("function", element).flatMap(elem => MappingLibrary.readFieldSubStruct("function", elem, "FunctionType", io.greenbus.edge.dnp3.config.model.FunctionType.read, ctx))
    val controlOptions = MappingLibrary.getMapField("controlOptions", element).flatMap(elem => MappingLibrary.readFieldSubStruct("controlOptions", elem, "ControlOptions", io.greenbus.edge.dnp3.config.model.ControlOptions.read, ctx))

    if (name.isRight && index.isRight && function.isRight && controlOptions.isRight) {
      Right(Control(name.right.get, index.right.get, function.right.get, controlOptions.right.get))
    } else {
      Left(Seq(name.left.toOption, index.left.toOption, function.left.toOption, controlOptions.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Control): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("name"), ValueString(obj.name)),
      (ValueString("index"), ValueUInt32(obj.index)),
      (ValueString("function"), io.greenbus.edge.dnp3.config.model.FunctionType.write(obj.function)),
      (ValueString("controlOptions"), io.greenbus.edge.dnp3.config.model.ControlOptions.write(obj.controlOptions))
    ))

    TaggedValue("Control", built)
  }
}
case class Control(name: String, index: Int, function: io.greenbus.edge.dnp3.config.model.FunctionType, controlOptions: io.greenbus.edge.dnp3.config.model.ControlOptions)

object ControlOptions {

  def read(element: Value, ctx: ReaderContext): Either[String, ControlOptions] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type ControlOptions did not recognize value type " + other)
        }
      case other => Left("Type ControlOptions did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, ControlOptions] = {
    val controlType = MappingLibrary.getMapField("controlType", element).flatMap(elem => MappingLibrary.readFieldSubStruct("controlType", elem, "ControlType", io.greenbus.edge.dnp3.config.model.ControlType.read, ctx))
    val onTime = MappingLibrary.optMapField("onTime", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val offTime = MappingLibrary.optMapField("offTime", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))
    val count = MappingLibrary.optMapField("count", element).flatMap(elem => MappingLibrary.asOption(elem)).map(elem => MappingLibrary.readInt(elem, ctx).map(r => Some(r))).getOrElse(Right(None))

    if (controlType.isRight && onTime.isRight && offTime.isRight && count.isRight) {
      Right(ControlOptions(controlType.right.get, onTime.right.get, offTime.right.get, count.right.get))
    } else {
      Left(Seq(controlType.left.toOption, onTime.left.toOption, offTime.left.toOption, count.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: ControlOptions): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("controlType"), io.greenbus.edge.dnp3.config.model.ControlType.write(obj.controlType)),
      (ValueString("onTime"), obj.onTime.map(p => ValueUInt32(p)).getOrElse(ValueNone)),
      (ValueString("offTime"), obj.offTime.map(p => ValueUInt32(p)).getOrElse(ValueNone)),
      (ValueString("count"), obj.count.map(p => ValueUInt32(p)).getOrElse(ValueNone))
    ))

    TaggedValue("ControlOptions", built)
  }
}
case class ControlOptions(controlType: io.greenbus.edge.dnp3.config.model.ControlType, onTime: Option[Int], offTime: Option[Int], count: Option[Int])

object ControlType {

  case object PULSE extends ControlType("PULSE", 0)
  case object PULSE_CLOSE extends ControlType("PULSE_CLOSE", 1)
  case object PULSE_TRIP extends ControlType("PULSE_TRIP", 2)
  case object LATCH_ON extends ControlType("LATCH_ON", 3)
  case object LATCH_OFF extends ControlType("LATCH_OFF", 4)

  def read(element: Value, ctx: ReaderContext): Either[String, ControlType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type ControlType did not recognize value type " + other)
        }
      case other => Left("Type ControlType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, ControlType] = {
    element.toInt match {
      case 0 => Right(PULSE)
      case 1 => Right(PULSE_CLOSE)
      case 2 => Right(PULSE_TRIP)
      case 3 => Right(LATCH_ON)
      case 4 => Right(LATCH_OFF)
      case other => Left("Enum ControlType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, ControlType] = {
    element.value match {
      case "PULSE" => Right(PULSE)
      case "PULSE_CLOSE" => Right(PULSE_CLOSE)
      case "PULSE_TRIP" => Right(PULSE_TRIP)
      case "LATCH_ON" => Right(LATCH_ON)
      case "LATCH_OFF" => Right(LATCH_OFF)
      case other => Left("Enum ControlType did not recognize string value " + other)
    }
  }
  def write(obj: ControlType): TaggedValue = {
    TaggedValue("ControlType", ValueUInt32(obj.value))
  }
}
sealed abstract class ControlType(val name: String, val value: Int)

object DNP3Gateway {

  def read(element: Value, ctx: ReaderContext): Either[String, DNP3Gateway] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type DNP3Gateway did not recognize value type " + other)
        }
      case other => Left("Type DNP3Gateway did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, DNP3Gateway] = {
    val master = MappingLibrary.getMapField("master", element).flatMap(elem => MappingLibrary.readFieldSubStruct("master", elem, "Master", io.greenbus.edge.dnp3.config.model.Master.read, ctx))
    val client = MappingLibrary.getMapField("client", element).flatMap(elem => MappingLibrary.readFieldSubStruct("client", elem, "TCPClient", io.greenbus.edge.dnp3.config.model.TCPClient.read, ctx))
    val inputModel = MappingLibrary.getMapField("inputModel", element).flatMap(elem => MappingLibrary.readFieldSubStruct("inputModel", elem, "InputModel", io.greenbus.edge.dnp3.config.model.InputModel.read, ctx))
    val outputModel = MappingLibrary.getMapField("outputModel", element).flatMap(elem => MappingLibrary.readFieldSubStruct("outputModel", elem, "OutputModel", io.greenbus.edge.dnp3.config.model.OutputModel.read, ctx))
    val endpoint = MappingLibrary.getMapField("endpoint", element).flatMap(elem => MappingLibrary.readFieldSubStruct("endpoint", elem, "FrontendConfiguration", io.greenbus.edge.fep.config.model.FrontendConfiguration.read, ctx))

    if (master.isRight && client.isRight && inputModel.isRight && outputModel.isRight && endpoint.isRight) {
      Right(DNP3Gateway(master.right.get, client.right.get, inputModel.right.get, outputModel.right.get, endpoint.right.get))
    } else {
      Left(Seq(master.left.toOption, client.left.toOption, inputModel.left.toOption, outputModel.left.toOption, endpoint.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: DNP3Gateway): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("master"), io.greenbus.edge.dnp3.config.model.Master.write(obj.master)),
      (ValueString("client"), io.greenbus.edge.dnp3.config.model.TCPClient.write(obj.client)),
      (ValueString("inputModel"), io.greenbus.edge.dnp3.config.model.InputModel.write(obj.inputModel)),
      (ValueString("outputModel"), io.greenbus.edge.dnp3.config.model.OutputModel.write(obj.outputModel)),
      (ValueString("endpoint"), io.greenbus.edge.fep.config.model.FrontendConfiguration.write(obj.endpoint))
    ))

    TaggedValue("DNP3Gateway", built)
  }
}
case class DNP3Gateway(master: io.greenbus.edge.dnp3.config.model.Master, client: io.greenbus.edge.dnp3.config.model.TCPClient, inputModel: io.greenbus.edge.dnp3.config.model.InputModel, outputModel: io.greenbus.edge.dnp3.config.model.OutputModel, endpoint: io.greenbus.edge.fep.config.model.FrontendConfiguration)

object FunctionType {

  case object SelectBeforeOperate extends FunctionType("SelectBeforeOperate", 0)
  case object DirectOperate extends FunctionType("DirectOperate", 1)

  def read(element: Value, ctx: ReaderContext): Either[String, FunctionType] = {
    element match {
      case data: IntegerValue => readInteger(data, ctx)
      case data: ValueString => readString(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: IntegerValue => readInteger(data, ctx)
          case data: ValueString => readString(data, ctx)
          case other => Left("Type FunctionType did not recognize value type " + other)
        }
      case other => Left("Type FunctionType did not recognize value type " + other)
    }
  }
  def readInteger(element: IntegerValue, ctx: ReaderContext): Either[String, FunctionType] = {
    element.toInt match {
      case 0 => Right(SelectBeforeOperate)
      case 1 => Right(DirectOperate)
      case other => Left("Enum FunctionType did not recognize integer value " + other)
    }
  }
  def readString(element: ValueString, ctx: ReaderContext): Either[String, FunctionType] = {
    element.value match {
      case "SelectBeforeOperate" => Right(SelectBeforeOperate)
      case "DirectOperate" => Right(DirectOperate)
      case other => Left("Enum FunctionType did not recognize string value " + other)
    }
  }
  def write(obj: FunctionType): TaggedValue = {
    TaggedValue("FunctionType", ValueUInt32(obj.value))
  }
}
sealed abstract class FunctionType(val name: String, val value: Int)

object IndexRange {

  def read(element: Value, ctx: ReaderContext): Either[String, IndexRange] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type IndexRange did not recognize value type " + other)
        }
      case other => Left("Type IndexRange did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, IndexRange] = {
    val start = MappingLibrary.getMapField("start", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val count = MappingLibrary.getMapField("count", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))

    if (start.isRight && count.isRight) {
      Right(IndexRange(start.right.get, count.right.get))
    } else {
      Left(Seq(start.left.toOption, count.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: IndexRange): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("start"), ValueUInt32(obj.start)),
      (ValueString("count"), ValueUInt32(obj.count))
    ))

    TaggedValue("IndexRange", built)
  }
}
case class IndexRange(start: Int, count: Int)

object IndexSet {

  def read(element: Value, ctx: ReaderContext): Either[String, IndexSet] = {
    element match {
      case data: ValueList => readRepr(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueList => readRepr(data, ctx)
          case other => Left("Type IndexSet did not recognize value type " + other)
        }
      case other => Left("Type IndexSet did not recognize value type " + other)
    }
  }
  def readRepr(element: ValueList, ctx: ReaderContext): Either[String, IndexSet] = {
    MappingLibrary.readList[io.greenbus.edge.dnp3.config.model.IndexRange](element, io.greenbus.edge.dnp3.config.model.IndexRange.read, ctx).map(result => IndexSet(result))
  }
  def write(obj: IndexSet): TaggedValue = {
    val built = MappingLibrary.writeList(obj.value, io.greenbus.edge.dnp3.config.model.IndexRange.write)

    TaggedValue("IndexSet", built)
  }
}
case class IndexSet(value: Seq[io.greenbus.edge.dnp3.config.model.IndexRange])

object InputModel {

  def read(element: Value, ctx: ReaderContext): Either[String, InputModel] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type InputModel did not recognize value type " + other)
        }
      case other => Left("Type InputModel did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, InputModel] = {
    val binaryInputs = MappingLibrary.getMapField("binaryInputs", element).flatMap(elem => MappingLibrary.readFieldSubStruct("binaryInputs", elem, "IndexSet", io.greenbus.edge.dnp3.config.model.IndexSet.read, ctx))
    val analogInputs = MappingLibrary.getMapField("analogInputs", element).flatMap(elem => MappingLibrary.readFieldSubStruct("analogInputs", elem, "IndexSet", io.greenbus.edge.dnp3.config.model.IndexSet.read, ctx))
    val counterInputs = MappingLibrary.getMapField("counterInputs", element).flatMap(elem => MappingLibrary.readFieldSubStruct("counterInputs", elem, "IndexSet", io.greenbus.edge.dnp3.config.model.IndexSet.read, ctx))
    val binaryOutputs = MappingLibrary.getMapField("binaryOutputs", element).flatMap(elem => MappingLibrary.readFieldSubStruct("binaryOutputs", elem, "IndexSet", io.greenbus.edge.dnp3.config.model.IndexSet.read, ctx))
    val analogOutputs = MappingLibrary.getMapField("analogOutputs", element).flatMap(elem => MappingLibrary.readFieldSubStruct("analogOutputs", elem, "IndexSet", io.greenbus.edge.dnp3.config.model.IndexSet.read, ctx))

    if (binaryInputs.isRight && analogInputs.isRight && counterInputs.isRight && binaryOutputs.isRight && analogOutputs.isRight) {
      Right(InputModel(binaryInputs.right.get, analogInputs.right.get, counterInputs.right.get, binaryOutputs.right.get, analogOutputs.right.get))
    } else {
      Left(Seq(binaryInputs.left.toOption, analogInputs.left.toOption, counterInputs.left.toOption, binaryOutputs.left.toOption, analogOutputs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: InputModel): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("binaryInputs"), io.greenbus.edge.dnp3.config.model.IndexSet.write(obj.binaryInputs)),
      (ValueString("analogInputs"), io.greenbus.edge.dnp3.config.model.IndexSet.write(obj.analogInputs)),
      (ValueString("counterInputs"), io.greenbus.edge.dnp3.config.model.IndexSet.write(obj.counterInputs)),
      (ValueString("binaryOutputs"), io.greenbus.edge.dnp3.config.model.IndexSet.write(obj.binaryOutputs)),
      (ValueString("analogOutputs"), io.greenbus.edge.dnp3.config.model.IndexSet.write(obj.analogOutputs))
    ))

    TaggedValue("InputModel", built)
  }
}
case class InputModel(binaryInputs: io.greenbus.edge.dnp3.config.model.IndexSet, analogInputs: io.greenbus.edge.dnp3.config.model.IndexSet, counterInputs: io.greenbus.edge.dnp3.config.model.IndexSet, binaryOutputs: io.greenbus.edge.dnp3.config.model.IndexSet, analogOutputs: io.greenbus.edge.dnp3.config.model.IndexSet)

object LinkLayer {

  def read(element: Value, ctx: ReaderContext): Either[String, LinkLayer] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type LinkLayer did not recognize value type " + other)
        }
      case other => Left("Type LinkLayer did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, LinkLayer] = {
    val isMaster = MappingLibrary.getMapField("isMaster", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val localAddress = MappingLibrary.getMapField("localAddress", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val remoteAddress = MappingLibrary.getMapField("remoteAddress", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val userConfirmations = MappingLibrary.getMapField("userConfirmations", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val ackTimeoutMs = MappingLibrary.getMapField("ackTimeoutMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))
    val numRetries = MappingLibrary.getMapField("numRetries", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))

    if (isMaster.isRight && localAddress.isRight && remoteAddress.isRight && userConfirmations.isRight && ackTimeoutMs.isRight && numRetries.isRight) {
      Right(LinkLayer(isMaster.right.get, localAddress.right.get, remoteAddress.right.get, userConfirmations.right.get, ackTimeoutMs.right.get, numRetries.right.get))
    } else {
      Left(Seq(isMaster.left.toOption, localAddress.left.toOption, remoteAddress.left.toOption, userConfirmations.left.toOption, ackTimeoutMs.left.toOption, numRetries.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: LinkLayer): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("isMaster"), ValueBool(obj.isMaster)),
      (ValueString("localAddress"), ValueUInt32(obj.localAddress)),
      (ValueString("remoteAddress"), ValueUInt32(obj.remoteAddress)),
      (ValueString("userConfirmations"), ValueBool(obj.userConfirmations)),
      (ValueString("ackTimeoutMs"), ValueUInt64(obj.ackTimeoutMs)),
      (ValueString("numRetries"), ValueUInt32(obj.numRetries))
    ))

    TaggedValue("LinkLayer", built)
  }
}
case class LinkLayer(isMaster: Boolean, localAddress: Int, remoteAddress: Int, userConfirmations: Boolean, ackTimeoutMs: Long, numRetries: Int)

object Master {

  def read(element: Value, ctx: ReaderContext): Either[String, Master] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Master did not recognize value type " + other)
        }
      case other => Left("Type Master did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Master] = {
    val stack = MappingLibrary.getMapField("stack", element).flatMap(elem => MappingLibrary.readFieldSubStruct("stack", elem, "StackConfig", io.greenbus.edge.dnp3.config.model.StackConfig.read, ctx))
    val masterSettings = MappingLibrary.getMapField("masterSettings", element).flatMap(elem => MappingLibrary.readFieldSubStruct("masterSettings", elem, "MasterSettings", io.greenbus.edge.dnp3.config.model.MasterSettings.read, ctx))
    val scanList = MappingLibrary.getMapField("scanList", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.dnp3.config.model.Scan](elem, io.greenbus.edge.dnp3.config.model.Scan.read, ctx))
    val unsol = MappingLibrary.getMapField("unsol", element).flatMap(elem => MappingLibrary.readFieldSubStruct("unsol", elem, "Unsol", io.greenbus.edge.dnp3.config.model.Unsol.read, ctx))

    if (stack.isRight && masterSettings.isRight && scanList.isRight && unsol.isRight) {
      Right(Master(stack.right.get, masterSettings.right.get, scanList.right.get, unsol.right.get))
    } else {
      Left(Seq(stack.left.toOption, masterSettings.left.toOption, scanList.left.toOption, unsol.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Master): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("stack"), io.greenbus.edge.dnp3.config.model.StackConfig.write(obj.stack)),
      (ValueString("masterSettings"), io.greenbus.edge.dnp3.config.model.MasterSettings.write(obj.masterSettings)),
      (ValueString("scanList"), MappingLibrary.writeList(obj.scanList, io.greenbus.edge.dnp3.config.model.Scan.write)),
      (ValueString("unsol"), io.greenbus.edge.dnp3.config.model.Unsol.write(obj.unsol))
    ))

    TaggedValue("Master", built)
  }
}
case class Master(stack: io.greenbus.edge.dnp3.config.model.StackConfig, masterSettings: io.greenbus.edge.dnp3.config.model.MasterSettings, scanList: Seq[io.greenbus.edge.dnp3.config.model.Scan], unsol: io.greenbus.edge.dnp3.config.model.Unsol)

object MasterSettings {

  def read(element: Value, ctx: ReaderContext): Either[String, MasterSettings] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type MasterSettings did not recognize value type " + other)
        }
      case other => Left("Type MasterSettings did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, MasterSettings] = {
    val allowTimeSync = MappingLibrary.getMapField("allowTimeSync", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val taskRetryMs = MappingLibrary.getMapField("taskRetryMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))
    val integrityPeriodMs = MappingLibrary.getMapField("integrityPeriodMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))

    if (allowTimeSync.isRight && taskRetryMs.isRight && integrityPeriodMs.isRight) {
      Right(MasterSettings(allowTimeSync.right.get, taskRetryMs.right.get, integrityPeriodMs.right.get))
    } else {
      Left(Seq(allowTimeSync.left.toOption, taskRetryMs.left.toOption, integrityPeriodMs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: MasterSettings): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("allowTimeSync"), ValueBool(obj.allowTimeSync)),
      (ValueString("taskRetryMs"), ValueUInt64(obj.taskRetryMs)),
      (ValueString("integrityPeriodMs"), ValueUInt64(obj.integrityPeriodMs))
    ))

    TaggedValue("MasterSettings", built)
  }
}
case class MasterSettings(allowTimeSync: Boolean, taskRetryMs: Long, integrityPeriodMs: Long)

object OutputModel {

  def read(element: Value, ctx: ReaderContext): Either[String, OutputModel] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type OutputModel did not recognize value type " + other)
        }
      case other => Left("Type OutputModel did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, OutputModel] = {
    val controls = MappingLibrary.getMapField("controls", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.dnp3.config.model.Control](elem, io.greenbus.edge.dnp3.config.model.Control.read, ctx))
    val setpoints = MappingLibrary.getMapField("setpoints", element).flatMap(elem => MappingLibrary.readList[io.greenbus.edge.dnp3.config.model.Setpoint](elem, io.greenbus.edge.dnp3.config.model.Setpoint.read, ctx))

    if (controls.isRight && setpoints.isRight) {
      Right(OutputModel(controls.right.get, setpoints.right.get))
    } else {
      Left(Seq(controls.left.toOption, setpoints.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: OutputModel): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("controls"), MappingLibrary.writeList(obj.controls, io.greenbus.edge.dnp3.config.model.Control.write)),
      (ValueString("setpoints"), MappingLibrary.writeList(obj.setpoints, io.greenbus.edge.dnp3.config.model.Setpoint.write))
    ))

    TaggedValue("OutputModel", built)
  }
}
case class OutputModel(controls: Seq[io.greenbus.edge.dnp3.config.model.Control], setpoints: Seq[io.greenbus.edge.dnp3.config.model.Setpoint])

object Scan {

  def read(element: Value, ctx: ReaderContext): Either[String, Scan] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Scan did not recognize value type " + other)
        }
      case other => Left("Type Scan did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Scan] = {
    val enableClass1 = MappingLibrary.getMapField("enableClass1", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enableClass2 = MappingLibrary.getMapField("enableClass2", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enableClass3 = MappingLibrary.getMapField("enableClass3", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val periodMs = MappingLibrary.getMapField("periodMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))

    if (enableClass1.isRight && enableClass2.isRight && enableClass3.isRight && periodMs.isRight) {
      Right(Scan(enableClass1.right.get, enableClass2.right.get, enableClass3.right.get, periodMs.right.get))
    } else {
      Left(Seq(enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption, periodMs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Scan): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("enableClass1"), ValueBool(obj.enableClass1)),
      (ValueString("enableClass2"), ValueBool(obj.enableClass2)),
      (ValueString("enableClass3"), ValueBool(obj.enableClass3)),
      (ValueString("periodMs"), ValueUInt64(obj.periodMs))
    ))

    TaggedValue("Scan", built)
  }
}
case class Scan(enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean, periodMs: Long)

object Setpoint {

  def read(element: Value, ctx: ReaderContext): Either[String, Setpoint] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Setpoint did not recognize value type " + other)
        }
      case other => Left("Type Setpoint did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Setpoint] = {
    val name = MappingLibrary.getMapField("name", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val index = MappingLibrary.getMapField("index", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val function = MappingLibrary.getMapField("function", element).flatMap(elem => MappingLibrary.readFieldSubStruct("function", elem, "FunctionType", io.greenbus.edge.dnp3.config.model.FunctionType.read, ctx))

    if (name.isRight && index.isRight && function.isRight) {
      Right(Setpoint(name.right.get, index.right.get, function.right.get))
    } else {
      Left(Seq(name.left.toOption, index.left.toOption, function.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Setpoint): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("name"), ValueString(obj.name)),
      (ValueString("index"), ValueUInt32(obj.index)),
      (ValueString("function"), io.greenbus.edge.dnp3.config.model.FunctionType.write(obj.function))
    ))

    TaggedValue("Setpoint", built)
  }
}
case class Setpoint(name: String, index: Int, function: io.greenbus.edge.dnp3.config.model.FunctionType)

object StackConfig {

  def read(element: Value, ctx: ReaderContext): Either[String, StackConfig] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type StackConfig did not recognize value type " + other)
        }
      case other => Left("Type StackConfig did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, StackConfig] = {
    val linkLayer = MappingLibrary.getMapField("linkLayer", element).flatMap(elem => MappingLibrary.readFieldSubStruct("linkLayer", elem, "LinkLayer", io.greenbus.edge.dnp3.config.model.LinkLayer.read, ctx))
    val appLayer = MappingLibrary.getMapField("appLayer", element).flatMap(elem => MappingLibrary.readFieldSubStruct("appLayer", elem, "AppLayer", io.greenbus.edge.dnp3.config.model.AppLayer.read, ctx))

    if (linkLayer.isRight && appLayer.isRight) {
      Right(StackConfig(linkLayer.right.get, appLayer.right.get))
    } else {
      Left(Seq(linkLayer.left.toOption, appLayer.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: StackConfig): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("linkLayer"), io.greenbus.edge.dnp3.config.model.LinkLayer.write(obj.linkLayer)),
      (ValueString("appLayer"), io.greenbus.edge.dnp3.config.model.AppLayer.write(obj.appLayer))
    ))

    TaggedValue("StackConfig", built)
  }
}
case class StackConfig(linkLayer: io.greenbus.edge.dnp3.config.model.LinkLayer, appLayer: io.greenbus.edge.dnp3.config.model.AppLayer)

object TCPClient {

  def read(element: Value, ctx: ReaderContext): Either[String, TCPClient] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type TCPClient did not recognize value type " + other)
        }
      case other => Left("Type TCPClient did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, TCPClient] = {
    val host = MappingLibrary.getMapField("host", element).flatMap(elem => MappingLibrary.readString(elem, ctx))
    val port = MappingLibrary.getMapField("port", element).flatMap(elem => MappingLibrary.readInt(elem, ctx))
    val retryMs = MappingLibrary.getMapField("retryMs", element).flatMap(elem => MappingLibrary.readLong(elem, ctx))

    if (host.isRight && port.isRight && retryMs.isRight) {
      Right(TCPClient(host.right.get, port.right.get, retryMs.right.get))
    } else {
      Left(Seq(host.left.toOption, port.left.toOption, retryMs.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: TCPClient): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("host"), ValueString(obj.host)),
      (ValueString("port"), ValueUInt32(obj.port)),
      (ValueString("retryMs"), ValueUInt64(obj.retryMs))
    ))

    TaggedValue("TCPClient", built)
  }
}
case class TCPClient(host: String, port: Int, retryMs: Long)

object Unsol {

  def read(element: Value, ctx: ReaderContext): Either[String, Unsol] = {
    element match {
      case data: ValueMap => readMap(data, ctx)
      case tagged: TaggedValue =>
        tagged.value match {
          case data: ValueMap => readMap(data, ctx)
          case other => Left("Type Unsol did not recognize value type " + other)
        }
      case other => Left("Type Unsol did not recognize value type " + other)
    }
  }
  def readMap(element: ValueMap, ctx: ReaderContext): Either[String, Unsol] = {
    val doTask = MappingLibrary.getMapField("doTask", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enable = MappingLibrary.getMapField("enable", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enableClass1 = MappingLibrary.getMapField("enableClass1", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enableClass2 = MappingLibrary.getMapField("enableClass2", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))
    val enableClass3 = MappingLibrary.getMapField("enableClass3", element).flatMap(elem => MappingLibrary.readBool(elem, ctx))

    if (doTask.isRight && enable.isRight && enableClass1.isRight && enableClass2.isRight && enableClass3.isRight) {
      Right(Unsol(doTask.right.get, enable.right.get, enableClass1.right.get, enableClass2.right.get, enableClass3.right.get))
    } else {
      Left(Seq(doTask.left.toOption, enable.left.toOption, enableClass1.left.toOption, enableClass2.left.toOption, enableClass3.left.toOption).flatten.mkString(", "))
    }
  }

  def write(obj: Unsol): TaggedValue = {
    val built = ValueMap(Map(
      (ValueString("doTask"), ValueBool(obj.doTask)),
      (ValueString("enable"), ValueBool(obj.enable)),
      (ValueString("enableClass1"), ValueBool(obj.enableClass1)),
      (ValueString("enableClass2"), ValueBool(obj.enableClass2)),
      (ValueString("enableClass3"), ValueBool(obj.enableClass3))
    ))

    TaggedValue("Unsol", built)
  }
}
case class Unsol(doTask: Boolean, enable: Boolean, enableClass1: Boolean, enableClass2: Boolean, enableClass3: Boolean)


