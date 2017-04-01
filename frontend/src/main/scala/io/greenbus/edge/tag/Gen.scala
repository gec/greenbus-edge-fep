package io.greenbus.edge.tag

object TestSchema {

  val linkLayer: VTExtType = {
    VTExtType("LinkLayer", VTTuple(Vector(
      FieldDef("isMaster", VTBool),
      FieldDef("localAddress", VTUInt32),
      FieldDef("remoteAddress", VTUInt32),
      FieldDef("userConfirmations", VTBool),
      FieldDef("userConfirmations", VTUInt64),
      FieldDef("numRetries", VTUInt32)
    )))
  }
  val appLayer: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      FieldDef("timeoutMs", VTUInt64),
      FieldDef("maxFragSize", VTUInt32),
      FieldDef("numRetries", VTUInt32)
    )))
  }
  val stackConfig: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      FieldDef("linkLayer", linkLayer),
      FieldDef("appLayer", appLayer)
    )))
  }
  val masterSettings: VTExtType = {
    VTExtType("MasterSettings", VTTuple(Vector(
      FieldDef("allowTimeSync", VTBool),
      FieldDef("taskRetryMs", VTUInt64),
      FieldDef("integrityPeriodMs", VTUInt64)
    )))
  }
  val scan: VTExtType = {
    VTExtType("Scan", VTTuple(Vector(
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool),
      FieldDef("periodMs", VTUInt64)
    )))
  }
  val unsol: VTExtType = {
    VTExtType("Unsol", VTTuple(Vector(
      FieldDef("doTask", VTBool),
      FieldDef("enable", VTBool),
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool)
    )))
  }

  val master: VTExtType = {
    VTExtType("Master", VTTuple(Vector(
      FieldDef("stack", stackConfig),
      FieldDef("masterSettings", masterSettings),
      FieldDef("scanList", VTList(scan)),
      FieldDef("unsol", unsol)
    )))
  }

  /*



   */

}


trait ReaderContext {
  def context: String
  def field(name: String): ReaderContext
  def structField(tag: String, name: String): ReaderContext
}

object MappingLibrary {

  def descName(v: ValueElement): String = {
    v.getClass.getSimpleName
  }

  def toFieldMap(struct: VTuple): Map[String, ValueElement] = {
    struct.value.flatMap {
      case field: TaggedField => Some(field.name, field.value)
      case _ => None
    }.toMap
  }

  def readFieldSubStruct[A](fieldName: String, map: Map[String, ValueElement], tag: String, read: (VTuple, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, A] = {

    def matchV(elem: ValueElement): Either[String, A] = {
      elem match {
        case v: VTuple => read(v, ctx.structField(tag, fieldName))
        case _ => Left(s"${ctx.context} error: expected boolean value, saw: ${descName(elem)}")
      }
    }

    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field '$fieldName'")
      case Some(elem) =>
        elem match {
          case v: TaggedValue => matchV(v.value)
          case other => matchV(other)
        }
    }
  }

  def readField[A](fieldName: String, map: Map[String, ValueElement], read: (ValueElement, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, A] = {
    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field '$fieldName'")
      case Some(elem) => read(elem, ctx.field(fieldName))
    }
  }

  def readBool(elem: ValueElement, ctx: ReaderContext): Either[String, Boolean] = {
    elem match {
      case v: VBool => Right(v.value)
      case _ => Left(s"${ctx.context} error: expected boolean value, saw: ${descName(elem)}")
    }
  }
  def readByte(elem: ValueElement, ctx: ReaderContext): Either[String, Byte] = {
    elem match {
      case v: VByte => Right(v.value)
      case _ => Left(s"${ctx.context} error: expected byte value, saw: ${descName(elem)}")
    }
  }
  def readInt(elem: ValueElement, ctx: ReaderContext): Either[String, Int] = {
    elem match {
      case v: IntegerValue => Right(v.toInt)
      case _ => Left(s"${ctx.context} error: expected integer value, saw: ${descName(elem)}")
    }
  }
  def readLong(elem: ValueElement, ctx: ReaderContext): Either[String, Long] = {
    elem match {
      case v: IntegerValue => Right(v.toLong)
      case _ => Left(s"${ctx.context} error: expected long value, saw: ${descName(elem)}")
    }
  }
  def readFloat(elem: ValueElement, ctx: ReaderContext): Either[String, Float] = {
    elem match {
      case v: FloatingPointValue => Right(v.toFloat)
      case _ => Left(s"${ctx.context} error: expected float value, saw: ${descName(elem)}")
    }
  }
  def readDouble(elem: ValueElement, ctx: ReaderContext): Either[String, Double] = {
    elem match {
      case v: FloatingPointValue => Right(v.toDouble)
      case _ => Left(s"${ctx.context} error: expected double value, saw: ${descName(elem)}")
    }
  }
  def readString(elem: ValueElement, ctx: ReaderContext): Either[String, String] = {
    elem match {
      case v: VString => Right(v.toString)
      case _ => Left(s"${ctx.context} error: expected string value, saw: ${descName(elem)}")
    }
  }
}

object TestGenerated {

  case class LinkLayerConfig(
                              isMaster: Boolean,
                              localAddress: Int,
                              remoteAddress: Int,
                              userConfirmations: Boolean,
                              ackTimeoutMs: Int,
                              numRetries: Int)


  def readLinkLayerConfig(data: VTuple, ctx: ReaderContext): Either[String, LinkLayerConfig] = {
    val fieldMap = MappingLibrary.toFieldMap(data)

    val isMaster = MappingLibrary.readField("isMaster", fieldMap, MappingLibrary.readBool, ctx)
    val localAddress = MappingLibrary.readField("localAddress", fieldMap, MappingLibrary.readInt, ctx)

    if (isMaster.isRight && localAddress.isRight) {
      LinkLayerConfig(
        isMaster.right.get,
        localAddress.right.get
      )
    } else {
      Left(Seq(isMaster.left.toOption, localAddress.left.toOption).flatten.mkString(", "))
    }

    /*val fieldMap = ValueUtils.toFieldMap(data)

    for {
      isMaster <- readField(LinkConfig.isMaster.fieldName, fieldMap, readBool, ctx)
      localAddress <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
      remoteAddress <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
      userConfirmations <- readField(LinkConfig.isMaster.fieldName, fieldMap, readBool, ctx)
      ackTimeoutMs <- readField(LinkConfig.isMaster.fieldName, fieldMap, readLong, ctx)
      numRetries <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
    } yield {
      val cfg = new LinkConfig(isMaster, userConfirmations)
      cfg.setLocalAddr(localAddress)
      cfg.setRemoteAddr(remoteAddress)
      cfg.setTimeout(ackTimeoutMs)
      cfg.setNumRetry(numRetries)
      cfg
    }*/
  }
}

object Gen {

  case class TypeDefFrag(code: String)

  def gen(typ: VTExtType) = {

  }

}
