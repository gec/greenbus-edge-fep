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
package io.greenbus.edge.tag

import java.io.{ File, FileOutputStream, PrintWriter }

import io.greenbus.edge.util.EitherUtil

/*object TestSchema {

  val linkLayer: VTExtType = {
    VTExtType("LinkLayer", VTTuple(Vector(
      FieldDef("isMaster", VTBool),
      FieldDef("localAddress", VTUInt32),
      FieldDef("remoteAddress", VTUInt32),
      FieldDef("userConfirmations", VTBool),
      FieldDef("userConfirmations", VTUInt64),
      FieldDef("numRetries", VTUInt32))))
  }
  val appLayer: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      FieldDef("timeoutMs", VTUInt64),
      FieldDef("maxFragSize", VTUInt32),
      FieldDef("numRetries", VTUInt32))))
  }
  val stackConfig: VTExtType = {
    VTExtType("AppLayer", VTTuple(Vector(
      FieldDef("linkLayer", linkLayer),
      FieldDef("appLayer", appLayer))))
  }
  val masterSettings: VTExtType = {
    VTExtType("MasterSettings", VTTuple(Vector(
      FieldDef("allowTimeSync", VTBool),
      FieldDef("taskRetryMs", VTUInt64),
      FieldDef("integrityPeriodMs", VTUInt64))))
  }
  val scan: VTExtType = {
    VTExtType("Scan", VTTuple(Vector(
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool),
      FieldDef("periodMs", VTUInt64))))
  }
  val unsol: VTExtType = {
    VTExtType("Unsol", VTTuple(Vector(
      FieldDef("doTask", VTBool),
      FieldDef("enable", VTBool),
      FieldDef("enableClass1", VTBool),
      FieldDef("enableClass2", VTBool),
      FieldDef("enableClass3", VTBool))))
  }

  val master: VTExtType = {
    VTExtType("Master", VTTuple(Vector(
      FieldDef("stack", stackConfig),
      FieldDef("masterSettings", masterSettings),
      FieldDef("scanList", VTList(scan)),
      FieldDef("unsol", unsol))))
  }

}*/

object MappingLibrary {

  def descName(v: ValueElement): String = {
    v.getClass.getSimpleName
  }

  def toFieldMap(struct: VStruct): Map[String, ValueElement] = {
    struct.value.flatMap {
      case field: TaggedField => Some(field.name, field.value)
      case _ => None
    }.toMap
  }

  def readFieldSubStruct[A](fieldName: String, map: Map[String, ValueElement], tag: String, read: (VStruct, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, A] = {

    def matchV(elem: ValueElement): Either[String, A] = {
      elem match {
        case v: VStruct => read(v, ctx.structField(tag, fieldName))
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

  def readListField[A](fieldName: String, map: Map[String, ValueElement], read: (ValueElement, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, Seq[A]] = {
    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field '$fieldName'")
      case Some(elem) => readList(elem, read, ctx.field(fieldName))
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

  def readList[A](elem: ValueElement, readContained: (ValueElement, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, Seq[A]] = {
    elem match {
      case v: VList =>
        EitherUtil.rightSequence(v.value.map(readContained(_, ctx)))
      case _ => Left(s"${ctx.context} error: expected string value, saw: ${descName(elem)}")
    }
  }

  def readTup[A](elem: ValueElement, ctx: ReaderContext, read: (VStruct, ReaderContext) => Either[String, A]): Either[String, A] = {
    elem match {
      case v: TaggedValue => readTup(v.value, ctx, read)
      case v: VStruct => read(v, ctx)
      case _ => Left(s"${ctx.context} error: expected tuple value, saw ${descName(elem)}")
    }
  }

  def writeList[A](list: Seq[A], write: A => ValueElement): VList = {
    VList(list.toIndexedSeq.map(write))
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

  /*def write(obj: LinkLayerConfig): TaggedValue = {

    val built = VStruct(Vector(
      TaggedField("isMaster", VBool(obj.isMaster))))

    TaggedValue("LinkLayerConfig", built)
  }*/
}

object Gen {

  case class TypeDefFrag(code: String)

  sealed trait FieldTypeDef
  case class SimpleTypeDef(typ: ValueType) extends FieldTypeDef
  case class ParamTypeDef(typ: ValueType) extends FieldTypeDef
  case class TagTypeDef(tag: String) extends FieldTypeDef

  case class FieldDef(name: String, typ: FieldTypeDef)
  case class ObjDef(fields: Seq[FieldDef])

  def objDefForExtType(typ: TExt): ObjDef = {

    def singleParam(typ: ValueType): Seq[FieldDef] = {
      Seq(FieldDef("value", ParamTypeDef(typ)))
    }

    val fields = typ.reprType match {
      case struct: TStruct => {
        Seq()
      }
      case list: TList => singleParam(list)
      case map: TMap => singleParam(map)
      case union: TUnion => singleParam(union)
      case either: TEither => singleParam(either)
      case option: TOption => singleParam(option)
      case basic => Seq(FieldDef("value", SimpleTypeDef(basic)))
    }

    ObjDef(fields)
  }

  def collectObjDefs(typ: VTValueElem, seen: Map[String, ObjDef]): Map[String, ObjDef] = {
    typ match {
      case ext: TExt => {

        collectObjDefs(ext.reprType, seen)
      }
      case struct: TStruct =>
        struct.fields.foldLeft(seen) { (accum, fd) => collectObjDefs(fd.typ, accum) }
      case list: TList =>
        collectObjDefs(list.paramType, seen)
      case map: TMap =>
        collectObjDefs(map.keyType, seen) ++ collectObjDefs(map.valueType, seen)
      case union: TUnion =>
        union.unionTypes.foldLeft(seen) { (accum, t) => collectObjDefs(t, accum) }
      case either: TEither =>
        collectObjDefs(either.leftType, seen) ++ collectObjDefs(either.rightType, seen)
      case option: TOption =>
        collectObjDefs(option.paramType, seen)
      case basic => seen
    }
  }

  def collectTypes(typ: TExt, seen: Map[String, ObjDef]): Map[String, ObjDef] = {

    var collected: Map[String, ObjDef] = seen

    val objDef = typ.reprType match {
      case tup: TStruct => {
        val fieldDefs = tup.fields.map { fd =>
          val name = fd.name
          val typ = fd.typ
          val typDef = typ match {
            case t: TExt => {
              if (!collected.contains(t.tag)) {
                val added = collectTypes(t, collected)
                collected ++= added
              }
              TagTypeDef(t.tag)
            }
            case t: TList => {
              t.paramType match {
                case ext: TExt => {
                  if (!collected.contains(ext.tag)) {
                    val added = collectTypes(ext, collected)
                    collected ++= added
                  }
                }
                case _ =>
              }
              ParamTypeDef(t)
            }
            case t => SimpleTypeDef(t)
          }
          FieldDef(name, typDef)
        }
        ObjDef(fieldDefs)
      }
      /*case list: TList => {
        ObjDef(Seq(FieldDef()))

      }*/
      case other => throw new IllegalArgumentException(s"No support for $other")
    }

    collected + (typ.tag -> objDef)
  }

  def output(pkg: String, objs: Map[String, ObjDef], pw: PrintWriter): Unit = {

    pw.println(s"package $pkg")
    pw.println()
    pw.println("import io.greenbus.edge.tag._")
    pw.println()

    objs.foreach {
      case (tag, obj) => writeStatic(tag, obj, pw)
    }

    pw.println()
  }

  val utilKlass = "MappingLibrary"

  def readFuncForTypeParam(typ: ValueType): String = {
    typ match {
      //case t: VTTuple =>
      case t: TExt => s"${t.tag}.read"
      case t => readFuncForSimpleTyp(t)
    }
  }

  def signatureFor(ftd: FieldTypeDef): String = {
    ftd match {
      case td: SimpleTypeDef => fieldSignatureFor(td.typ)
      case td: ParamTypeDef => fieldSignatureFor(td.typ)
      case td: TagTypeDef => td.tag
    }
  }

  def fieldSignatureFor(typ: ValueType): String = {
    typ match {
      case TBool => "Boolean"
      case TInt32 => "Int"
      case TUInt32 => "Int"
      case TInt64 => "Long"
      case TUInt64 => "Long"
      case TFloat => "Float"
      case TDouble => "Double"
      case TString => "String"
      case t: TList => s"Seq[${fieldSignatureFor(t.paramType)}]"
      case t: TExt => t.tag
      case t => throw new IllegalArgumentException(s"Type signature unhandled: $t")
    }
  }

  def readFuncForSimpleTyp(typ: ValueType): String = {
    typ match {
      case TBool => s"$utilKlass.readBool"
      case TInt32 => s"$utilKlass.readInt"
      case TUInt32 => s"$utilKlass.readInt"
      case TInt64 => s"$utilKlass.readLong"
      case TUInt64 => s"$utilKlass.readLong"
      case TFloat => s"$utilKlass.readFloat"
      case TDouble => s"$utilKlass.readDouble"
      case TString => s"$utilKlass.readString"
      case t => throw new IllegalArgumentException(s"Simple type unhandled: $t")
    }
  }

  def writeCallFor(ftd: FieldTypeDef, paramDeref: String): String = {
    ftd match {
      case SimpleTypeDef(t) => writeFuncFor(t) + s"($paramDeref)"
      case ParamTypeDef(t) => {
        t match {
          case list: TList => {
            s"$utilKlass.writeList($paramDeref, ${writeFuncFor(list.paramType)})"
          }
          case _ => throw new IllegalArgumentException(s"Unhandled parameterized type def")
        }
      }
      case TagTypeDef(tag) => s"$tag.write($paramDeref)"
    }
  }

  def writeFuncFor(ftd: FieldTypeDef): String = {
    ftd match {
      case SimpleTypeDef(t) => writeFuncFor(t)
      case ParamTypeDef(t) => writeFuncFor(t)
      case TagTypeDef(tag) => s"$tag.write"
    }
  }

  def writeFuncFor(typ: ValueType): String = {
    typ match {
      case t: TExt => s"${t.tag}.write"
      case t =>
        typ match {
          case TBool => "VBool"
          case TInt32 => "VInt32"
          case TUInt32 => "VUInt32"
          case TInt64 => "VInt64"
          case TUInt64 => "VUInt64"
          case TFloat => "VFloat"
          case TDouble => "VDouble"
          case TString => "VString"
          case _ => throw new IllegalArgumentException(s"Type unhandled: $typ")
        }
    }
  }

  def tab(n: Int): String = Range(0, n).map(_ => "  ").mkString("")

  def writeStatic(name: String, objDef: ObjDef, pw: PrintWriter): Unit = {
    pw.println(s"object $name {")
    pw.println()

    pw.println(tab(1) + s"def read(data: VStruct, ctx: ReaderContext): Either[String, $name] = {")
    pw.println(tab(2) + s"val fieldMap = $utilKlass.toFieldMap(data)")
    pw.println()
    objDef.fields.foreach { fd =>
      val name = fd.name
      fd.typ match {
        case std: SimpleTypeDef => {
          val readFun = readFuncForSimpleTyp(std.typ)
          pw.println(tab(2) + "" + s"""val $name = $utilKlass.readField("${name}", fieldMap, $readFun, ctx)""")
        }
        case ptd: ParamTypeDef => {
          ptd.typ match {
            case typ: TList => {
              val paramRead = readFuncForTypeParam(typ.paramType)
              val paramSig = fieldSignatureFor(typ.paramType)
              pw.println(tab(2) + s"""val $name = $utilKlass.readListField[$paramSig]("$name", fieldMap, $utilKlass.readTup[$paramSig](_, _, $paramRead), ctx)""")
            }
            case other => throw new IllegalArgumentException(s"Parameterized type not handled: $other")
          }
        }
        case ttd: TagTypeDef => {
          val tagName = ttd.tag
          pw.println(tab(2) + s"""val $name = $utilKlass.readFieldSubStruct("${name}", fieldMap, "$tagName", $tagName.read, ctx)""")
        }
      }
    }
    pw.println()

    val isRightJoin = objDef.fields.map(f => f.name + ".isRight").mkString(" && ")
    pw.println(tab(2) + s"if ($isRightJoin) {")
    val rightGetJoin = objDef.fields.map(_.name + ".right.get").mkString(", ")
    pw.println(tab(3) + s"Right($name($rightGetJoin))")
    pw.println(tab(2) + "} else {")
    val leftJoin = objDef.fields.map(_.name + ".left.toOption").mkString(", ")
    pw.println(tab(3) + s"""Left(Seq($leftJoin).flatten.mkString(", "))""")
    pw.println(tab(2) + "}")
    pw.println()
    pw.println(tab(1) + "}")
    pw.println()

    pw.println(tab(1) + s"def write(obj: $name): TaggedValue = {")
    pw.println(tab(2) + "val built = VStruct(Vector(")
    val buildList = objDef.fields.map { d =>
      s"""TaggedField("${d.name}", ${writeCallFor(d.typ, s"obj.${d.name}")})"""
    }.mkString(tab(3), ",\n" + tab(3), "")
    pw.println(buildList)
    pw.println(tab(2) + "))")
    pw.println()
    pw.println(tab(2) + s"""TaggedValue("$name", built)""")
    pw.println(tab(1) + "}")

    pw.println("}")

    val fieldDescs = objDef.fields.map(fd => s"${fd.name}: ${signatureFor(fd.typ)}")

    pw.println(s"""case class $name(${fieldDescs.mkString(", ")})""")
    pw.println()
  }

}
