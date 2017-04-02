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

object TestSchema {

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

  def readList[A](elem: ValueElement, readContained: (Value, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, Seq[A]] = {
    elem match {
      case v: VList =>
        EitherUtil.rightSequence(v.value.map(readContained(_, ctx)))
      case _ => Left(s"${ctx.context} error: expected string value, saw: ${descName(elem)}")
    }
  }

  def readTup[A](elem: ValueElement, ctx: ReaderContext, read: (VTuple, ReaderContext) => Either[String, A]): Either[String, A] = {
    elem match {
      case v: VTuple => read(v, ctx)
      case _ => Left(s"${ctx.context} error: expected tuple value, saw ${descName(elem)}")
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

  def write(obj: LinkLayerConfig): TaggedValue = {

    val built = VTuple(Vector(
      TaggedField("isMaster", VBool(obj.isMaster))))

    TaggedValue("LinkLayerConfig", built)
  }
}

object Gen {

  case class TypeDefFrag(code: String)

  sealed trait FieldTypeDef
  case class SimpleTypeDef(typ: VType) extends FieldTypeDef
  case class ParamTypeDef(typ: VType) extends FieldTypeDef
  case class TagTypeDef(tag: String) extends FieldTypeDef

  case class FieldDef(name: String, typ: FieldTypeDef)
  case class ObjDef(fields: Seq[FieldDef])

  def collectTypes(typ: VTExtType, seen: Map[String, ObjDef]): Map[String, ObjDef] = {

    var collected: Map[String, ObjDef] = seen

    val objDef = typ.reprType match {
      case tup: VTTuple => {
        val fieldDefs = tup.elementTypes.map { fd =>
          val name = fd.fieldName
          val typ = fd.fieldType
          val typDef = typ match {
            case t: VTExtType => {
              if (!collected.contains(t.tag)) {
                val added = collectTypes(t, collected)
                collected ++= added
              }
              TagTypeDef(t.tag)
            }
            case t: VTList => {
              t.paramType match {
                case ext: VTExtType => {
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

  def readFuncForTypeParam(typ: VType): String = {
    typ match {
      //case t: VTTuple =>
      case t: VTExtType => s"${t.tag}.read"
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

  def fieldSignatureFor(typ: VType): String = {
    typ match {
      case VTBool => "Boolean"
      case VTInt32 => "Int"
      case VTUInt32 => "Int"
      case VTInt64 => "Long"
      case VTUInt64 => "Long"
      case VTFloat => "Float"
      case VTDouble => "Double"
      case VTString => "String"
      case t: VTList => s"Seq[${fieldSignatureFor(t.paramType)}]"
      case t: VTExtType => t.tag
      case t => throw new IllegalArgumentException(s"Type signature unhandled: $t")
    }
  }

  def readFuncForSimpleTyp(typ: VType): String = {
    typ match {
      case VTBool => s"$utilKlass.readBool"
      case VTInt32 => s"$utilKlass.readInt"
      case VTUInt32 => s"$utilKlass.readInt"
      case VTInt64 => s"$utilKlass.readLong"
      case VTUInt64 => s"$utilKlass.readLong"
      case VTFloat => s"$utilKlass.readFloat"
      case VTDouble => s"$utilKlass.readDouble"
      case VTString => s"$utilKlass.readString"
      case t => throw new IllegalArgumentException(s"Simple type unhandled: $t")
    }
  }

  def tab(n: Int): String = Range(0, n).map(_ => "  ").mkString("")
  //val tab = "  "

  def writeStatic(name: String, objDef: ObjDef, pw: PrintWriter): Unit = {
    pw.println(s"object $name {")
    pw.println()

    pw.println(tab(1) + s"def read(data: VTuple, ctx: ReaderContext): Either[String, $name] = {")
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
            case typ: VTList => {
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
    pw.println("}")

    val fieldDescs = objDef.fields.map(fd => s"${fd.name}: ${signatureFor(fd.typ)}")

    pw.println(s"""case class $name(${fieldDescs.mkString(", ")})""")
    pw.println()

  }

  def main(args: Array[String]): Unit = {
    val all = collectTypes(TestSchema.master, Map())
    println(all.mkString("\n"))
    println("")

    val fw = new PrintWriter("testfile.scala" /*new FileOutputStream(new File("testdir/testfile.scala"))*/ )
    output("io.greenbus.edge.dnp3.config.model", all, fw)
    fw.flush()
  }
}
