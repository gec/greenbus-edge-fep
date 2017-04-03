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
package io.greenbus.edge.tag.xml

import java.io.{ InputStream, OutputStream }
import javax.xml.stream._

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import io.greenbus.edge.tag._

object Writer {

  /*

  value = none | prim | tuple | seq | map

  --------------------------------

  elem = tagged_value | value
  value = none | prim | tuple | list | map
  field = tagged_field | elem
  tuple = { field }
  list = { elem }
  map = { elem, elem }

  ---------------------------------

   */

  def write(value: ValueElement, os: OutputStream): Unit = {

    val output = XMLOutputFactory.newFactory()
    val base = output.createXMLStreamWriter(os)
    val w = new IndentingXMLStreamWriter(base)
    w.writeStartDocument()

    writeElem(value, w)

    w.writeEndDocument()
    w.flush()
  }

  val basicTag = "value"

  def writeValue(value: Value, w: XMLStreamWriter, ctxName: Option[String] = None): Unit = {
    value match {
      case v: VTuple => {
        w.writeStartElement(ctxName.getOrElse("tuple"))
        v.value.foreach(f => writeField(f, w))
        w.writeEndElement()
      }
      case v: VList => {
        w.writeStartElement(ctxName.getOrElse("list"))
        v.value.foreach(elem => writeElem(elem, w))
        w.writeEndElement()
      }
      case v: VMap => {
        ???
      }
      case v: VBool => writeSimple("bool", v.value.toString, w, ctxName)
      case v: VByte => writeSimple("byte", v.value.toString, w, ctxName)
      case v: VInt32 => writeSimple("int32", v.value.toString, w, ctxName)
      case v: VUInt32 => writeSimple("uint32", v.value.toString, w, ctxName)
      case v: VInt64 => writeSimple("int64", v.value.toString, w, ctxName)
      case v: VUInt64 => writeSimple("uint64", v.value.toString, w, ctxName)
      case v: VFloat => writeSimple("float", v.value.toString, w, ctxName)
      case v: VDouble => writeSimple("double", v.value.toString, w, ctxName)
      case v: VString => writeSimple("string", v.value.toString, w, ctxName)
      case _ => throw new IllegalArgumentException(s"Type not handled: " + value)
    }
  }

  def writeElem(value: ValueElement, w: XMLStreamWriter, ctxName: Option[String] = None): Unit = {
    value match {
      case tagged: TaggedValue => {
        writeValue(tagged.value, w, Some(tagged.tag))
      }
      case untagged: Value => {
        writeValue(untagged, w, ctxName)
      }
    }
  }

  def writeSimple(typName: String, value: String, w: XMLStreamWriter, ctxName: Option[String] = None): Unit = {
    w.writeEmptyElement(ctxName.getOrElse(typName))
    w.writeAttribute("value", value)
  }

  def writeField(field: Element, w: XMLStreamWriter): Unit = {
    field match {
      case tf: TaggedField => writeElem(tf.value, w, Some(tf.name))
      case v: ValueElement => writeElem(v, w, None)
    }
  }
}

object XmlReader {
  import scala.collection.mutable

  def typeToName(t: VTBasic): String = {
    t match {
      case _: VTTuple => "tuple"
      case _: VTList => "list"
      case _: VTMap => "map"
      case VTBool => "bool"
      case VTByte => "byte"
      case VTInt32 => "int32"
      case VTUInt32 => "uint32"
      case VTInt64 => "int64"
      case VTUInt64 => "uint64"
      case VTFloat => "float"
      case VTDouble => "double"
      case VTString => "string"
      case _ => throw new IllegalArgumentException(s"Type not handled: " + t)
    }
  }

  def nameAndBasic(nodeType: VTValueElem): (String, VTBasic) = {
    nodeType match {
      case t: VTExtType => (t.tag, t.reprType)
      case t: VTBasic => (typeToName(t), t)
    }
  }

  case class ResultBuilder(typ: VType, attributes: Seq[(String, String)], sub: mutable.ArrayBuffer[Element])

  sealed trait Node {
    def resultType: VType
  }
  case class SingleNode(resultType: VType) extends Node
  case class TupleNode(resultType: VType, queue: mutable.Queue[Node]) extends Node
  case class SeqNode(resultType: VType, paramType: VTValueElem) extends Node

  def nodeForType(t: VTBasic): Node = ???

  def read(is: InputStream, rootType: VTValueElem): Element = {

    val fac = XMLInputFactory.newInstance()
    val doc = fac.createXMLStreamReader(is)

    var schemaStack = List.empty[Node]
    var resultStack = List.empty[ResultBuilder]
    var resultOpt = Option.empty[Element]

    def pushResultBuilder(resultType: VType): Unit = {
      val attributes = readAttrs(doc)
      resultStack ::= ResultBuilder(resultType, attributes, mutable.ArrayBuffer.empty[Element])
    }

    def nodeFor(basicType: VTBasic, matchType: VType): Node = {
      basicType match {
        case t: VTTuple => {
          val subNodes = mutable.Queue.empty[Node]
          t.elementTypes.foreach { fd =>
            val (_, subBasic) = nameAndBasic(fd.fieldType)
            subNodes += nodeFor(subBasic, fd)
          }
          TupleNode(matchType, subNodes)
        }
        case t: VTList => {
          SeqNode(matchType, t.paramType)
        }
        case t => SingleNode(matchType)
      }
    }

    def expandBasic(basicType: VTBasic, matchType: VTValueElem): Unit = {
      val node = nodeFor(basicType, matchType)
      schemaStack ::= node
    }

    val (name, basicType) = nameAndBasic(rootType)
    expandBasic(basicType, rootType)

    while (doc.hasNext) {
      val eventType = doc.next()

      eventType match {
        case XMLStreamConstants.START_ELEMENT => {
          //println("STACK: " + schemaStack.mkString("\n\t"))
          schemaStack.head match {
            case SingleNode(matchType) => {
              pushResultBuilder(matchType)
            }
            case n: SeqNode => {
              val (paramName, paramBasic) = nameAndBasic(n.paramType)
              expandBasic(paramBasic, n.paramType)
              pushResultBuilder(n.paramType)
            }
            case tn: TupleNode => {
              println("tuple had: " + tn.queue.toVector)
              if (tn.queue.isEmpty) {
                throw new IllegalArgumentException(s"Too many arguments for tuple type ${tn.resultType}")
              } else {
                val n = tn.queue.dequeue()
                schemaStack ::= n
                pushResultBuilder(n.resultType)
              }
            }
            case n => ???
          }
        }
        case XMLStreamConstants.END_ELEMENT => {

          def popAndPushResult(built: Element): Unit = {
            resultStack = resultStack.tail
            if (resultStack.nonEmpty) {
              resultStack.head.sub += built
            } else {
              resultOpt = Some(built)
            }
          }

          def process(nodeType: VType): Unit = {
            val result = resultStack.head
            val built = build(nodeType, result.attributes, result.sub.toVector)
            popAndPushResult(built)
            schemaStack = schemaStack.tail
          }

          schemaStack.head match {
            case SingleNode(nodeType) => {
              process(nodeType)
            }
            case SeqNode(nodeType, _) => {
              process(nodeType)
            }
            case TupleNode(nodeType, queue) => {
              if (queue.nonEmpty) {
                throw new IllegalArgumentException(s"Did not see all elements in queue for tuple: $nodeType")
              } else {
                process(nodeType)
              }
            }
          }
        }
        case _ =>
      }
    }

    resultOpt.getOrElse(throw new IllegalArgumentException("Did not produce result"))
  }

  def build(typ: VType, attrs: Seq[(String, String)], subElements: Seq[Element]): Element = {
    typ match {
      case t: VTValueElem => buildValueType(t, attrs, subElements)
      case t: VTField => {
        TaggedField(t.fieldName, buildValueType(t.fieldType, attrs, subElements))
      }
    }
  }

  def buildValueType(typ: VTValueElem, attrs: Seq[(String, String)], subElements: Seq[Element]): ValueElement = {
    typ match {
      case t: VTExtType => TaggedValue(t.tag, buildBasic(t.reprType, attrs.toMap, subElements))
      case t: VTBasic => buildBasic(t, attrs.toMap, subElements)
    }
  }

  def buildBasic(typ: VTBasic, attrs: Map[String, String], subElements: Seq[Element]): Value = {
    typ match {
      case t: VTTuple => {
        VTuple(subElements.toIndexedSeq)
      }
      case t: VTList => {
        val velems = subElements.map {
          case v: ValueElement => v
          case v => throw new IllegalArgumentException(s"Value element did not match for list $v")
        }
        VList(velems.toIndexedSeq)
      }
      case VTBool => VBool(readAttr(attrs, "value", java.lang.Boolean.parseBoolean))
      case VTByte => VByte(readAttr(attrs, "value", java.lang.Byte.parseByte))
      case VTUInt32 => VUInt32(readAttr(attrs, "value", java.lang.Integer.parseUnsignedInt))
      case VTInt32 => VInt32(readAttr(attrs, "value", java.lang.Integer.parseInt))
      case VTUInt64 => VUInt64(readAttr(attrs, "value", java.lang.Long.parseUnsignedLong))
      case VTInt64 => VInt64(readAttr(attrs, "value", java.lang.Long.parseLong))
      case VTFloat => VFloat(readAttr(attrs, "value", java.lang.Float.parseFloat))
      case VTDouble => VDouble(readAttr(attrs, "value", java.lang.Double.parseDouble))
      case VTString => VString(readAttr(attrs, "value", a => a))
      case _ => throw new IllegalArgumentException(s"Type not handled: " + typ)
    }
  }

  private def readAttr[A](attrs: Map[String, String], name: String, f: String => A): A = {
    val s = attrs.getOrElse(name, throw new IllegalArgumentException(s"Attribute value $name could not be found: $attrs"))
    try {
      f(s)
    } catch {
      case ex: Throwable =>
        throw new IllegalArgumentException(s"Attribute value $name could not be parsed: $attrs, $ex")
    }
  }

  private def readAttrs(doc: XMLStreamReader): Seq[(String, String)] = {
    val b = Vector.newBuilder[(String, String)]
    var i = 0
    val count = doc.getAttributeCount
    while (i < count) {
      b += ((doc.getAttributeLocalName(i), doc.getAttributeValue(i)))
      i += 1
    }
    b.result()
  }
}
