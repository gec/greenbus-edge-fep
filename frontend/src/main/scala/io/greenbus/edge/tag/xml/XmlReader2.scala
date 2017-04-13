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

import java.io.InputStream
import javax.xml.stream.XMLInputFactory

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.tag._

import scala.collection.mutable

object Node extends LazyLogging {

  def tryParse[A](value: String, f: String => A): Option[A] = {
    try {
      Some(f(value))
    } catch {
      case ex: Throwable =>
        logger.debug(s"Problem parsing value: $ex")
        None
    }
  }

  def termParse[A](parse: String => A, build: A => Value): String => Option[Value] = {
    s: String => { tryParse[A](s, parse).map(build) }
  }
  def term[A](parse: String => A, build: A => Value, tagOpt: Option[String]): Node = {
    new TerminalNode(termParse(parse, build), tagOpt)
  }

  def simpleNodeFor(typ: VTValueElem, tagOpt: Option[String]): Node = {
    typ match {
      case TByte => term(java.lang.Byte.parseByte, VByte, tagOpt)
      case TBool => term(java.lang.Boolean.parseBoolean, VBool, tagOpt)
      case TInt32 => term(java.lang.Integer.parseInt, VInt32, tagOpt)
      case TUInt32 => term(java.lang.Integer.parseInt, VUInt32, tagOpt)
      case TInt64 => term(java.lang.Long.parseLong, VInt64, tagOpt)
      case TUInt64 => term(java.lang.Long.parseLong, VUInt64, tagOpt)
      case TFloat => term(java.lang.Float.parseFloat, VFloat, tagOpt)
      case TDouble => term(java.lang.Double.parseDouble, VDouble, tagOpt)
      case TString => term(s => s, VString, tagOpt)
      case t: TList => new ListNode(t, tagOpt)
      case _ => throw new IllegalArgumentException(s"Type unhandled: " + typ)
    }
  }

  def nodeFor(typ: VTValueElem): Node = {
    typ match {
      case t: TExt => {
        t.reprType match {
          case extType: TStruct => new StructNode(t.tag, extType)
          case other => simpleNodeFor(other, Some(t.tag))
        }
      }
      case t => simpleNodeFor(typ, None)
    }
  }

}
trait Node {
  def setText(content: String): Unit
  def onPop(subElemOpt: Option[ValueElement]): Unit
  def onPush(name: String): Node
  def result(): Option[ValueElement]
}

class NullNode extends Node {
  def setText(content: String): Unit = {}

  def onPush(name: String): Node = new NullNode
  def onPop(subElemOpt: Option[ValueElement]): Unit = {}

  def result(): Option[ValueElement] = {
    None
  }
}

class TerminalNode(parser: String => Option[Value], tagOpt: Option[String]) extends Node {

  private var textOpt = Option.empty[String]

  def setText(content: String): Unit = {
    textOpt = Some(content)
  }

  def onPush(name: String): Node = new NullNode
  def onPop(subElemOpt: Option[ValueElement]): Unit = {}

  def result(): Option[ValueElement] = {
    textOpt.flatMap(parser).map { elem =>
      tagOpt match {
        case None => elem
        case Some(tag) => TaggedValue(tag, elem)
      }
    }
  }
}

class ListNode(typ: TList, tagOpt: Option[String]) extends Node {
  private val elems = mutable.ArrayBuffer.empty[ValueElement]

  def setText(content: String): Unit = {}

  def onPush(name: String): Node = {
    Node.nodeFor(typ.paramType)
  }

  def onPop(subElemOpt: Option[ValueElement]): Unit = {
    subElemOpt.foreach { elems += _ }
  }

  def result(): Option[ValueElement] = {
    tagOpt match {
      case None => Some(VList(elems.toVector))
      case Some(tag) => Some(TaggedValue(tag, VList(elems.toVector)))
    }
  }
}

class MapNode(typ: TMap) extends Node {
  def setText(content: String): Unit = ???

  def onPop(subElemOpt: Option[ValueElement]): Unit = ???

  def onPush(name: String): Node = ???

  def result(): Option[ValueElement] = ???
}

object StructNode {
  def fieldIsOptional(sfd: StructFieldDef): Boolean = {
    sfd.typ match {
      case _: TOption => true
      case _: TList => true
      case _: TMap => true
      case _ => false
    }
  }
}
class StructNode(typeTag: String, vt: TStruct) extends Node {
  private val fieldMap: Map[String, StructFieldDef] = vt.fields.map { sfd =>
    /*val name = sfd.typ match {
      case t: TExt => t.tag
      case _ => sfd.name
    }*/
    (sfd.name, sfd)
  }.toMap

  private var builtFields = Map.empty[String, ValueElement]

  private var currentField = Option.empty[String]

  def setText(content: String): Unit = {

  }

  def onPush(name: String): Node = {
    fieldMap.get(name) match {
      case None =>
        new NullNode
      case Some(sfd) => {
        currentField = Some(sfd.name)
        Node.nodeFor(sfd.typ)
      }
    }
  }

  def onPop(subElemOpt: Option[ValueElement]): Unit = {
    subElemOpt.foreach { subElem =>
      currentField.foreach { fieldName =>
        builtFields += (fieldName -> subElem)
      }
    }
    currentField = None
  }

  def result(): Option[ValueElement] = {
    val kvs: Seq[(VString, ValueElement)] = vt.fields.flatMap { sfd =>
      builtFields.get(sfd.name) match {
        case None =>
          if (!StructNode.fieldIsOptional(sfd)) {
            throw new IllegalArgumentException(s"Could not find required field ${sfd.name} for $typeTag")
          } else {
            None
          }
        case Some(elem) => {
          Some((VString(sfd.name), elem))
        }
      }
    }

    Some(TaggedValue(typeTag, VMap(kvs.toMap)))
  }
}

class RootNode(rootType: VTValueElem) extends Node {

  private var resultOpt = Option.empty[ValueElement]
  private var pushed = false
  private var popped = false

  def setText(content: String): Unit = {}

  def onPush(name: String): Node = {
    if (!pushed) {
      pushed = true
      Node.nodeFor(rootType)
    } else {
      new NullNode
    }
  }

  def onPop(subElemOpt: Option[ValueElement]): Unit = {
    if (!popped) {
      popped = true
      resultOpt = subElemOpt
    }
  }

  def result(): Option[ValueElement] = {
    resultOpt
  }
}

object XmlReader2 {

  class ResultBuilder(name: String, attributes: Seq[(String, String)], var text: Option[String], sub: mutable.ArrayBuffer[Element])

  def read(is: InputStream, rootType: VTValueElem): Option[ValueElement] = {
    val fac = XMLInputFactory.newInstance()
    val reader = fac.createXMLEventReader(is)

    var nodeStack = List.empty[Node]

    val root = new RootNode(rootType)
    nodeStack ::= root

    while (reader.hasNext) {
      val event = reader.nextEvent()

      if (event.isStartElement) {

        val elem = event.asStartElement()

        nodeStack.headOption.foreach { head =>
          val pushed = head.onPush(elem.getName.getLocalPart)
          nodeStack ::= pushed
        }

      } else if (event.isCharacters) {

        val trimmed = event.asCharacters().getData.trim
        if (trimmed.nonEmpty) {
          nodeStack.headOption.foreach { head =>
            head.setText(trimmed)
          }
        }

      } else if (event.isEndElement) {

        if (nodeStack.nonEmpty) {
          val current = nodeStack.head
          nodeStack = nodeStack.tail

          if (nodeStack.nonEmpty) {
            val prev = nodeStack.head
            prev.onPop(current.result())
          } else {
          }
        }
      }

    }

    root.result()
  }
}
