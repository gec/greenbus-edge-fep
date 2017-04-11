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

import io.greenbus.edge.tag._

import scala.collection.mutable

/*



 */

/*
trait Stack[A] {
  def push(obj: A): Unit
  def pop(): Unit
}


  sealed trait Node {
    def resultType: ValueType
  }
  case class SingleNode(resultType: ValueType) extends Node
  case class TupleNode(resultType: ValueType, queue: mutable.Queue[Node]) extends Node
  case class SeqNode(resultType: ValueType, paramType: VTValueElem) extends Node


  struct
  map
  list
  union
  simple


*/

trait Node {
  //def setName(name: String): Unit
  def setText(content: String): Unit
  def onPush(name: String): Node
  def result(): Element
}

class NullNode extends Node {
  def setText(content: String): Unit = {}

  def onPush(name: String): Node = {}

  def result(): Element = {}
}

/*object FieldHolder {
  def build(sfd: StructFieldDef): FieldHolder = {
    val isOptional = sfd.typ match {
      case _: TOption => true
      case _: TList => true
      case _: TMap => true
      case _ => false
    }

    FieldHolder(isOptional, sfd)
  }
}
case class FieldHolder(isOptional: Boolean, structFieldDef: StructFieldDef)*/

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
class StructNode(typName: String, vt: TStruct) extends Node {
  private val fieldMap: Map[String, StructFieldDef] = vt.fields.map(sfd => (sfd.name, sfd)).toMap
  private var builtFields = Map.empty[String, ValueElement]

  private var currentField = Option.empty[String]

  def setText(content: String): Unit = {

  }

  def onPush(name: String): Node = {
    fieldMap.get(name)
  }

  def onPop(subElemOpt: Option[ValueElement]): Unit = {

  }

  def result(): Option[ValueElement] = {
    val kvs: Seq[(VString, ValueElement)] = vt.fields.flatMap { sfd =>
      builtFields.get(sfd.name) match {
        case None =>
          if (StructNode.fieldIsOptional(sfd)) {
            throw new IllegalArgumentException(s"Could not find required field ${sfd.name} for $typName")
          } else {
            None
          }
        case Some(elem) => {
          Some((VString(sfd.name), elem))
        }
      }
    }

    Some(TaggedValue(typName, VMap(kvs.toMap)))
  }
}

object XmlReader2 {

  /*sealed trait Node {
    def resultType: ValueType
  }
  case class SingleNode(resultType: ValueType) extends Node
  case class TupleNode(resultType: ValueType, queue: mutable.Queue[Node]) extends Node
  case class SeqNode(resultType: ValueType, paramType: VTValueElem) extends Node*/
  /*
  def nodeFor(basicType: BasicValueType, matchType: ValueType): Node = {
    basicType match {
      case t: TStruct => {
        val subNodes = mutable.Queue.empty[Node]
        t.fields.foreach { fd =>
          val (_, subBasic) = nameAndBasic(fd.typ)
          subNodes += nodeFor(subBasic, fd.typ)
        }
        TupleNode(matchType, subNodes)
      }
      case t: TList => {
        SeqNode(matchType, t.paramType)
      }
      case t => SingleNode(matchType)
    }
  }
  */

  class ResultBuilder(name: String, attributes: Seq[(String, String)], var text: Option[String], sub: mutable.ArrayBuffer[Element])

  def read(is: InputStream, rootType: VTValueElem): Element = {
    val fac = XMLInputFactory.newInstance()
    val reader = fac.createXMLEventReader(is)

    var builderStack = List.empty[ResultBuilder]

    while (reader.hasNext) {
      val event = reader.nextEvent()

      if (event.isStartElement) {
        println("start: " + event.asStartElement().getName)

        val elem = event.asStartElement()
        val b = new ResultBuilder(elem.getName.getLocalPart, Seq(), None, mutable.ArrayBuffer.empty[Element])
        //builderStack ::=

      } else if (event.isCharacters) {

        println("char: " + event.asCharacters().getData)

      } else if (event.isEndElement) {

        println("end: " + event.asEndElement().getName)

      }

    }

    null
  }
}
