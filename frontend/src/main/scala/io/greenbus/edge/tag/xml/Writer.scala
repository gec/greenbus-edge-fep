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

  def write(value: Element, os: OutputStream): Unit = {

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
    }
  }

  def writeElem(value: Element, w: XMLStreamWriter, ctxName: Option[String] = None): Unit = {
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

  def writeField(field: Field, w: XMLStreamWriter): Unit = {
    field match {
      case tf: TaggedField => writeElem(tf.value, w, Some(tf.name))
      case v: Element => writeElem(v, w, None)
    }
  }
}

object XmlReader {
  import scala.collection.mutable

  /*
    expectation stack:
    - singular
    - iterable

     */

  case class ResultBuilder(attributes: Seq[(String, String)], results: mutable.ArrayBuffer[Field])

  def read(is: InputStream, rootType: VType): Unit = {

    val fac = XMLInputFactory.newInstance()
    val doc = fac.createXMLStreamReader(is)

    val schemaStack = mutable.ArrayBuffer.empty[VType]
    val resultStack = mutable.ArrayBuffer.empty[ResultBuilder]

    while (doc.hasNext) {
      val eventType = doc.next()

      eventType match {
        case XMLStreamConstants.START_ELEMENT => {
          println("startElement getname: " + doc.getLocalName)

          schemaStack.last match {
            case t: VTExtType => {
              if (doc.getLocalName == t.tag) {

              } else {
                throw new IllegalArgumentException(s"Did not match user type ${t.tag}")
              }
            }
            //case t: VT
          }

        }
        case XMLStreamConstants.END_ELEMENT => {
          println("end elem getname: " + doc.getLocalName)

        }
        case XMLStreamConstants.ATTRIBUTE =>
          println("attr")
        case _ =>
      }

      //println(eventType)

    }
  }

  /*def expectExtType(typ: VTExtType, doc: XMLStreamReader): TaggedValue = {
    if (!doc.hasNext) throw new IllegalArgumentException("Exited too early")
    val eventType = doc.next()

    eventType match {
      case XMLStreamConstants.START_ELEMENT => {

      }
    }
  }*/

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

  /*
  <?xml version="1.0" ?>
<Master>
  <StackConfig>
    <LinkLayer>
      <isMaster value="true"/>
      <localAddress value="1"/>
      <remoteAddress value="100"/>
      <userConfirmations value="false"/>
      <ackTimeoutMs value="1000"/>
      <numRetries value="3"/>
    </LinkLayer>
    <AppLayer>
      <timeoutMs value="5000"/>
      <maxFragSize value="2048"/>
      <numRetries value="0"/>
    </AppLayer>
  </StackConfig>
  <MasterSettings>
    <allowTimeSync value="true"/>
    <taskRetryMs value="5000"/>
    <integrityPeriodMs value="300000"/>
  </MasterSettings>
  <scanList>
    <Scan>
      <enableClass1 value="true"/>
      <enableClass2 value="true"/>
      <enableClass3 value="true"/>
      <periodMs value="2000"/>
    </Scan>
  </scanList>
  <Unsol>
    <doTask value="true"/>
    <enable value="true"/>
    <enableClass1 value="true"/>
    <enableClass2 value="true"/>
    <enableClass3 value="true"/>
  </Unsol>
</Master>
   */

}
