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
package io.greenbus.edge.dnp3.config

import java.io.PrintWriter

import io.greenbus.edge.data.schema.{ TExt, TOption, TStruct, VTValueElem }

object DocGen {

  def main(args: Array[String]): Unit = {

    generate(DnpGatewaySchema.all)
  }

  def generate(userTypes: Seq[TExt]): Unit = {
    val w = new PrintWriter(System.out)
    val sorted = userTypes.sortBy(_.tag)

    //println(sorted.map(_.tag).mkString("\n"))

    sorted.foreach(t => typeEntry(w, t))

    w.flush()
  }

  def typeEntry(w: PrintWriter, typ: TExt): Unit = {
    typ.reprType match {
      case s: TStruct => structEntry(w, typ.tag, s)
      case _ =>
    }
  }

  def structEntry(w: PrintWriter, tag: String, struct: TStruct): Unit = {

    w.println()
    w.println(s"### $tag")
    w.println()
    w.println("...")
    w.println()
    w.println("Fields:")
    w.println()

    val table = struct.fields.map { s =>
      Vector(s"`${s.name}`", fieldDefName(s.typ), "description")
    }

    printTable(w, Vector("Name", "Type", "Description"), table)
  }

  def fieldDefName(fieldType: VTValueElem): String = {
    fieldType match {
      case ext: TExt => ext.tag
      case opt: TOption => "opt"
      case _ => "unknown"
    }
  }

  def printTable(w: PrintWriter, headers: Vector[String], rows: Seq[Vector[String]]): Unit = {
    val tableWidth = headers.size

    val widths: Array[Int] = Array.ofDim[Int](tableWidth)
    Range(0, tableWidth).foreach { i => widths.update(i, 0) }

    (Seq(headers) ++ rows).foreach { row =>
      row.zipWithIndex.take(tableWidth).foreach {
        case (v, i) => widths.update(i, Math.max(v.length, widths(i)))
      }
    }

    def printRow(row: Vector[String]): Unit = {

      val paddedCols = row.zipWithIndex.map {
        case (col, i) =>
          val colSize = widths(i)
          col.padTo(colSize, " ").mkString
      }

      w.println(paddedCols.mkString("| ", " | ", " |"))
    }

    printRow(headers)
    w.println(widths.map(count => stringOf(count + 2, "-")).mkString("|", "|", "|"))
    rows.foreach(printRow)
  }

  def stringOf(n: Int, part: String): String = {
    val b = StringBuilder.newBuilder
    Range(0, n).foreach(_ => b.append(part))
    b.mkString
  }
}
