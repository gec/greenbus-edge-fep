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
package io.greenbus.edge.configure.sql.server

import io.greenbus.edge.configure.dyn.{ ModuleValueRemove, ModuleValueUpdate }
import io.greenbus.edge.configure.sql.JooqTransactable
import io.greenbus.edge.stream.gateway.MapDiffCalc
import org.jooq._
import org.jooq.impl.DSL

import scala.concurrent.Future

object ModuleSchema {

  object Values {
    val table = DSL.table(DSL.name("module_component_values"))
    //val t2 = DSL.table

    val module = DSL.field("module", classOf[String])
    val component = DSL.field("component", classOf[String])
    val node = DSL.field("node", classOf[String])
    val data = DSL.field("data", classOf[Array[Byte]])
  }
}

case class ModuleEntrySummary(module: String, component: String, nodeOpt: Option[String])
case class ModuleDbEntry(module: String, component: String, nodeOpt: Option[String], data: Array[Byte])
case class ModuleValue(component: String, nodeOpt: Option[String], data: Array[Byte])

object ModuleDb {
  def build(db: JooqTransactable): ModuleDb = {
    new ModuleDbImpl(db)
  }
}
trait ModuleDb {
  def valuesForModule(module: String): Future[Seq[ModuleDbEntry]]
  def valuesForComponent(component: String): Future[Seq[ModuleDbEntry]]
  def componentSummary(): Future[Seq[ModuleEntrySummary]]
  def valuesForNodeComponent(node: String, component: String): Future[Seq[ModuleDbEntry]]
  def insertValue(value: ModuleDbEntry): Future[Int]
  def moduleUpdates(module: String, values: Seq[ModuleValue]): Future[(Seq[ModuleValueRemove], Seq[ModuleValueUpdate])]
  def removeModule(module: String): Future[Int]
  def getAndRemoveModule(module: String): Future[Seq[ModuleValueRemove]]
}

import scala.collection.JavaConverters._
class ModuleDbImpl(db: JooqTransactable) extends ModuleDb {

  def valuesForModule(module: String): Future[Seq[ModuleDbEntry]] = {
    db.transaction { sql =>
      inTransValueForModule(sql, module)
    }
  }

  private def inTransValueForModule(sql: DSLContext, module: String): Seq[ModuleDbEntry] = {
    import ModuleSchema.Values

    val results: Result[Record3[String, String, Array[Byte]]] =
      sql.select(Values.component, Values.node, Values.data)
        .from(Values.table)
        .where(Values.module.eq(module))
        .fetch()

    results.asScala.map(rec => ModuleDbEntry(module, rec.value1(), Option(rec.value2()), rec.value3())).toVector
  }

  def valuesForComponent(component: String): Future[Seq[ModuleDbEntry]] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val results: Result[Record3[String, String, Array[Byte]]] =
        sql.select(Values.module, Values.node, Values.data)
          .from(Values.table)
          .where(Values.component.eq(component))
          .fetch()

      results.asScala.map(rec => ModuleDbEntry(rec.value1(), component, Option(rec.value2()), rec.value3())).toVector
    }
  }

  def componentSummary(): Future[Seq[ModuleEntrySummary]] = {
    db.transaction { sql =>
      import ModuleSchema.Values

      val results: Result[Record3[String, String, String]] =
        sql.select(Values.module, Values.component, Values.node)
          .from(Values.table)
          .fetch()

      results.asScala.map(rec => ModuleEntrySummary(rec.value1(), rec.value2(), Option(rec.value3()))).toVector
    }
  }

  def valuesForNodeComponent(node: String, component: String): Future[Seq[ModuleDbEntry]] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val results: Result[Record3[String, String, Array[Byte]]] =
        sql.select(Values.module, Values.node, Values.data)
          .from(Values.table)
          .where(Values.component.eq(component).and(Values.node.eq(node)))
          .fetch()

      results.asScala.map(rec => ModuleDbEntry(rec.value1(), component, Option(rec.value2()), rec.value3())).toVector
    }
  }

  def insertValue(value: ModuleDbEntry): Future[Int] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val current = sql.select(Values.data).from(Values.table)
        .where(Values.module.eq(value.module))
        .and(Values.component.eq(value.component))
        .fetch()
        .asScala

      if (current.nonEmpty) {
        sql.update(Values.table)
          .set(Values.data, value.data)
          .where(Values.module.eq(value.module).and(Values.component.eq(value.component)))
          .execute()
      } else {
        sql.insertInto(Values.table,
          Values.module, Values.component, Values.node, Values.data)
          .values(value.module, value.component, value.nodeOpt.orNull, value.data)
          .execute()
      }
    }
  }

  def moduleUpdates(module: String, values: Seq[ModuleValue]): Future[(Seq[ModuleValueRemove], Seq[ModuleValueUpdate])] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val current: Seq[ModuleDbEntry] = inTransValueForModule(sql, module)

      val currentMap = current.map(entry => (entry.component, (entry.nodeOpt, entry.data))).toMap
      val updateMap = values.map(entry => (entry.component, (entry.nodeOpt, entry.data))).toMap

      val (removed, added, modified) = MapDiffCalc.calculate(currentMap, updateMap)

      val addedAndModified = added ++ modified

      sql.deleteFrom(Values.table)
        .where(Values.module.eq(module).and(Values.component.in((removed ++ modified.map(_._1)).asJava)))
        .execute()

      val insert: InsertValuesStep4[Record, String, String, String, Array[Byte]] = sql.insertInto(Values.table)
        .columns(Values.module, Values.component, Values.node, Values.data)

      val allInserts = (added ++ modified).foldLeft(insert) {
        case (ins, (component, (nodeOpt, bytes))) =>
          ins.values(module, component, nodeOpt.orNull, bytes)
      }

      allInserts.execute()

      val removeResults = removed.flatMap { component =>
        currentMap.get(component).map({
          case (nodeOpt, _) => ModuleValueRemove(component, nodeOpt)
        })
      }.toSeq

      val updateResults = addedAndModified.map {
        case (component, (nodeOpt, bytes)) => ModuleValueUpdate(component, nodeOpt, bytes)
      }.toSeq

      (removeResults, updateResults)
    }
  }

  def getAndRemoveModule(module: String): Future[Seq[ModuleValueRemove]] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val results: Result[Record2[String, String]] =
        sql.select(Values.component, Values.node)
          .from(Values.table)
          .where(Values.module.eq(module))
          .fetch()

      val removes = results.asScala.map(rec => ModuleValueRemove(rec.value1(), Option(rec.value2()))).toVector

      sql.deleteFrom(Values.table).where(Values.module.eq(module)).execute()

      removes
    }
  }

  def removeModule(module: String): Future[Int] = {
    db.transaction { sql =>
      import ModuleSchema.Values

      sql.deleteFrom(Values.table).where(Values.module.eq(module)).execute()
    }
  }

}
