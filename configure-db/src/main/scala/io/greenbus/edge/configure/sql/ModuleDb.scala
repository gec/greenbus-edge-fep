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
package io.greenbus.edge.configure.sql

import org.jooq._
import org.jooq.impl.DSL

import scala.concurrent.Future

/*
create table module_component_values (
  module text not null,
  component text not null,
  data bytea not null,
  PRIMARY KEY (module, component)
) WITH (OIDS = false)
;
create
 */
object ModuleSchema {

  object Values {
    val table = DSL.table(DSL.name("module_component_values"))
    //val t2 = DSL.table

    val module = DSL.field("module", classOf[String])
    val component = DSL.field("component", classOf[String])
    val data = DSL.field("data", classOf[Array[Byte]])
  }
}

case class ModuleComponentValue(module: String, component: String, data: Array[Byte])

object ModuleDb {
  def build(db: JooqTransactable): ModuleDb = {
    new ModuleDbImpl(db)
  }
}
trait ModuleDb {
  def valuesForModule(module: String): Future[Seq[ModuleComponentValue]]
  def valuesForComponent(component: String): Future[Seq[ModuleComponentValue]]
  def insertValues(value: ModuleComponentValue): Future[Int]
  def removeModule(module: String): Future[Int]
}

import scala.collection.JavaConverters._
class ModuleDbImpl(db: JooqTransactable) extends ModuleDb {

  def valuesForModule(module: String): Future[Seq[ModuleComponentValue]] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val results: Result[Record2[String, Array[Byte]]] =
        sql.select(Values.component, Values.data)
          .from(Values.table)
          .where(Values.module.eq(module))
          .fetch()

      results.asScala.map(rec => ModuleComponentValue(module, rec.value1(), rec.value2())).toVector
    }
  }

  def valuesForComponent(component: String): Future[Seq[ModuleComponentValue]] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val results: Result[Record2[String, Array[Byte]]] =
        sql.select(Values.module, Values.data)
          .from(Values.table)
          .where(Values.component.eq(component))
          .fetch()

      results.asScala.map(rec => ModuleComponentValue(rec.value1(), component, rec.value2())).toVector
    }
  }

  def insertValues(value: ModuleComponentValue): Future[Int] = {
    db.transaction { sql =>

      import ModuleSchema.Values

      val current = sql.select(Values.data).from(Values.table)
        .where(Values.module.eq(value.module))
        .and(Values.component.eq(value.component))
        .fetch()
        .asScala

      if (current.nonEmpty) {
        sql.update(Values.table).set(Values.data, value.data).execute()
      } else {
        sql.insertInto(Values.table,
          Values.module, Values.component, Values.data)
          .values(value.module, value.component, value.data)
          .execute()
      }
    }
  }

  def removeModule(module: String): Future[Int] = {
    db.transaction { sql =>
      import ModuleSchema.Values

      sql.deleteFrom(Values.table).where(Values.module.eq(module)).execute()
    }
  }

}
