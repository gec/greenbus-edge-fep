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

import java.util.concurrent.Executors

import org.jooq.impl.DSL

import scala.concurrent.Await
import scala.concurrent.duration._

object InsertTable {

  def main(args: Array[String]): Unit = {
    val settings = SqlSettings.load("io.greenbus.test.cfg")
    val ds = PostgresDataPool.dataSource(settings)

    val s = Executors.newFixedThreadPool(4)
    val db = JooqTransactable.build(ds, s)

    val moduleDb = ModuleDb.build(db)
    try {
      run(moduleDb)
    } catch {
      case ex: Throwable =>
        println(ex)
    } finally {
      s.shutdown()
    }
  }

  def run(moduleDb: ModuleDb): Unit = {

    {
      println("ORIGINAL: ")
      val current = moduleDb.valuesForComponent("dnpgateway")
      val result = Await.result(current, 5000.milliseconds)
      println(result)
    }

    Await.result(moduleDb.insertValues(ModuleComponentValue("mode01", "dnpgateway", "content01".getBytes("UTF-8"))), 5000.milliseconds)
    Await.result(moduleDb.insertValues(ModuleComponentValue("mode02", "dnpgateway", "content02".getBytes("UTF-8"))), 5000.milliseconds)

    {
      println("INSERTED 2: ")
      val current = moduleDb.valuesForComponent("dnpgateway")
      val result = Await.result(current, 5000.milliseconds)
      println(result)
    }

    Await.result(moduleDb.removeModule("mode01"), 5000.milliseconds)

    {
      println("REMOVED 1: ")
      val current = moduleDb.valuesForComponent("dnpgateway")
      val result = Await.result(current, 5000.milliseconds)
      println(result)
    }

  }
}
