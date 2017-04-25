package io.greenbus.edge.configure.sql

import java.util.concurrent.Executors

import org.jooq.impl.DSL

import scala.concurrent.Await
import scala.concurrent.duration._

object InsertTable {

  def main(args: Array[String]): Unit = {
    val settings = SqlSettings.load("io.greenbus.sql.cfg")
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

    val current = moduleDb.valuesForComponent("dnpgateway")

    {
      val result = Await.result(current, 5000.milliseconds)
      println(result)
    }

    Await.result(moduleDb.insertValues(ModuleComponentValue("mode01", "dnpgateway", "content01".getBytes("UTF-8"))), 5000.milliseconds)
    Await.result(moduleDb.insertValues(ModuleComponentValue("mode02", "dnpgateway", "content02".getBytes("UTF-8"))), 5000.milliseconds)

    {
      val result = Await.result(current, 5000.milliseconds)
      println(result)
    }

  }
}
