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
package io.greenbus.edge.sql.h2
import org.h2.jdbcx.JdbcDataSource
import javax.naming.Context
import javax.naming.InitialContext

import org.h2.tools.Server
import org.jooq.conf.Settings
import org.jooq.{ Configuration, SQLDialect, TransactionalRunnable }
import org.jooq.impl.{ DSL, SQLDataType }

object Main {

  /*
  node fep configurer process
    - endpoint: fep_configurer / (node)
      - key: dnp

  node dnp fep gateway
    - endpoint: dnp_gateway / (node)
    (request dnp endpoints for node)
    - endpoint: (dnp)


   */

  def main(args: Array[String]): Unit = {
    val ds = new JdbcDataSource
    //ds.setURL("jdbc:h2:./h2/h2test;AUTO_SERVER=TRUE;AUTO_SERVER_PORT=9123")
    ds.setURL("jdbc:h2:tcp://localhost:9123/mem:test")
    ds.setUser("sa")
    ds.setPassword("sa")

    val server = Server.createTcpServer("-tcpPort", "9123", "-tcpAllowOthers")
    server.start()

    val connection = ds.getConnection

    //val jooqH2 = org.jooq.util.h2.
    val sql = DSL.using(connection, SQLDialect.H2)

    sql.transaction(new TransactionalRunnable {
      def run(configuration: Configuration): Unit = {

        val trans = DSL.using(configuration)

        trans.createSchema("my_schema_2").execute()

        trans.createTable(DSL.name("my_schema_2", "my_table"))
          .column("key", SQLDataType.VARCHAR)
          .column("value", SQLDataType.BINARY)
          .execute()

      }
    })

    try {

      sql.transaction(new TransactionalRunnable {
        def run(configuration: Configuration): Unit = {

          val trans = DSL.using(configuration)

          val result = trans.select().from(DSL.name("my_schema_2", "my_table")).fetch()
          println(result)
        }
      })
    } catch {
      case ex: Throwable =>
        println(ex)
    }

    try {

      sql.transaction(new TransactionalRunnable {
        def run(configuration: Configuration): Unit = {

          val result = DSL.using(configuration).meta().getTables
          println(result)
        }
      })
    } catch {
      case ex: Throwable =>
        println(ex)
    }

    System.in.read()

    /*sql.transaction(new TransactionalRunnable {
      def run(configuration: Configuration): Unit = {

        val trans = DSL.using(configuration)

        val result = trans.select().from("my_schema.my_table").fetch()
        println(result)
      }
    })*/

    /*sql.createSchema("my_schema").execute()

    sql.createTable("my_schema.my_table")
      .column("key", SQLDataType.VARCHAR)
      .column("value", SQLDataType.BINARY)
      .execute()

    connection.commit()*/

    /*val sql2 = DSL.using(connection, SQLDialect.H2)

    val results = sql2.select().from("my_schema.my_table").fetch()

    println(results)*/

    /*
    DSLContext create = DSL.using(conn, SQLDialect.MYSQL);
Result<Record> result = create.select().from(AUTHOR).fetch();
     */

  }
}
