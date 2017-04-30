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

import java.util.concurrent.Executor
import javax.sql.DataSource

import org.jooq.{ Configuration, DSLContext, TransactionalRunnable }
import org.jooq.impl.DSL

import scala.concurrent.{ Future, Promise }
import scala.util.{ Failure, Success, Try }

object JooqTransactable {
  def build(ds: DataSource, exe: Executor): JooqTransactable = {
    new JooqTransactableImpl(ds, exe)
  }
}
trait JooqTransactable {
  def transaction[A](f: DSLContext => A): Future[A]
}

class JooqTransactableImpl(ds: DataSource, exe: Executor) extends JooqTransactable {
  def transaction[A](f: DSLContext => A): Future[A] = {

    val prom = Promise[A]

    exe.execute(new Runnable {
      def run(): Unit = {
        var result = Option.empty[Try[A]]
        val connection = ds.getConnection
        try {
          val jooq = DSL.using(connection)
          jooq.transaction(new TransactionalRunnable {
            def run(configuration: Configuration): Unit = {
              val inTrans: DSLContext = DSL.using(configuration)
              result = try {
                Some(Success(f(inTrans)))
              } catch {
                case ex: Throwable =>
                  Some(Failure(ex))
              }
            }
          })
        } catch {
          case ex: Throwable => prom.failure(ex)
        } finally {
          connection.close()
        }

        prom.complete(result.getOrElse(Failure(new IllegalStateException("no transaction result"))))
      }
    })

    prom.future
  }
}