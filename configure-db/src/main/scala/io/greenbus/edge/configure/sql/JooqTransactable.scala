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

import org.jooq.exception.DataAccessException

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

  private def inTransaction[A](f: DSLContext => A): Try[A] = {
    var result = Option.empty[Try[A]]
    try {

      val connection = ds.getConnection
      try {
        val jooq = DSL.using(connection)
        jooq.transaction(new TransactionalRunnable {
          def run(configuration: Configuration): Unit = {
            val inTrans: DSLContext = DSL.using(configuration)
            try {
              result = Some(Success(f(inTrans)))
            } catch {
              case ex: Throwable =>
                result = Some(Failure(ex))
                throw ex
            }
          }
        })
      } catch {
        case ex: DataAccessException =>
          // Jooq wraps the original exception
          Option(ex.getCause()) match {
            case None =>
              result = Some(Failure(ex))
            case Some(cause) =>
              result = Some(Failure(cause))
          }
        case ex: Throwable =>
          result = Some(Failure(ex))
      } finally {
        connection.close()
      }

    } catch {
      case ex: Throwable =>
        result = Some(Failure(ex))
    }

    result.getOrElse(Failure(new IllegalStateException("no transaction result")))
  }

  def blockingTransaction[A](f: DSLContext => A): A = {
    val result = inTransaction(f)
    result match {
      case Success(r) => r
      case Failure(ex) => throw ex
    }
  }

  def transaction[A](f: DSLContext => A): Future[A] = {

    val prom = Promise[A]

    exe.execute(new Runnable {
      def run(): Unit = {

        prom.complete(inTransaction(f))
      }
    })

    prom.future
  }
}

/*
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
          case ex: Throwable =>
            println(ex)
            prom.failure(ex)
        } finally {
          connection.close()
        }

        prom.complete(result.getOrElse(Failure(new IllegalStateException("no transaction result"))))
      }
    })

    prom.future
  }
}*/
