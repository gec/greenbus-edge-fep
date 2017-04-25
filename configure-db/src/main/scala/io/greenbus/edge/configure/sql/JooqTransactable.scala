package io.greenbus.edge.configure.sql

import java.util.concurrent.Executor
import javax.sql.DataSource

import org.jooq.{Configuration, DSLContext, TransactionalRunnable}
import org.jooq.impl.DSL

import scala.concurrent.{Future, Promise}


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
    val jooq = DSL.using(ds.getConnection)
    val prom = Promise[A]

    exe.execute(new Runnable {
      def run(): Unit = {
        jooq.transaction(new TransactionalRunnable {
          def run(configuration: Configuration): Unit = {
            val inTrans: DSLContext = DSL.using(configuration)
            try {
              prom.success(f(inTrans))
            } catch {
              case ex: Throwable =>
                prom.failure(ex)
            }
          }
        })
      }
    })

    prom.future
  }
}