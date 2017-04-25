package io.greenbus.edge.configure.sql

import com.typesafe.scalalogging.LazyLogging

import PropertyReading._

object SqlSettings extends LazyLogging {

  def load(file: String): SqlSettings = {
    apply(loadFile(file))
  }

  def apply(props: Map[String, String]): SqlSettings = {
    SqlSettings(
      get(props, "io.greenbus.sql.url"),
      get(props, "io.greenbus.sql.username"),
      get(props, "io.greenbus.sql.password"),
      getLong(props, "io.greenbus.sql.slowquery"),
      optionalInt(props, "io.greenbus.sql.maxactive") getOrElse 8)
  }
}

case class SqlSettings(url: String, user: String, password: String,
                       slowQueryTimeMilli: Long, poolMaxActive: Int) {

  // custom to hide password
  override def toString() = user + "@" + url
}

