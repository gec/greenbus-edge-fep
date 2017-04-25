package io.greenbus.edge.configure.sql

import java.io.FileInputStream
import java.util.Properties

import scala.collection.JavaConverters._

class ConfigurationException(message: String) extends Exception(message)

object PropertyReading {

  def loadFile(filename: String): Map[String, String] = {
    val properties = new Properties
    try {
      properties.load(new FileInputStream(filename))
      asMap(properties)
    } catch {
      case ex: Throwable =>
        throw new ConfigurationException("Problem loading configuration file: " + ex.getMessage)
    }
  }

  private def asMap(properties: Properties): Map[String, String] = {
    properties.stringPropertyNames().asScala.map(name => (name, properties.getProperty(name))).toMap
  }

  private def missing(name: String) = new ConfigurationException(s"Missing property: '$name'")

  def optional(props: Map[String, String], name: String): Option[String] = {
    Option(System.getProperty(name)) orElse props.get(name)
  }

  def get(props: Map[String, String], name: String): String = {
    optional(props, name) getOrElse (throw missing(name))
  }

  def optionalLong(props: Map[String, String], name: String): Option[Long] = {
    checkSignedInt(name) {
      optional(props, name).map(_.toLong)
    }
  }

  def getLong(props: Map[String, String], name: String): Long = {
    checkSignedInt(name) {
      get(props, name).toLong
    }
  }

  def optionalInt(props: Map[String, String], name: String): Option[Int] = {
    checkSignedInt(name) {
      optional(props, name).map(_.toInt)
    }
  }

  def getInt(props: Map[String, String], name: String): Int = {
    checkSignedInt(name) {
      get(props, name).toInt
    }
  }

  private def checkSignedInt[A](name: String)(fun: => A): A = {
    try {
      fun
    } catch {
      case ex: NumberFormatException =>
        throw new ConfigurationException(s"Configuration property '$name' must be a signed integer.")
    }
  }
}
