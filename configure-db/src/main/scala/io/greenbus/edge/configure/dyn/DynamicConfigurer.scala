package io.greenbus.edge.configure.dyn

import java.util.concurrent.Executors

import io.greenbus.edge.api.{EndpointId, Path}
import io.greenbus.edge.configure.endpoint.ModuleConfigurerSettings
import io.greenbus.edge.configure.server.WebServer
import io.greenbus.edge.configure.sql.server.ModuleDb
import io.greenbus.edge.configure.sql.{JooqTransactable, PostgresDataPool, SqlSettings}
import io.greenbus.edge.peer.{AmqpEdgeConnectionManager, PeerClientSettings}
import io.greenbus.edge.thread.EventThreadService

import scala.concurrent.ExecutionContext.Implicits.global

object DynamicConfigurer {

  def main(args: Array[String]): Unit = {

    val baseDir = Option(System.getProperty("io.greenbus.config.base")).getOrElse("")
    val sqlConfigPath = Option(System.getProperty("io.greenbus.config.sql")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.sql.cfg")
    val amqpConfigPath = Option(System.getProperty("io.greenbus.edge.config.client")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.peer.client.cfg")
    val appConfigPath = Option(System.getProperty("io.greenbus.edge.config.moduleconfig")).map(baseDir + _).getOrElse(baseDir + "io.greenbus.edge.moduleconfig.cfg")

    val clientSettings = PeerClientSettings.load(amqpConfigPath)
    val appSettings = ModuleConfigurerSettings.load(appConfigPath)

    val settings = SqlSettings.load(sqlConfigPath)
    val ds = PostgresDataPool.dataSource(settings)

    val s = Executors.newFixedThreadPool(4)
    val db = JooqTransactable.build(ds, s)

    val moduleDb = ModuleDb.build(db)

    val services = AmqpEdgeConnectionManager.build(
      clientSettings.host,
      clientSettings.port,
      retryIntervalMs = clientSettings.retryIntervalMs,
      connectTimeoutMs = clientSettings.connectTimeoutMs)

    val producerServices = services.bindProducerServices()
    val eventThread = EventThreadService.build("Configurer Event")

    val mgr = ConfigurationTable.build(eventThread, EndpointId(Path(Seq("configuration_server"))), producerServices, moduleDb)
    services.start()

    val server = WebServer.build(mgr, appSettings.port)
    server.start()
    server.join()
    s.shutdown()
  }
}
