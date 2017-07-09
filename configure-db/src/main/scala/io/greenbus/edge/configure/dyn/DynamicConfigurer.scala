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
package io.greenbus.edge.configure.dyn

import java.util.concurrent.Executors

import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.configure.endpoint.ModuleConfigurerSettings
import io.greenbus.edge.configure.server.WebServer
import io.greenbus.edge.configure.sql.server.ModuleDb
import io.greenbus.edge.configure.sql.{ JooqTransactable, PostgresDataPool, SqlSettings }
import io.greenbus.edge.peer.{ AmqpEdgeConnectionManager, PeerClientSettings }
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
