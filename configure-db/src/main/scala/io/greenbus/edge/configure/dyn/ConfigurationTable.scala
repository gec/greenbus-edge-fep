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

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.configure.endpoint.{ ModuleConfiguration, ModuleConfigurer }
import io.greenbus.edge.configure.sql.server.{ ModuleDb, ModuleValue }
import io.greenbus.edge.data.proto.convert.ValueConversions
import io.greenbus.edge.thread.CallMarshaller

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise

object ConfigurationTable {
  def build(eventThread: CallMarshaller, id: EndpointId, producerServices: ProducerService, db: ModuleDb): ConfigurationTable = {
    new ConfigurationTable(eventThread, db, id, producerServices)
  }
}
class ConfigurationTable(eventThread: CallMarshaller, db: ModuleDb, endpointId: EndpointId, producerService: ProducerService) extends ModuleConfigurer with LazyLogging {

  private var dynSetHandleOpt = Option.empty[DynamicActiveSetHandle]
  private var producerHandleOpt = Option.empty[ProducerHandle]

  private val index = new ModuleIndex(eventThread, db)

  eventThread.marshal {
    init()
  }

  private def init(): Unit = {
    val builder = producerService.endpointBuilder(endpointId)

    val setHandle = builder.activeSetDynamicSet("configuration", new DynamicDataKey {
      def subscribed(path: Path): Unit = {
        eventThread.marshal(onSubscribed(path))
      }

      def unsubscribed(path: Path): Unit = {
        eventThread.marshal(onUnsubscribed(path))
      }
    })

    dynSetHandleOpt = Some(setHandle)
    val producerHandle = builder.build()
    producerHandleOpt = Some(producerHandle)
    index.setProducerHandle(producerHandle)
  }

  private def onSubscribed(path: Path): Unit = {
    logger.debug(s"onSubscribed: $path")
    if (path.parts.size >= 2) {
      val node = path.parts(0)
      val component = path.parts(1)

      dynSetHandleOpt.foreach { dynHandle =>
        val handle: ActiveSetHandle = dynHandle.add(path)
        index.register(node, component, handle)
        producerHandleOpt.foreach(_.flush())
      }
    }
  }

  private def onUnsubscribed(path: Path): Unit = {
    logger.debug(s"onUnsubscribed: $path")
    if (path.parts.size >= 2) {
      val node = path.parts(0)
      val component = path.parts(1)
      index.unregister(node, component)
    }
    dynSetHandleOpt.foreach { h => h.remove(path) }
    producerHandleOpt.foreach(_.flush())
  }

  def updateModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {

    val moduleValues = config.components.map {
      case (component, (v, nodeOpt)) =>
        ModuleValue(component, nodeOpt, ValueConversions.toProto(v).toByteArray)
    }.toSeq

    val resultsFut = db.moduleUpdates(module, moduleValues)

    resultsFut.foreach {
      case (removes, updates) =>
        eventThread.marshal {
          index.onModuleUpdates(module, removes, updates)
          producerHandleOpt.foreach(_.flush())
        }
    }

    promise.completeWith(resultsFut.map(_ => true))
  }

  def removeModule(module: String, promise: Promise[Boolean]): Unit = {

    val removeFut = db.getAndRemoveModule(module)

    removeFut.foreach { removes =>
      index.onModuleUpdates(module, removes, Seq())
      producerHandleOpt.foreach(_.flush())
    }

    promise.completeWith(removeFut.map(_ => true))
  }
}
