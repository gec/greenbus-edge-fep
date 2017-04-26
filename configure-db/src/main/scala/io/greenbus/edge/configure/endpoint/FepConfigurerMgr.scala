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
package io.greenbus.edge.configure.endpoint

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.api.stream.EndpointBuilder
import io.greenbus.edge.configure.sql.{ ModuleComponentValue, ModuleDb }
import io.greenbus.edge.data.proto.convert.ValueConversions
import io.greenbus.edge.data.{ IndexableValue, Value, ValueString }
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller

import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global

/*
push:
  module (name)
    - component (name)
      - key-value

 */
//case class ModuleComponent(kvs: Map[String, Value])
case class ModuleConfiguration(components: Map[String, Value])

trait ModuleConfigurer {
  def updateModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit
  def removeModule(module: String, promise: Promise[Boolean]): Unit
}

class FepConfigureHandle(b: EndpointBuilder) {

  val dnpHandle = b.activeSet(Path("dnp3"))

  private val handle = b.build(100, 100)
  def flush(): Unit = {
    handle.flush()
  }
}

object FepConfigurerMgr extends LazyLogging {
  def load(eventThread: CallMarshaller, id: EndpointId, producerServices: ProducerServices, db: ModuleDb): FepConfigurerMgr = {

    val b = producerServices.endpointBuilder(id)

    val cfg = new FepConfigureHandle(b)

    new FepConfigurerMgr(eventThread, cfg, db)
  }

  def fromDbBytes(ctx: String, bytes: Array[Byte]): Option[Value] = {
    try {
      val proto = io.greenbus.edge.data.proto.Value.parseFrom(bytes)
      ValueConversions.fromProto(proto) match {
        case Left(err) =>
          logger.warn(s"Could not parse proto def for $ctx: " + err); None
        case Right(v) => Some(v)
      }
    } catch {
      case ex: Throwable =>
        logger.warn(s"Could not parse bytes for $ctx: " + ex)
        None
    }
  }
}
class FepConfigurerMgr(eventThread: CallMarshaller, handle: FepConfigureHandle, db: ModuleDb) extends ModuleConfigurer {

  private var currentDnp = Map.empty[IndexableValue, Value]

  eventThread.marshal {
    load()
    //handle.dnpHandle.update(currentDnp)
    //handle.flush()
  }

  private def load(): Unit = {
    db.valuesForComponent("dnpgateway").foreach { values =>
      eventThread.marshal {
        currentDnp = values.flatMap { mv =>
          FepConfigurerMgr.fromDbBytes(s"${mv.module}/${mv.component}", mv.data)
            .map(v => (ValueString(mv.module), v))
        }.toMap
        handle.flush()
      }
    }
  }

  def removeModule(module: String, promise: Promise[Boolean]): Unit = {
    db.removeModule(module).foreach(_ => eventThread.marshal { onModuleRemove(module, promise) })
  }

  def updateModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {
    val updates = config.components.map { case (comp, v) => ModuleComponentValue(module, comp, ValueConversions.toProto(v).toByteArray) }
    val futs = updates.map(v => db.insertValues(v))
    Future.sequence(futs).foreach { _ =>
      eventThread.marshal {
        onModuleUpdate(module, config, promise)
      }
    }
  }

  def onModuleRemove(module: String, promise: Promise[Boolean]): Unit = {
    currentDnp = currentDnp - ValueString(module)
    handle.dnpHandle.update(currentDnp)
    handle.flush()
    promise.success(true)
  }

  def onModuleUpdate(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {
    config.components.foreach {
      case (comp, value) => {
        if (comp == "dnpgateway") {
          currentDnp = currentDnp + (ValueString(module) -> value)
        }
      }
    }
    handle.dnpHandle.update(currentDnp)
    handle.flush()
    promise.success(true)
  }
}