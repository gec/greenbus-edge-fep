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

import io.greenbus.edge.api.{ EndpointId, Path }
import io.greenbus.edge.api.stream.EndpointBuilder
import io.greenbus.edge.data.{ IndexableValue, Value, ValueString }
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller

import scala.concurrent.Promise

/*
push:
  module (name)
    - component (name)
      - key-value

 */
//case class ModuleComponent(kvs: Map[String, Value])
case class ModuleConfiguration(components: Map[String, Value])

trait ModuleConfigurer {
  def handleModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit
}

class FepConfigureHandle(b: EndpointBuilder) {

  val dnpHandle = b.activeSet(Path("dnp3"))

  private val handle = b.build(100, 100)
  def flush(): Unit = {
    handle.flush()
  }
}

object FepConfigurerMgr {
  def load(eventThread: CallMarshaller, id: EndpointId, producerServices: ProducerServices): FepConfigurerMgr = {

    val b = producerServices.endpointBuilder(id)

    val cfg = new FepConfigureHandle(b)

    new FepConfigurerMgr(eventThread, cfg)
  }
}
class FepConfigurerMgr(eventThread: CallMarshaller, handle: FepConfigureHandle) extends ModuleConfigurer {

  private var current = Map.empty[IndexableValue, Value]

  eventThread.marshal {
    handle.dnpHandle.update(current)
    handle.flush()
  }

  def handleModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {
    eventThread.marshal {
      onModuleConfig(module, config, promise)
    }
  }

  def onModuleConfig(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {
    config.components.foreach {
      case (comp, value) => {
        if (comp == "dnpgateway") {
          current = current + (ValueString(module) -> value)
          handle.dnpHandle.update(current)
        }
      }
    }
    handle.flush()
    promise.success(true)
  }
}