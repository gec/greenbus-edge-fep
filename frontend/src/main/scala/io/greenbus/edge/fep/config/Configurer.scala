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
package io.greenbus.edge.fep.config

import java.util.UUID

import io.greenbus.edge.api.stream.{ EndpointBuilder, ProducerHandle }
import io.greenbus.edge.api._
import io.greenbus.edge.data.ValueString
import io.greenbus.edge.flow
import io.greenbus.edge.peer.{ AmqpEdgeService, ProducerServices }
import io.greenbus.edge.thread.{ CallMarshaller, EventThreadService }

import scala.concurrent.ExecutionContext.Implicits.global

object Configurer {

  def main(args: Array[String]): Unit = {

    val services = AmqpEdgeService.build("127.0.0.1", 50001, 10000)
    services.start()
    val producerServices = services.producer

    val eventThread = EventThreadService.build("Config")

    val end = FepConfigureEndpoint.load(eventThread, EndpointId(Path(Seq("fep", "config", "dnp3"))), producerServices)

    System.in.read()

    //gatewayMgr.close()
    services.shutdown()
    eventThread.close()
  }
}

class FepConfigureHandle(b: EndpointBuilder) {

  val dnpHandle = b.activeSet(Path("dnp3"))

  val uploadHandle = b.outputStatus(Path(Seq("dnp3", "upload")))
  val req = b.registerOutput(Path(Seq("dnp3", "upload")))
}

object FepConfigureEndpoint {
  def load(eventThread: CallMarshaller, id: EndpointId, producerServices: ProducerServices): FepConfigureEndpoint = {

    val b = producerServices.endpointBuilder(id)

    val cfg = new FepConfigureHandle(b)

    val handle: ProducerHandle = b.build(100, 100)

    new FepConfigureEndpoint(eventThread, handle, cfg)
  }
}
class FepConfigureEndpoint(eventThread: CallMarshaller, handle: ProducerHandle, end: FepConfigureHandle) {

  private val session = UUID.randomUUID()
  eventThread.marshal {
    end.dnpHandle.update(Map(ValueString("name01") -> ValueString("temp"), ValueString("name02") -> ValueString("temp")))
    end.uploadHandle.update(OutputKeyStatus(session, 0, None))
  }

  end.req.bind(new flow.Responder[OutputParams, OutputResult] {
    def handle(obj: OutputParams, respond: (OutputResult) => Unit): Unit = {
      eventThread.marshal {
        onUploadReq(obj, respond)
      }
    }
  })

  def onUploadReq(params: OutputParams, respond: OutputResult => Unit): Unit = {
    println(params)
  }

}