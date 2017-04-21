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
package io.greenbus.edge.dnp3

import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.dnp3.config.model.DNP3Gateway
import io.greenbus.edge.fep.PublisherHandle
import io.greenbus.edge.fep.model.FrontendEndpointConfiguration
import io.greenbus.edge.peer.ProducerServices
import io.greenbus.edge.thread.CallMarshaller

class SplittingMeasObserver(observers: Seq[MeasObserver]) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    observers.foreach(_.flush(batch))
  }
}

class FrontendAdapter(handle: PublisherHandle) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    handle.batch(batch)
  }
}

class DNPGatewayMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerServices) {

  private val mgr = new Dnp3Mgr[String]

  def onGatewayConfigured(endpointConfig: FrontendEndpointConfiguration, config: DNP3Gateway): Unit = {
    eventThread.marshal {
      val name = config.client.host + ":" + config.client.port
      val stackConfig = Dnp3MasterConfig.load(config)

      val (rawMeasObserver, controlAdapter) = RawDnpEndpoint.build(eventThread, localId, producerServices, config)
      def commsObs(value: Boolean): Unit = println("got comms: " + value)

      val gatewayPub = PublisherHandle.load(producerServices, endpointConfig)

      val observer = new SplittingMeasObserver(Seq(
        rawMeasObserver,
        new FrontendAdapter(gatewayPub)))

      val cmdAcceptor = mgr.add(name, name, stackConfig, observer, commsObs)

      val cmdMgr = new DNP3ControlHandleImpl(eventThread, cmdAcceptor)

    }
  }

  def close(): Unit = {
    eventThread.marshal {
      mgr.shutdown()
    }
  }
}
