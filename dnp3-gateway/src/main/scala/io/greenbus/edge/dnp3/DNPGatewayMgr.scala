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

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api.ProducerService
import io.greenbus.edge.data.SampleValue
import io.greenbus.edge.dnp3.config.model.DNP3Gateway
import io.greenbus.edge.fep._
import io.greenbus.edge.thread.CallMarshaller

trait DNPGatewayHandler {
  def onGatewayConfigured(key: String, config: DNP3Gateway): Unit
  def onGatewayRemoved(key: String): Unit
}

class DNPGatewayMgr(eventThread: CallMarshaller, localId: String, producerServices: ProducerService, eventSink: EventSink) extends ConfigurationHandler[DNP3Gateway] with LazyLogging {

  private val mgr = new Dnp3Mgr
  private var resources = Map.empty[String, (DnpPubAdapter, KeyedDeviceObserver)]

  // TODO: close producers
  def onConfigured(key: String, config: DNP3Gateway): Unit = {
    logger.info(s"Gateway configured: $key")
    eventThread.marshal {
      eventSink.publishEvent(Seq("source", "updated"), s"Gateway configured: $key")

      remove(key)

      val stackConfig = Dnp3MasterConfig.load(config)

      val rawDnpEndpoint = new DnpPubAdapter(eventThread, localId, producerServices, config)

      val gatewayPub = new FrontendPubAdapter(eventThread, producerServices, rawDnpEndpoint, config.endpoint)

      val keyedObserver = new SplittingKeyedObserver(Seq(rawDnpEndpoint, gatewayPub))

      val adapter = new KeyedObserverAdapter(keyedObserver)

      val cmdAcceptor = mgr.add(key, stackConfig, adapter, adapter.commsObs)

      val stackCmdMgr = new DNP3ControlHandleImpl(eventThread, cmdAcceptor)
      rawDnpEndpoint.setControlHandler(stackCmdMgr)

      resources += ((key, (rawDnpEndpoint, gatewayPub)))
    }
  }

  def onRemoved(key: String): Unit = {
    logger.info(s"Gateway removed: $key")
    eventThread.marshal {
      eventSink.publishEvent(Seq("source", "updated"), s"Gateway removed: $key")
      remove(key)
    }
  }

  private def remove(key: String): Unit = {
    mgr.remove(key)
    resources.get(key).foreach {
      case (handle1, handle2) =>
        handle1.close()
        handle2.close()
    }
  }

  def close(): Unit = {
    logger.info(s"Gateway mgr closed")
    eventThread.marshal {
      mgr.shutdown()
    }
  }
}

class KeyedObserverAdapter(obs: KeyedDeviceObserver) extends MeasObserver {

  def commsObs(value: Boolean): Unit = {
    if (value) {
      obs.handleOnline()
    } else {
      obs.handleOffline()
    }
  }

  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    obs.handleBatch(batch)
  }
}
