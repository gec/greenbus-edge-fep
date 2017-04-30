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
package io.greenbus.edge.dnp3.sub

import java.io.ByteArrayInputStream

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.data.mapping.{ RootCtx, SimpleReaderContext }
import io.greenbus.edge.data.xml.XmlReader
import io.greenbus.edge.data.{ IndexableValue, Value, ValueBytes, ValueString }
import io.greenbus.edge.dnp3.{ DNPGatewayHandler, EventSink, GatewayEndpointPublisher }
import io.greenbus.edge.dnp3.config.DnpGatewaySchema
import io.greenbus.edge.dnp3.config.model.DNP3Gateway
import io.greenbus.edge.peer.ConsumerServices
import io.greenbus.edge.thread.CallMarshaller

class ConfigSubscriber(eventThread: CallMarshaller, consumerServices: ConsumerServices, handler: DNPGatewayHandler, eventSink: EventSink) extends LazyLogging {

  private val endpointPath = EndpointPath(EndpointId(Path("configuration_server")), Path("dnp3"))
  private var connected = false

  private val sub = consumerServices.subscriptionClient.subscribe(
    SubscriptionParams(dataKeys =
      DataKeySubscriptionParams(activeSet =
        Seq(endpointPath))))

  sub.updates.bind(updates => eventThread.marshal {
    onUpdates(updates)
  })

  private var current = Map.empty[String, DNP3Gateway]

  private def onUpdates(updates: Seq[IdentifiedEdgeUpdate]): Unit = {

    logger.debug("updates: " + updates)
    updates.foreach {
      case idUp: IdDataKeyUpdate => {
        if (idUp.id == endpointPath) {
          idUp.data match {
            case Pending =>
            case DataUnresolved =>
            case ResolvedAbsent =>
            case ResolvedValue(update) => {
              if (!connected) {
                eventSink.publishEvent(Seq("configuration", "subscription"), s"Configuration subscription established")
                connected = true
              }

              update.value match {
                case asu: ActiveSetUpdate =>
                  processUpdate(asu.value)
                case other =>
                  logger.warn(s"Got wrong data key type: $other")
              }
            }
            case Disconnected =>
          }
        } else {
          //eventSink.publishEvent(Seq("error", "internal"), s"Unexpected endpoint path in subscription: ${idUp.id}")
          logger.warn(s"Unexpected endpoint path in subscription: ${idUp.id}")
        }
      }
      case other =>
        //eventSink.publishEvent(Seq("error", "internal"), s"Unexpected endpoint path in subscription")
        logger.warn(s"Wrong kind of update in active set subscription: " + other)
    }
  }

  private def processUpdate(value: Map[IndexableValue, Value]): Unit = {

    val map = value.flatMap {
      case (ValueString(key), ValueBytes(data)) => Some((key, data))
      case (otherK, otherV) =>
        logger.warn(s"K/v types were unexpected was other than string: $otherK -> $otherV")
        None
    }

    val removedKeys = current.keySet -- map.keySet

    val modified: Map[String, DNP3Gateway] = map.flatMap {
      case (module, bytes) =>

        val gatewayOpt = try {
          val is = new ByteArrayInputStream(bytes)
          XmlReader.read(is, DnpGatewaySchema.gateway).flatMap { value =>
            DNP3Gateway.read(value, SimpleReaderContext(Vector(RootCtx("DNP3Gateway")))) match {
              case Left(err) =>
                logger.warn(s"Could not parse value object: $err")
                eventSink.publishEvent(Seq("configuration", "subscription"), s"Parse error: $err")
                None
              case Right(obj) => Some(obj)
            }
          }
        } catch {
          case ex: Throwable =>
            logger.warn(s"Parse error: " + ex)
            eventSink.publishEvent(Seq("configuration", "subscription"), s"Parse exception: ${ex.getMessage}")
            None
        }

        gatewayOpt.flatMap { value =>
          current.get(module) match {
            case None =>
              current = current.updated(module, value)
              Some(module -> value)
            case Some(prev) =>
              if (prev != value) {
                current = current.updated(module, value)
                Some(module -> value)
              } else {
                None
              }
          }
        }
    }

    current = current -- removedKeys

    removedKeys.foreach(k => handler.onGatewayRemoved(k))
    modified.foreach {
      case (key, cfg) => handler.onGatewayConfigured(key, cfg)
    }

  }
}
