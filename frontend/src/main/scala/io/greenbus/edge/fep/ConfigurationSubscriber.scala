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
package io.greenbus.edge.fep

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.api._
import io.greenbus.edge.data.{ IndexableValue, Value, ValueString }
import io.greenbus.edge.thread.CallMarshaller

trait ConfigurationHandler[A] {
  def onConfigured(key: String, config: A): Unit
  def onRemoved(key: String)
}

class ConfigurationSubscriber[A](
    eventThread: CallMarshaller,
    consumerServices: ConsumerService,
    endpointPath: EndpointPath,
    parser: Value => Either[String, A],
    handler: ConfigurationHandler[A],
    eventSink: EventSink) extends LazyLogging {

  private var connected = false

  private val sub = consumerServices.subscriptionClient.subscribe(
    SubscriptionParams(dataKeys =
      Set(endpointPath)))

  sub.updates.bind(updates => eventThread.marshal {
    onUpdates(updates)
  })

  private var current = Map.empty[String, A]

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

    val map: Map[String, Value] = value.flatMap {
      case (ValueString(key), v) => Some((key, v))
      case (otherK, otherV) =>
        logger.warn(s"K/v types were unexpected was other than string: $otherK -> $otherV")
        None
    }

    val removedKeys = current.keySet -- map.keySet

    val modified: Map[String, A] = map.flatMap {
      case (module, configValue) =>
        val parsedObjOpt = try {
          parser(configValue) match {
            case Left(err) =>
              logger.warn(s"Could not parse value object: $err")
              eventSink.publishEvent(Seq("configuration", "subscription"), s"Parse error: $err")
              None
            case Right(obj) =>
              Some(obj)
          }
        } catch {
          case ex: Throwable =>
            logger.warn(s"Parse error: " + ex)
            eventSink.publishEvent(Seq("configuration", "subscription"), s"Parse exception: ${ex.getMessage}")
            None
        }

        parsedObjOpt.flatMap { obj =>
          current.get(module) match {
            case None =>
              current = current.updated(module, obj)
              Some(module -> obj)
            case Some(prev) =>
              if (prev != obj) {
                current = current.updated(module, obj)
                Some(module -> obj)
              } else {
                None
              }
          }
        }
    }

    removedKeys.foreach(k => handler.onRemoved(k))
    modified.foreach {
      case (key, cfg) => handler.onConfigured(key, cfg)
    }
  }
}
