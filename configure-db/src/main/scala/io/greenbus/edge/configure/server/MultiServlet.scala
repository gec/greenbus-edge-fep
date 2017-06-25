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
package io.greenbus.edge.configure.server

import java.io.ByteArrayInputStream
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import com.typesafe.scalalogging.LazyLogging
import io.greenbus.edge.configure.endpoint.{ ModuleConfiguration, ModuleConfigurer }
import io.greenbus.edge.data._
import io.greenbus.edge.data.json.EdgeJsonReader
import io.greenbus.edge.util.EitherUtil
import org.apache.commons.io.IOUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise

object MultiServlet {

  def matchString(v: Value): Option[String] = {
    v match {
      case ValueString(s) => Some(s)
      case _ => None
    }
  }

  def handleComponentValueList(value: Value): Either[String, ModuleConfiguration] = {
    value match {
      case list: ValueList =>
        val entries = list.value.map {
          case map: ValueMap =>
            val componentOpt = map.value.get(ValueString("component")).flatMap(matchString)
            val nodeOpt = map.value.get(ValueString("node")).flatMap(matchString)
            val valueOpt = map.value.get(ValueString("value"))

            val tupOpt = for {
              component <- componentOpt
              v <- valueOpt
            } yield (component, v)

            tupOpt match {
              case Some((component, v)) =>
                Right((component, v, nodeOpt))
              case None => Left("Could not extract component and value.")
            }

          case _ => Left(s"List value was not a map of parameters.")
        }

        val all = EitherUtil.rightSequence(entries)

        all.map { results =>
          ModuleConfiguration(results.map {
            case (component, v, nodeOpt) => (component, (v, nodeOpt))
          }.toMap)
        }
      case _ => Left(s"Value was not a list.")
    }
  }
}
class MultiServlet(handler: ModuleConfigurer) extends HttpServlet with LazyLogging {

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    logger.debug("Multi POST: " + req)
    logger.debug("content length: " + req.getContentLength)

    logger.debug(s"Path: " + req.getRequestURI)

    val bytes = IOUtils.readFully(req.getInputStream, req.getContentLength)

    val namesEnum = req.getHeaderNames
    while (namesEnum.hasMoreElements) {
      val name = namesEnum.nextElement()
      logger.debug(name + " : " + req.getHeader(name))
    }

    val moduleOpt = Option(req.getHeader("EdgeModule"))

    moduleOpt match {
      case None => resp.setStatus(HttpServletResponse.SC_BAD_REQUEST)
      case Some(module) => {

        val valueOpt: Option[Value] = {
          try {
            EdgeJsonReader.read(new ByteArrayInputStream(bytes))
          } catch {
            case ex: Throwable =>
              logger.warn(s"Could not parse configuration: $ex")
              None
          }
        }

        println(valueOpt)

        valueOpt match {
          case None => {
            resp.setStatus(HttpServletResponse.SC_BAD_REQUEST)
          }
          case Some(v) =>
            MultiServlet.handleComponentValueList(v) match {
              case Left(err) =>
                logger.debug(s"Got error: $err")
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, err)
              case Right(config) =>
                logger.trace(s"Got config: $config")

                val ctx = req.startAsync()
                val prom = Promise[Boolean]
                handler.updateModule(module, config, prom)

                val future = prom.future

                future.foreach { result =>
                  ctx.getResponse match {
                    case r: HttpServletResponse =>
                      if (result) {
                        r.setStatus(HttpServletResponse.SC_OK)
                      } else {
                        r.setStatus(HttpServletResponse.SC_BAD_REQUEST)
                      }
                    case _ =>
                  }
                  ctx.complete()
                }
                future.failed.foreach { ex =>
                  ctx.getResponse match {
                    case r: HttpServletResponse =>
                      r.sendError(HttpServletResponse.SC_BAD_REQUEST, ex.getMessage)
                    case _ =>
                  }
                  ctx.complete()
                }
            }
            resp.setStatus(HttpServletResponse.SC_OK)
        }
      }
    }

  }

  override def doDelete(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    logger.debug("DELETE: " + req)

    val namesEnum = req.getHeaderNames
    while (namesEnum.hasMoreElements) {
      val name = namesEnum.nextElement()
      logger.debug(name + " : " + req.getHeader(name))
    }

    val moduleOpt = Option(req.getHeader("EdgeModule"))

    moduleOpt match {
      case None => resp.setStatus(HttpServletResponse.SC_BAD_REQUEST)
      case Some(module) => {

        val ctx = req.startAsync()
        val prom = Promise[Boolean]
        handler.removeModule(module, prom)

        val future = prom.future

        future.foreach { result =>
          ctx.getResponse match {
            case r: HttpServletResponse =>
              if (result) {
                r.setStatus(HttpServletResponse.SC_OK)
              } else {
                r.setStatus(HttpServletResponse.SC_BAD_REQUEST)
              }
            case _ =>
          }
          ctx.complete()
        }
        future.failed.foreach { ex =>
          ctx.getResponse match {
            case r: HttpServletResponse =>
              r.sendError(HttpServletResponse.SC_BAD_REQUEST, ex.getMessage)
            case _ =>
          }
          ctx.complete()
        }
      }
    }

  }
}
