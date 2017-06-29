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
import io.greenbus.edge.data.{ Value, ValueBytes }
import io.greenbus.edge.data.json.EdgeJsonReader
import org.apache.commons.io.IOUtils
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ ServletContextHandler, ServletHolder }

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

object WebServer {

  def build(handler: ModuleConfigurer, port: Int): Server = {
    val server = new Server(port)
    val context = new ServletContextHandler()
    context.setContextPath("/")

    val holder = new ServletHolder(new AsyncServlet(handler: ModuleConfigurer))
    holder.setAsyncSupported(true)
    context.addServlet(holder, "/")

    server.setHandler(context)
    server
  }
}

class AsyncServlet(handler: ModuleConfigurer) extends HttpServlet with LazyLogging {
  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    logger.debug("POST: " + req)
    logger.debug("content length: " + req.getContentLength)

    logger.debug(s"Path: " + req.getRequestURI)
    val uri = req.getRequestURI

    val bytes = IOUtils.readFully(req.getInputStream, req.getContentLength)

    val namesEnum = req.getHeaderNames
    while (namesEnum.hasMoreElements) {
      val name = namesEnum.nextElement()
      logger.debug(name + " : " + req.getHeader(name))
    }

    val moduleOpt = Option(req.getHeader("EdgeModule"))
    val componentOpt = Option(req.getHeader("EdgeComponent"))
    val nodeOpt = Option(req.getHeader("EdgeNode"))

    val cfgOpt = for {
      module <- moduleOpt
      component <- componentOpt
    } yield {
      (module, component)
    }

    cfgOpt match {
      case None => resp.setStatus(HttpServletResponse.SC_BAD_REQUEST)
      case Some((module, component)) => {

        val valueOpt: Option[Value] = if (uri.startsWith("/json")) {
          logger.debug(s"JSON: ")
          try {
            EdgeJsonReader.read(new ByteArrayInputStream(bytes))
          } catch {
            case ex: Throwable =>
              logger.warn(s"Could not parse configuration: $ex")
              None
          }
        } else {
          Some(ValueBytes(bytes))
        }

        valueOpt match {
          case None => {
            resp.setStatus(HttpServletResponse.SC_BAD_REQUEST)
          }
          case Some(v) =>
            val ctx = req.startAsync()
            val prom = Promise[Boolean]
            handler.updateModule(module, ModuleConfiguration(Map(component -> (v, nodeOpt))), prom)

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