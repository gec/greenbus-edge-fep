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

import com.typesafe.scalalogging.{ LazyLogging, Logger }
import org.slf4j.LoggerFactory
import org.totalgrid.dnp3.{ FilterLevel, ILogBase, LogEntry }

class LogAdapter extends ILogBase with LazyLogging {

  //import SafeExecution._

  // DNPLOG is common logger name for all dnp log messages
  val customLogger = Logger(LoggerFactory.getLogger(classOf[LogAdapter]))

  final override def SetVar(source: String, variable: String, value: Int) {}

  final override def Log(entry: LogEntry): Unit = safeExecute {

    def getMsg: String = entry.GetFilterLevel + " - " + entry.GetDeviceName + " - " + entry.GetMessage

    // we need to upgrade all of the messages up to a usable logging level, we let the
    // filter settings we pass into the stack determine how much info we see
    entry.GetFilterLevel match {
      case FilterLevel.LEV_COMM => customLogger.info(getMsg)
      case FilterLevel.LEV_DEBUG => customLogger.info(getMsg)
      case FilterLevel.LEV_ERROR => customLogger.error(getMsg)
      case FilterLevel.LEV_EVENT => customLogger.error(getMsg)
      case FilterLevel.LEV_INFO => customLogger.info(getMsg)
      case FilterLevel.LEV_INTERPRET => customLogger.info(getMsg)
      case FilterLevel.LEV_WARNING => customLogger.warn(getMsg)
      case _ => customLogger.error(getMsg)
    }

  }

  def safeExecute[A](fun: => A) {
    try {
      fun
    } catch {
      case e: Exception =>
        logger.error(e.getMessage(), e)
        logger.error(e.getStackTraceString)
    }
  }
}