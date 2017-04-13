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
import org.totalgrid.dnp3._

class Adapter2 {

  private val stackManager = new StackManager
  private val logAdapter = new LogAdapter

  stackManager.AddLogHook(logAdapter)
}

class MeasAdapter() extends IDataObserver with LazyLogging {

  import SafeExecution._

  private var wallTime = Option.empty[Long]
  //private var batch = List.empty[(String, Measurement)]

  override def _Start(): Unit = safeExecute {
    wallTime = Some(System.currentTimeMillis())
  }

  override def _End(): Unit = safeExecute {
    /*if (batch.size > 0) {
      val time = wallTime.getOrElse(System.currentTimeMillis())
      val current = batch.reverse
      batch = Nil
      wallTime = None
      accept(time, current)
    }*/
  }

  /*private def add(index: Long, map: Map[Long, String], typ: String, meas: => Measurement) {
    map.get(index) match {
      case None => //logger.debug("Unknown type/index: " + typ + "/" + index)
      case Some(name) => batch ::= (name, meas)
    }
  }*/

  override def _Update(v: Binary, index: Long): Unit = safeExecute {
    //add(index, mapping.binaries, "Binary", DNPTranslator.translate(v))
  }

  override def _Update(v: Analog, index: Long) = safeExecute {
    //add(index, mapping.analogs, "Analog", DNPTranslator.translate(v))
  }

  override def _Update(v: Counter, index: Long) = safeExecute {
    //add(index, mapping.counters, "Counter", DNPTranslator.translate(v))
  }

  override def _Update(v: SetpointStatus, index: Long) = safeExecute {
    //add(index, mapping.setpointStatuses, "SetpointStatus", DNPTranslator.translate(v))
  }

  override def _Update(v: ControlStatus, index: Long) = safeExecute {
    //add(index, mapping.controlStatuses, "ControlStatus", DNPTranslator.translate(v))
  }
}