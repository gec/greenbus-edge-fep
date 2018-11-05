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

import io.greenbus.edge.data.SampleValue

trait MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit
}

class SplittingMeasObserver(observers: Seq[MeasObserver]) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    observers.foreach(_.flush(batch))
  }
}

class FrontendAdapter(handle: FrontendPublisher) extends MeasObserver {
  def flush(batch: Seq[(String, SampleValue)]): Unit = {
    handle.batch(batch)
  }
}

trait EventSink {
  def publishEvent(topic: Seq[String], event: String): Unit
}

class SplittingKeyedObserver(observers: Seq[KeyedDeviceObserver]) extends KeyedDeviceObserver {
  def handleOnline(): Unit = observers.foreach(_.handleOnline())

  def handleOffline(): Unit = observers.foreach(_.handleOffline())

  def handleBatch(batch: Seq[(String, SampleValue)]): Unit = {
    observers.foreach(_.handleBatch(batch))
  }

  def close(): Unit = observers.foreach(_.close())
}