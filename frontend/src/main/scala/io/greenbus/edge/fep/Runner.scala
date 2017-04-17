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

import io.greenbus.edge.api.proto.EndpointId
import io.greenbus.edge.api.{ EndpointDescriptor, Path }
import io.greenbus.edge.data._
import io.greenbus.edge.fep.model._
import io.greenbus.edge.peer.{ AmqpEdgeService, ProducerServices }

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

/*

key ->
  transforms[]
  filter
  config >
    data key
    result type
    unit
    indexes >
      string -> value
    metadata >
      string -> value

 */

/*

types:
-----------
bool
analog value (int/double)
boolean -> label
int -> label

filters:
-----------
disable de-duplication
deadband

transforms:
------------
scale integer/real
flip bool
cast to float
cast to integer
int (test) -> boolean

modbus:
-------------
cast to (byte ordering)
mask -> int



decorate with kvs:
--------------
unit
user def string -> simple value


commands:
-----------
indication
setpoint (bool, int, float)

 */

object Runner {

  def main(args: Array[String]): Unit = {
    println("hello")

    val services = AmqpEdgeService.build("127.0.0.1", 50001, 10000)
    services.start()
    val producerServices = services.producer

  }
}

trait GatewayOutputAcceptor {
  def accept()
}

trait FrontendProcessor {
  def batch(batch: Seq[(String, SampleValue)])
}

