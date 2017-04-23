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
import io.greenbus.edge.fep.config.model.SimpleTransformType.Negate
import io.greenbus.edge.fep.config.model._
import io.greenbus.edge.fep.model._

import scala.annotation.tailrec

object KeyProcessor {

  def load(transforms: Seq[TransformDescriptor],
    filterOpt: Option[FilterDescriptor]): KeyProcessor = {

    val transSteps = transforms.map {
      case LinearTransform(scale, offset) => new LinearTransformStep(scale, offset)
      case TypeCast(sampleType) => CastStep.load(sampleType)
      case SimpleTransform(Negate) => new NegateBoolStep
    }

    val filtOpt = (filterOpt.flatMap(_.suppressDuplicates), filterOpt.flatMap(_.deadband)) match {
      case (_, Some(dead)) => Some(new DeadbandFilter(dead))
      case (Some(suppress), None) =>
        if (suppress) Some(new NoDuplicateFilter) else None
      case (None, None) => Some(new NoDuplicateFilter)
    }

    val steps = transSteps ++ Seq(filtOpt).flatten
    new KeyProcessorImpl(steps)
  }

}
trait KeyProcessor {
  def process(value: SampleValue): Option[SampleValue]
}
object KeyProcessorImpl {

  @tailrec
  def process(v: SampleValue, itr: Iterator[TransformStep]): Option[SampleValue] = {
    if (itr.hasNext) {
      val step = itr.next()
      val resultOpt = step.handle(v)
      resultOpt match {
        case None => None
        case Some(result) => process(result, itr)
      }
    } else {
      Some(v)
    }
  }
}
class KeyProcessorImpl(steps: Seq[TransformStep]) extends KeyProcessor {
  def process(value: SampleValue): Option[SampleValue] = {
    KeyProcessorImpl.process(value, steps.iterator)
  }
}
