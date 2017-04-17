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

import io.greenbus.edge.data._
import io.greenbus.edge.fep.model.SampleType

trait TransformStep {
  def handle(v: SampleValue): Option[SampleValue]
}

object CastStep {
  def load(sampleType: SampleType): CastStep = {
    val f = sampleType match {
      case SampleType.Bool => (v: SampleValue) => { ValueBool(v.toBoolean) }
      case SampleType.Byte => (v: SampleValue) => { ValueByte(v.toLong.toByte) }
      case SampleType.Int32 => (v: SampleValue) => { ValueInt32(v.toLong.toInt) }
      case SampleType.UInt32 => (v: SampleValue) => { ValueUInt32(v.toLong) }
      case SampleType.Int64 => (v: SampleValue) => { ValueInt64(v.toLong) }
      case SampleType.UInt64 => (v: SampleValue) => { ValueUInt64(v.toLong) }
      case SampleType.Float => (v: SampleValue) => { ValueFloat(v.toDouble.toFloat) }
      case SampleType.Double => (v: SampleValue) => { ValueDouble(v.toDouble) }
    }
    new CastStep(f)
  }
}
class CastStep(fun: SampleValue => SampleValue) extends TransformStep {
  def handle(v: SampleValue): Option[SampleValue] = {
    Some(fun(v))
  }
}

class LinearTransformStep(scale: Double, offset: Double) extends TransformStep {
  def handle(v: SampleValue): Option[SampleValue] = {
    val result = v match {
      case ValueFloat(value) => ValueFloat((value * scale + offset).toFloat)
      case ValueDouble(value) => ValueDouble(value * scale + offset)
      case ValueInt32(value) => ValueInt32((value * scale + offset).toInt)
      case ValueUInt32(value) => ValueUInt32((value * scale + offset).toInt)
      case ValueInt64(value) => ValueInt64((value * scale + offset).toLong)
      case ValueUInt64(value) => ValueUInt64((value * scale + offset).toLong)
      case ValueBool(value) =>
        val v = if (value) 1 else 0
        ValueBool((v * scale + offset) != 0.0)
      case ValueByte(value) => ValueByte((value.toDouble * scale + offset).toByte)
    }
    Some(result)
  }
}

class NegateBoolStep extends TransformStep {
  def handle(v: SampleValue): Option[SampleValue] = {
    v match {
      case ValueBool(value) => Some(ValueBool(!value))
      case _ => Some(v)
    }
  }
}

class MatchStep(matchFun: SampleValue => Boolean) extends TransformStep {
  def handle(v: SampleValue): Option[SampleValue] = {
    Some(ValueBool(matchFun(v)))
  }
}

class NoDuplicateFilter extends TransformStep {
  private var prevOpt = Option.empty[SampleValue]

  def handle(v: SampleValue): Option[SampleValue] = {
    prevOpt match {
      case None =>
        prevOpt = Some(v)
        Some(v)
      case Some(prev) => {
        if (v != prev) {
          prevOpt = Some(v)
          Some(v)
        } else {
          None
        }
      }
    }
  }
}

class DeadbandFilter(deadband: Double) extends TransformStep {
  private var stickingPoint = Option.empty[Double]

  def handle(v: SampleValue): Option[SampleValue] = {
    stickingPoint match {
      case None =>
        stickingPoint = Some(v.toDouble)
        Some(v)
      case Some(prevStuck) => {
        if (Math.abs(v.toDouble - prevStuck) >= deadband) {
          stickingPoint = Some(v.toDouble)
          Some(v)
        } else {
          None
        }
      }
    }
  }
}