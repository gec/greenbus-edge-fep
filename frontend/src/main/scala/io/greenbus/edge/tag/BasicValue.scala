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
package io.greenbus.edge.tag

/*

  elem = tagged_value | value
  value = none | prim | tuple | list | map
  field = tagged_field | elem
  tuple = { field }
  list = { elem }
  map = { elem, elem }

 */
/*sealed trait Element
case class TaggedField(name: String, value: Value) extends Element*/

//sealed trait ValueElement extends Element
sealed trait Value //extends Element
case class TaggedValue(tag: String, value: BasicValue) extends Value //ValueElement
sealed trait BasicValue extends Value //ValueElement

sealed trait PrimitiveValue extends BasicValue
case class ValueByte(value: Byte) extends PrimitiveValue
case class ValueBool(value: Boolean) extends PrimitiveValue

sealed trait IntegerValue extends PrimitiveValue {
  def toInt: Int
  def toLong: Long
}
case class ValueInt32(value: Int) extends IntegerValue {
  def toInt: Int = value
  def toLong: Long = value
}
case class ValueUInt32(value: Int) extends IntegerValue {
  def toInt: Int = value
  def toLong: Long = value
}
case class ValueInt64(value: Long) extends IntegerValue {
  def toInt: Int = value.toInt
  def toLong: Long = value
}
case class ValueUInt64(value: Long) extends IntegerValue {
  def toInt: Int = value.toInt
  def toLong: Long = value
}

sealed trait FloatingPointValue extends PrimitiveValue {
  def toFloat: Float
  def toDouble: Double
}
case class ValueFloat(value: Float) extends FloatingPointValue {
  def toFloat: Float = value
  def toDouble: Double = value.toDouble
}
case class ValueDouble(value: Double) extends FloatingPointValue {
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
}

case class VArrayByte(value: Array[Byte]) extends BasicValue
case class VArrayBool(value: Array[Boolean]) extends BasicValue
case class VArrayInt32(value: Array[Int]) extends BasicValue
case class VArrayInt64(value: Array[Int]) extends BasicValue
case class VArrayUInt32(value: Array[Long]) extends BasicValue
case class VArrayUInt64(value: Array[Long]) extends BasicValue
case class VArrayFloat(value: Array[Float]) extends BasicValue
case class VArrayDouble(value: Array[Double]) extends BasicValue

// FIELD as a value, taggable like tag?

/*sealed trait Field
case class Field extends Value()*/

case class ValueString(value: String) extends BasicValue
//case class VSymbol(value: String) extends Value

//case class VStruct(value: Seq[TaggedField]) extends BasicValue // ???

sealed trait StructuralValue extends BasicValue

case object ValueNone extends StructuralValue

//case class VTuple(value: IndexedSeq[Element]) extends StructuralValue // ???
case class ValueList(value: IndexedSeq[Value]) extends StructuralValue
case class ValueMap(value: Map[Value, Value]) extends StructuralValue

// ==============================

sealed trait ValueType
case class VTField(fieldName: String, fieldType: VTValueElem) extends ValueType

sealed trait VTValueElem extends ValueType
case class TExt(tag: String, reprType: BasicValueType) extends VTValueElem

sealed trait BasicValueType extends VTValueElem
sealed trait PrimitiveValueType extends BasicValueType

case object TByte extends PrimitiveValueType
case object TBool extends PrimitiveValueType
case object TInt32 extends PrimitiveValueType
case object TInt64 extends PrimitiveValueType
case object TUInt32 extends PrimitiveValueType
case object TUInt64 extends PrimitiveValueType
case object TFloat extends PrimitiveValueType
case object TDouble extends PrimitiveValueType

case object TString extends BasicValueType
//case object VTSymbol extends VType

//case object VTNone extends VType
case class TUnion(unionTypes: Set[VTValueElem]) extends BasicValueType
case class TOption(paramType: VTValueElem) extends BasicValueType
case class TEither(leftType: VTValueElem, rightType: VTValueElem) extends BasicValueType

case class StructFieldDef(name: String, typ: VTValueElem, number: Int)
case class TStruct(fields: Seq[StructFieldDef]) extends BasicValueType
//case class VTTuple(elementTypes: IndexedSeq[VType]) extends VType
case class TList(paramType: VTValueElem) extends BasicValueType
case class TMap(keyType: VTValueElem, valueType: VTValueElem) extends BasicValueType

//case class TaggedType(tag: String, typ: VType)

// ===============================

sealed trait SchemaReadError
trait VSchemaReader[A] {
  //def expectingTag: String
  //def expectingValueType: VType

  def read(elem: Value): Either[SchemaReadError, A]
}

object SchemalessReader extends VSchemaReader[Value] {
  def read(elem: Value): Either[SchemaReadError, Value] = {
    Right(elem)
  }
}

/*

schema/type constructs:

Array[T] : basic array or list
Option : some value in place or none in place
Either : value in place
Field (symbol -> element type) : VStruct, arity 2
Struct (ordered? list of fields) : VMap or VStruct of fields??

 */

/*

recursively composable "base" models? how?
the host type system natively gives you things like UUIDVal

represent UUID on the wire two different ways? probably not


primitives
specialized primitives (timestamps, etc.)

arrays
typed arrays
specialized typed arrays (string)

polymorphic aggregates (list, map)



strings/byte array vs. array

[primitives] =
bool
(s)int 16/32/64
float/double
byte


BASE MODEL:
--------------
bool
(s)int 16/32/64
float/double
byte
tuple

C PROG MODEL:
----------
bool
(s)int 16/32/64
float/double
byte
array[simple]
struct



??? MODEL:
-----------
[primitives]

none

array[T]
tuple
poly list
poly map

SCHEMA:

option
either
choice?

symbol?

array[T]


--------------

goals:
- interop
- extensibility
- mediate between (different) efficient wire representations and rich user models
- schemas (extensible)
- code gen
- be sympathetic to memory/data layouts


Forms:
- Context free values
  - everything type tagged
- schema instanced
  - raw structured values



wire format
wire format parser (may memoize, e.g.)



context + data = structure

type tags may come from context OR data


consumer platform-specific user library -> encoder -> wire format -> decoder -> second library


array
object

 */
