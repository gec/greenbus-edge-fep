package io.greenbus.edge.tag



sealed trait Element

sealed trait ValueElement extends Element
sealed trait Value extends ValueElement
case class TaggedValue(tag: String, value: Value) extends ValueElement

case class TaggedField(name: String, value: ValueElement) extends Element


sealed trait PrimitiveValue extends Value
case class VByte(value: Byte) extends PrimitiveValue
case class VBool(value: Boolean) extends PrimitiveValue

sealed trait IntegerValue extends PrimitiveValue {
  def toInt: Int
  def toLong: Long
}
case class VInt32(value: Int) extends IntegerValue {
  def toInt: Int = value
  def toLong: Long = value
}
case class VInt64(value: Int) extends IntegerValue {
  def toInt: Int = value
  def toLong: Long = value
}
case class VUInt32(value: Long) extends IntegerValue {
  def toInt: Int = value.toInt
  def toLong: Long = value
}
case class VUInt64(value: Long) extends IntegerValue {
  def toInt: Int = value.toInt
  def toLong: Long = value
}

sealed trait FloatingPointValue extends PrimitiveValue {
  def toFloat: Float
  def toDouble: Double
}
case class VFloat(value: Float) extends FloatingPointValue {
  def toFloat: Float = value
  def toDouble: Double = value.toDouble
}
case class VDouble(value: Double) extends FloatingPointValue {
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
}


sealed trait BasicValue extends Value
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

case class VString(value: String) extends Value
//case class VSymbol(value: String) extends Value

case class VTuple(value: IndexedSeq[Element]) extends BasicValue // ???

sealed trait StructuralValue

case object VNone extends StructuralValue

//case class VTuple(value: IndexedSeq[Element]) extends StructuralValue // ???
case class VList(value: IndexedSeq[Value]) extends StructuralValue
case class VMap(value: Map[Value, Value]) extends StructuralValue

// ==============================

case class FieldDef(fieldName: String, fieldType: VType)

sealed trait VType
sealed trait VTypePrimitive extends VType

case object VTByte extends VTypePrimitive
case object VTBool extends VTypePrimitive
case object VTInt32 extends VTypePrimitive
case object VTInt64 extends VTypePrimitive
case object VTUInt32 extends VTypePrimitive
case object VTUInt64 extends VTypePrimitive
case object VTFloat extends VTypePrimitive
case object VTDouble extends VTypePrimitive

case object VTString extends VType
//case object VTSymbol extends VType

//case object VTNone extends VType
case class VTUnion(unionTypes: Set[VType]) extends VType
case class VTOption(paramType: VType) extends VType
case class VTEither(leftType: VType, rightType: VType) extends VType

case class VTTuple(elementTypes: IndexedSeq[FieldDef]) extends VType
//case class VTTuple(elementTypes: IndexedSeq[VType]) extends VType
case class VTList(paramType: VType) extends VType
case class VTMap(keyType: VType, valueType: VType) extends VType

case class VTExtType(tag: String, reprType: VType) extends VType

//case class TaggedType(tag: String, typ: VType)

// ===============================


sealed trait SchemaReadError
trait VSchemaReader[A] {
  //def expectingTag: String
  //def expectingValueType: VType

  def read(elem: Element): Either[SchemaReadError, A]
}



object SchemalessReader extends VSchemaReader[Element] {
  def read(elem: Element): Either[SchemaReadError, Element] = {
    Right(elem)
  }
}


trait VReader {
  //def read[A](element: Element, schema: VSchema[A]): Either[VReadError, A]
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


//sealed trait

/*
object Path {
  def isPrefixOf(l: Path, r: Path): Boolean = {
    val lIter = l.parts.iterator
    val rIter = r.parts.iterator
    var okay = true
    var continue = true
    while (continue) {
      (lIter.hasNext, rIter.hasNext) match {
        case (true, true) =>
          if (lIter.next() != rIter.next()) {
            okay = false
            continue = false
          }
        case (false, true) => continue = false
        case (true, false) =>
          okay = false; continue = false
        case (false, false) => continue = false
      }
    }
    okay
  }

  def apply(part: String): Path = {
    Path(Seq(part))
  }
}
case class Path(parts: Seq[String])

sealed trait Value

sealed trait IndexableValue extends Value

sealed trait NumericConvertible extends IndexableValue {
  def toDouble: Double
  def toLong: Long
  def toBoolean: Boolean
}

sealed trait SampleValue extends NumericConvertible

object IntegerValue {
  def unapply(v: IntegerValue): Option[Long] = {
    Some(v.toLong)
  }
}
sealed trait IntegerValue extends NumericConvertible

object FloatingPointValue {
  def unapply(v: FloatingPointValue): Option[Double] = {
    Some(v.toDouble)
  }
}
sealed trait FloatingPointValue extends NumericConvertible

case class ValueFloat(v: Float) extends FloatingPointValue with SampleValue {
  def toDouble: Double = v.toDouble
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0.0
}
case class ValueDouble(v: Double) extends FloatingPointValue with SampleValue {
  def toDouble: Double = v
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0.0
}
case class ValueInt32(v: Int) extends IntegerValue with SampleValue {
  def toDouble: Double = v.toDouble
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0
}
case class ValueUInt32(v: Long) extends IntegerValue with SampleValue {
  def toDouble: Double = v.toDouble
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0
}
case class ValueInt64(v: Long) extends IntegerValue with SampleValue {
  def toDouble: Double = v.toDouble
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0
}
case class ValueUInt64(v: Long) extends IntegerValue with SampleValue {
  def toDouble: Double = v.toDouble
  def toLong: Long = v.toLong
  def toBoolean: Boolean = v != 0
}
case class ValueBool(v: Boolean) extends NumericConvertible with SampleValue {
  def toDouble: Double = if (v) 1.0 else 0.0
  def toLong: Long = if (v) 1 else 0
  def toBoolean: Boolean = v
}
case class ValueString(v: String) extends IndexableValue
case class ValueUuid(v: UUID) extends IndexableValue
case class ValuePath(v: Path) extends Value
case class ValueEndpointPath(v: EndpointPath) extends Value
case class ValueText(v: String, mimeType: Option[String] = None) extends Value

case class ValueArray(seq: IndexedSeq[Value]) extends Value
case class ValueObject(map: Map[String, Value]) extends Value

case class ValueBytes(v: Array[Byte]) extends IndexableValue {

  override def equals(r: Any): Boolean = {
    r match {
      case rv: ValueBytes => util.Arrays.equals(v, rv.v)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(v)
  }
}
case class ValueAnnotatedBytes(v: Array[Byte], mimeType: Option[String] = None, isText: Option[Boolean] = None) extends Value {

  override def equals(r: Any): Boolean = {
    r match {
      case rv: ValueAnnotatedBytes => util.Arrays.equals(v, rv.v) && rv.mimeType == mimeType && rv.isText == isText
      case _ => false
    }
  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(v) * mimeType.hashCode() * isText.hashCode()
  }
}
 */

