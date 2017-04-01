package io.greenbus.edge.tag

class Garbage {

}



/*
sealed trait Element

sealed trait Value extends Element
case class TaggedValue(tag: String, value: Value) extends Element


sealed trait PrimitiveValue extends Value
case class VByte(value: Byte) extends PrimitiveValue
case class VBool(value: Boolean) extends PrimitiveValue
case class VInt32(value: Int) extends PrimitiveValue
case class VInt64(value: Int) extends PrimitiveValue
case class VUInt32(value: Long) extends PrimitiveValue
case class VUInt64(value: Long) extends PrimitiveValue
case class VFloat(value: Float) extends PrimitiveValue
case class VDouble(value: Double) extends PrimitiveValue

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

case class VTuple(value: IndexedSeq[Element]) extends BasicValue // ???

sealed trait StructuralValue

case object VNone extends StructuralValue

//case class VTuple(value: IndexedSeq[Element]) extends StructuralValue // ???
case class VList(value: IndexedSeq[Element]) extends StructuralValue
case class VMap(value: Map[Element, Element]) extends StructuralValue

// ==============================

sealed trait VType
sealed trait VTypePrimitive

case object VTByte extends VTypePrimitive


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
*/
