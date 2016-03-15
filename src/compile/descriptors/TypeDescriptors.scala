package compile.descriptors

// offsetBytes will remain null for GlobalFieldTable entries
//  because global variables go into .DATA section and do not need an offset
abstract class BaseDescriptor extends Descriptor {
  var offsetBytes : Int = 2000 // Random null value
}

abstract class PrimitiveBaseDescriptor extends BaseDescriptor

case class IntTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "IntType"
  val sizeBytes : Int = 8
}

case class BoolTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "BoolType"
  val sizeBytes : Int = 8
}

case class VoidTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "VoidType"
}

abstract class ArrayBaseDescriptor(size : BigInt) extends BaseDescriptor {
    val length = size
}

case class IntArrayTypeDescriptor(size : BigInt) extends ArrayBaseDescriptor(size) {
  override val name = "IntArrayType"
  val sizeBytes : Int = 8*size.toInt
}

case class BoolArrayTypeDescriptor(size : BigInt) extends ArrayBaseDescriptor(size) {
  override val name = "BoolArrayType"
  val sizeBytes : Int = 8*size.toInt
}

