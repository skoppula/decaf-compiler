package compile.descriptors

abstract class BaseDescriptor extends Descriptor {}

abstract class PrimitiveBaseDescriptor extends BaseDescriptor

case class IntTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "IntType"
}

case class BoolTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "BoolType"
}

case class VoidTypeDescriptor() extends PrimitiveBaseDescriptor {
  override val name = "VoidType"
}

abstract class ArrayBaseDescriptor(size : Long) extends BaseDescriptor {}

case class IntArrayTypeDescriptor(size : Long) extends ArrayBaseDescriptor(size) {
  override val name = "IntArrayType"
}

case class BoolArrayTypeDescriptor(size : Long) extends ArrayBaseDescriptor(size) {
  override val name = "BoolArrayType"
}

