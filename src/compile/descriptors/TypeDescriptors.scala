package compile.descriptors

abstract class BaseDescriptor extends Descriptor {}

abstract class ArrayBaseDescriptor(size : Int) extends BaseDescriptor {}

class IntTypeDescriptor extends BaseDescriptor {
  override val name = "IntType"
}

class BoolTypeDescriptor extends BaseDescriptor {
  override val name = "BoolType"
}

class VoidTypeDescriptor extends BaseDescriptor {
  override val name = "VoidType"
}

class IntArrayTypeDescriptor(val size : Int) extends ArrayBaseDescriptor(size) {
  override val name = "IntArrayType"
}

class BoolArrayTypeDescriptor(val size : Int) extends ArrayBaseDescriptor(size) {
  override val name = "BoolArrayType"
}

