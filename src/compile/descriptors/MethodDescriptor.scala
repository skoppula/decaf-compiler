package compile.descriptors

import compile.symboltables.ParametersTable

class MethodDescriptor(
    parameterTable : ParametersTable,
    methodName: String,
    returnType : BaseDescriptor)
  extends Descriptor {
  val name = "MethodType"

  override def toString: String = {
    "Method(" + methodName + "," + returnType.toString + ")"
  }

  def validate(): Boolean = {
    parameterTable.validate()
  }
}
