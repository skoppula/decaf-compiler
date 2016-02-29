package compile.descriptors

import compile.symboltables.ParametersTable

class MethodDescriptor(
    parameterTable : ParametersTable,
    methodInfo: String)
  extends Descriptor {
  val name = "MethodType"

  override def toString: String = {
    methodInfo
  }

  def validate(): Boolean = {
    parameterTable.validate()
  }
}
