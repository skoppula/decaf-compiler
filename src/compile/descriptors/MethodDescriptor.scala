package compile.descriptors

import compile.symboltables.ParametersTable
import compile.descriptors.TypeDescriptors
import scala.collection.mutable

class MethodDescriptor(
    parameterTable : ParametersTable,
    methodName: String,
    returnType : PrimitiveBaseDescriptor)
  extends Descriptor {

  val name = methodName
  val methodType = returnType

  override def toString: String = {
    "Method(" + methodName + "," + returnType.toString + ")"
  }

  def getParamTable : mutable.LinkedHashMap[String, PrimitiveBaseDescriptor]  = parameterTable.getParametersTable

  def getTotalByteSize(): Int = {
    return parameterTable.getTotalByteSize()
  }

  def computeOffsets() {
    parameterTable.computeOffsets()
  }


}
