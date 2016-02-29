package compile.descriptors

import compile.symboltables.ParametersTable
import util.CLI
import scala.collection.mutable.ArrayBuffer

abstract class MethodDescriptor(
    signature : ArrayBuffer[BaseDescriptor],
    parameterTable : ParametersTable,
    methodInfo: String)
  extends Descriptor {
  val name = "MethodType"

  override def toString: String = {
    methodInfo
  }

  def validate(): Boolean = {
    /**
      * - Recursively checks that local symbol tables are valid
      * - Ensure parameter table type matches signature type (TODO)
      */
    if(signature.size != parameterTable.getParametersTable.size) {
      if (CLI.irdebug)
        println("Failed signature and parameters table consistency check")
      false
    } else {
      parameterTable.validate()
    }
  }
}
