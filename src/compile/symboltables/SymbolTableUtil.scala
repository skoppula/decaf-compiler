package compile.symboltables

import compile.descriptors._
import scala.collection.mutable.ListBuffer

object SymbolTableUtil {
  def typeDescriptorToString(descriptor : BaseDescriptor): String = {
    descriptor match {
      case it : IntTypeDescriptor => return "IntType(offset=" + it.offsetBytes.toString + ")"
      case bt : BoolTypeDescriptor => return "BoolType(offset=" + bt.offsetBytes.toString + ")"
      case bta : BoolArrayTypeDescriptor => return "BoolArrayType(offset=" + bta.offsetBytes.toString + ", length=" + bta.length + ")"
      case ita : IntArrayTypeDescriptor => return "IntArrayType(offset=" + ita.offsetBytes.toString + ", length=" + ita.length + ")"
    }
  }

  def symbolTableToString(table : SymbolTable) : ListBuffer[String] = {
    val outStrings : ListBuffer[String] = ListBuffer.empty[String]
    outStrings += "New scope: " + table.scopeType.toString
    outStrings += "Field declarations:"
    for((name, descriptor) <- table.symbolTableMap) {
      outStrings += (name + ':' + typeDescriptorToString(descriptor))
    }

    if(!table.childrenSymbolTables.isEmpty) {
      outStrings += "Table's sub-tables:"
      for(subtable <- table.childrenSymbolTables) {
        val subtableStrings = symbolTableToString(subtable)
        outStrings.appendAll(subtableStrings.map(x => '\t' + x))
        outStrings += "\n"
      }
    }

    return outStrings
  }

  def parameterTableToString(table : ParametersTable) : ListBuffer[String] = {
    val outStrings : ListBuffer[String] = ListBuffer.empty[String]
    outStrings += "Parameters:"
    for((name, descriptor) <- table.getParamMap) {
      outStrings += (name + ':' + typeDescriptorToString(descriptor))
    }

    outStrings += "\n"

    outStrings += "Field declarations:"
    for((name, descriptor) <- table.symbolTableMap) {
      outStrings += (name + ':' + typeDescriptorToString(descriptor))
    }
    outStrings += "\n"

    if(!table.childrenSymbolTables.isEmpty) {
      outStrings += "Table's sub-tables:"
      for(subtable <- table.childrenSymbolTables) {
        val subtableStrings = symbolTableToString(subtable)
        outStrings.appendAll(subtableStrings.map(x => '\t' + x))
        outStrings += "\n"
      }
    }

    return outStrings
  }

  def methodDescriptorToString(descriptor: MethodDescriptor): ListBuffer[String] = {
    val outStrings : ListBuffer[String] = ListBuffer.empty[String]

    outStrings += "Method Name: " + descriptor.name
    outStrings += "Method Return Type: " + descriptor.methodType
    outStrings.appendAll(parameterTableToString(descriptor.getParamTable).map(x => '\t' + x))

    return outStrings
  }

  def globalFieldTableToString(table : GlobalFieldTable) : ListBuffer[String] = {
    val outStrings: ListBuffer[String] = ListBuffer.empty[String]

    outStrings += "Global field declarations:"
    for ((name, descriptor) <- table.symbolTableMap) {
      outStrings += (name + ':' + typeDescriptorToString(descriptor))
    }

    return outStrings
  }

  def methodTableToString(table : MethodsTable) : ListBuffer[String] = {
    val outStrings : ListBuffer[String] = ListBuffer.empty[String]

    outStrings.appendAll(globalFieldTableToString(table.getGlobalFieldTable))
    outStrings += "\n"

    outStrings.append(table.getCallouts.toString)
    outStrings.append("\n")


    for((name, descriptor) <- table.methodTable) {
      outStrings.appendAll(methodDescriptorToString(descriptor))
    }

    return outStrings
  }

  def printSymbolTableStructure(table : MethodsTable): Unit = {
    println(methodTableToString(table).mkString("\n"))
  }
}
