package compile.symboltables

import compile.descriptors._
import compile.exceptionhandling.IdentifierAlreadyExistsException
import util.CLI

import scala.collection.mutable

/**
  * Class inherits from SymbolTable and captures local method variables directly inside the method
  *   and the parameters of the corresponding method
  */
class ParametersTable(
                       parentSymbolTable : GlobalFieldTable,
                       parametersMap : mutable.LinkedHashMap[String, PrimitiveBaseDescriptor])
  extends SymbolTable(parentSymbolTable, null) {

  override def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id) || parametersMap.contains(id)) {
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists")
    } else {
      symbolTableMap(id) = descriptor
    }
  }

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null if
      * identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else if (parametersMap.contains(id)) {
      parametersMap(id)
    } else {
      parentSymbolTable.lookupID(id)
    }
  }

  def getParametersTable : mutable.LinkedHashMap[String, PrimitiveBaseDescriptor] = {
    parametersMap
  }

  override def isGlobal(id : String) : Boolean = {
    if(symbolTableMap contains id) {
      return false
    } else if (parametersMap contains id) {
      return false
    } else {
      return parentSymbolTable.isGlobal(id)
    }
  }

  override def getTotalByteSize(): Int = {
    var currByteSize = 0
    for((name, descriptor) <- symbolTableMap) {
      currByteSize += computeSizeOfDescriptor(descriptor)
    }

    for((name, descriptor) <- parametersMap) {
      currByteSize += computeSizeOfDescriptor(descriptor)
    }

    for(table <- childrenSymbolTables) {
      currByteSize += table.getTotalByteSize()
    }

    return currByteSize
  }

  override def computeOffsets(baseOffset : Int): Int = {
    var currOffset = baseOffset
    for((name, descriptor) <- symbolTableMap) {
      currOffset = computeOffsetOfDescriptor(descriptor, currOffset)
    }

    for((name, descriptor) <- parametersMap) {
      currOffset = computeOffsetOfDescriptor(descriptor, currOffset)
    }

    for(table <- childrenSymbolTables) {
      currOffset = table.computeOffsets(currOffset)
    }

    return currOffset
  }


  override def toString : String = {
    "ParameterTable(ParamMap:" + parametersMap.mkString(",") + ", FieldTable:" + symbolTableMap.mkString(",") + ")"
  }
}
