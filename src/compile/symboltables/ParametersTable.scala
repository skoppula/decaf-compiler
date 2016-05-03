package compile.symboltables

import compile.descriptors._
import compile.exceptionhandling.IdentifierAlreadyExistsException

import scala.collection.mutable

/**
  * Class inherits from SymbolTable and captures local method variables directly inside the method
  *   and the parameters of the corresponding method
  */
class ParametersTable(
                       parentSymbolTable : GlobalFieldTable,
                       parametersMap : mutable.LinkedHashMap[String, PrimitiveBaseDescriptor])
  extends SymbolTable(parentSymbolTable, null) {

  var currTotalByteSize = 0;
  this.methodParameterTable = this

  for((name, descriptor) <- parametersMap) {
    methodParameterTable.currTotalByteSize = methodParameterTable.currTotalByteSize + descriptor.sizeBytes
    descriptor.offsetBytes = -methodParameterTable.currTotalByteSize
  }

  override def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id) || parametersMap.contains(id)) {
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists")
    } else {
      symbolTableMap(id) = descriptor
      methodParameterTable.currTotalByteSize = methodParameterTable.currTotalByteSize + descriptor.sizeBytes
      descriptor.offsetBytes = -methodParameterTable.currTotalByteSize
    }
  }

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null if
      * identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap.get(id) match {
        case Some(b) => {return b}
        case None => return null //SHOULD NEVER GET HERE DOE
      }
    } else if (parametersMap.contains(id)) {
      parametersMap(id)
    } else {
      parentSymbolTable.lookupID(id)
    }
  }

  override def getContainingSymbolTable(id : String): SymbolTable = {
    if(symbolTableMap.contains(id)) {
      return this
    } else if (parametersMap.contains(id)) {
      return this
    } else {
      return parentSymbolTable.getContainingSymbolTable(id)
    }
  }

  def getParamMap : mutable.LinkedHashMap[String, PrimitiveBaseDescriptor] = {
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

  override def toString : String = {
    "ParameterTable(ParamMap:" + parametersMap.mkString(",") + ", FieldTable:" + symbolTableMap.mkString(",") + ")"
  }
}
