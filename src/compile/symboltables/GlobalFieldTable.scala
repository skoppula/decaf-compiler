package compile.symboltables

import compile.descriptors.BaseDescriptor
import compile.exceptionhandling.IdentifierAlreadyExistsException

class GlobalFieldTable extends SymbolTable(null, null) {

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null
      * if identifier not found.
      */
    symbolTableMap.getOrElse(id, null)
  }

  override def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id)) {
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists")
    } else {
      symbolTableMap(id) = descriptor
    }
  }

  override def isGlobal(id : String) : Boolean = {
    return symbolTableMap contains id
  }

  override def getContainingSymbolTable(id : String): SymbolTable = {
    if(symbolTableMap.contains(id)) {
      return this
    } else {
      return null
    }
  }

  override def getGlobalFieldTable : SymbolTable = {
    return this
  }

  override def lookupIDOnlyInLocalScope(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s), stopping before the global/parameter scope.
      * Since this is the global scope, return null (not found)
      */
    null
  }

  override def toString : String = {
    "GlobalFieldTable(" + symbolTableMap.mkString(",") + ")"
  }
}
