package compile.symboltables

import compile.descriptors._
import compile.exceptionhandling.IdentifierAlreadyExistsException
import scala.collection.mutable.{HashMap, ListBuffer, ArrayBuffer}

class SymbolTable(parentSymbolTable : SymbolTable, sType : ScopeTypes.EnumVal) {

  var symbolTableMap : HashMap[String, BaseDescriptor] = HashMap.empty[String, BaseDescriptor]
  var childrenSymbolTables : ListBuffer[SymbolTable] = ListBuffer.empty[SymbolTable]
  val scopeType = sType

  var methodParameterTable : ParametersTable = null
  if(parentSymbolTable != null) {
    methodParameterTable = parentSymbolTable.methodParameterTable
  }

  if (parentSymbolTable != null) {
    parentSymbolTable.addChild(this)
  }

  def getChildrenSymbolTables : ListBuffer[SymbolTable] = {
    return this.childrenSymbolTables
  }

  def getParentSymbolTable : SymbolTable = {
    return this.parentSymbolTable
  }

  def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id)) {
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists")
    } else {
      symbolTableMap(id) = descriptor
      methodParameterTable.currTotalByteSize = methodParameterTable.currTotalByteSize + descriptor.sizeBytes
      descriptor.offsetBytes = -methodParameterTable.currTotalByteSize
    }
  }

  def addChild(symbolTable : SymbolTable): Unit = {
      childrenSymbolTables += symbolTable
  }

  def isGlobal(id : String) : Boolean = {
    if(symbolTableMap contains id) {
      return false
    } else {
      return parentSymbolTable.isGlobal(id)
    }
  }

  def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null if
      * identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else {
      parentSymbolTable.lookupID(id)
    }
  }

  def getContainingSymbolTable(id : String): SymbolTable = {
    if(symbolTableMap.contains(id)) {
      return this
    } else {
      return parentSymbolTable.getContainingSymbolTable(id)
    }
  }

  def lookupIDOnlyInLocalScope(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s), stopping before the global/parameter scope.
      * Returns descriptor or null if identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else {
      parentSymbolTable.lookupID(id)
    }
  }

  // TODO: Untested
  def getMinSymbolTable(that : SymbolTable) : SymbolTable = {
    // Returns the innermost symbol table 

    // Throw an exception here for that == null
    if (that == null) {
      throw new Exception("that is null")
    }

    var table : SymbolTable = that
    while (table != null && table != this) {
      table = table.getParentSymbolTable
    }

    if (table == null) { // TODO : Be careful here because if "that" is null this screws up the return
      return this
    } else if (table == this) {
      return that
    } else {
      throw new Exception("symbol table doesn't have a parent child relationship")
    }
  }

  def getGlobalFieldTable : SymbolTable = {
    return this.getParentSymbolTable.getGlobalFieldTable
  }

  override def toString : String = {
    "SymbolTable" + scopeType.toString + "(" + symbolTableMap.mkString(",") + ")"
  }
}
