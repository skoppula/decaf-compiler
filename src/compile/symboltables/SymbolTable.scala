package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import scala.collection.mutable
import compile.{ScopeTypes, IdentifierAlreadyExistsException}

class SymbolTable(parentSymbolTable : SymbolTable, sType : ScopeTypes.EnumVal) {
  var symbolTableMap : mutable.HashMap[String, BaseDescriptor] = mutable.HashMap.empty[String, BaseDescriptor]
  var childrenSymbolTables : mutable.Set[SymbolTable] = mutable.Set.empty[SymbolTable]
  val scopeType = sType

  if(CLI.irdebug)
    println("Creating symbol field table")

  if (parentSymbolTable != null) {
    parentSymbolTable.addChild(this)
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
    }
  }

  def addChild(symbolTable : SymbolTable): Unit = {
      childrenSymbolTables.add(symbolTable)
  }

  def isGlobal(id : String) : Boolean = {
    if(symbolTableMap contains id) {
      return false
    } else {
      return parentSymbolTable.isGlobal(id)
    }
  }

  def computeOffsets() {
    // TODO
  }

  def getTotalByteSize(): Int = {
    // TODO
    return 0
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

  override def toString : String = {
    "SymbolTable" + scopeType.toString + "(" + symbolTableMap.mkString(",") + ")"
  }
}
