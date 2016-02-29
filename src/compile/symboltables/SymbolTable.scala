package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import scala.collection.mutable
import compile.IdentifierAlreadyExistsException

class SymbolTable(parentSymbolTable : SymbolTable, tableInfo : String) {
  var symbolTableMap : mutable.HashMap[String, BaseDescriptor] = mutable.HashMap.empty[String, BaseDescriptor]
  var childrenSymbolTables : mutable.Set[SymbolTable] = mutable.Set.empty[SymbolTable]

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
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists in " + tableInfo)
    } else {
      symbolTableMap(id) = descriptor
    }
  }

  def addChild(symbolTable : SymbolTable): Unit = {
      childrenSymbolTables.add(symbolTable)
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

  def validate() : Boolean = {
    var check : Boolean = true
    for(table <- childrenSymbolTables.toIterator) {
      check &= table.validate()
    }
    check
  }

  override def toString : String = {
    tableInfo
  }

}