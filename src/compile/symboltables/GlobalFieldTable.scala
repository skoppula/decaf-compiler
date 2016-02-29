package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import compile.IdentifierNotFoundException

class GlobalFieldTable(tableInfo : String) extends SymbolTable(parentSymbolTable = null, tableInfo) {
  if(CLI.irdebug)
    println("Creating global field table")

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or throws
      * Exception if identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else {
      throw new IdentifierNotFoundException(id + " not found in " + tableInfo)
    }
  }

  override def validate() : Boolean = {
    true
  }
}