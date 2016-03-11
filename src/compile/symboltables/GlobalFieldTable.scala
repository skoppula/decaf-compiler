package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import compile.IdentifierNotFoundException

class GlobalFieldTable extends SymbolTable(null, null) {

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null
      * if identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else {
      null
    }
  }

  override def validate() : Boolean = {
    true
  }

  override def toString : String = {
    "GlobalFieldTable(" + symbolTableMap.mkString(",") + ")"
  }
}
