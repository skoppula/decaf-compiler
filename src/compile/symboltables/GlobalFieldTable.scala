package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import compile.IdentifierNotFoundException

class GlobalFieldTable extends SymbolTable(null, null) {

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or throws
      * Exception if identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else {
      throw new IdentifierNotFoundException(id + " not found")
    }
  }

  override def validate() : Boolean = {
    true
  }

  override def toString : String = {
    "GlobalFieldTable(" + symbolTableMap.mkString(",") + ")"
  }
}
