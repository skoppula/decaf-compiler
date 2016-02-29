package compile.symboltables

import compile.descriptors.Descriptor

class SymbolTable(parentSymbolTable : SymbolTable) extends SymbolTableEntry {
  var symbolTableMaps: Map[String, SymbolTableEntry] = Map();

  // Returns null if id not found
  def lookupID(id : String) : Descriptor = {
    null
  }

}
