package compile.descriptors

import compile.symboltables.SymbolTableEntry

abstract class Descriptor extends SymbolTableEntry {
  var byteSize : Int;
}
