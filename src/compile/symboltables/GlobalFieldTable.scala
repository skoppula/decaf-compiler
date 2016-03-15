package compile.symboltables

import compile.descriptors.BaseDescriptor
import util.CLI
import compile.{InvalidGlobalFieldTableMethodException, IdentifierNotFoundException}

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

  // Not a valid function for GFT
  override def getTotalByteSize(): Int = {
    throw new InvalidGlobalFieldTableMethodException("You shouldn't be calculating total byte size for GFT!")
  }

  // Not a valid function for GFT
  override def computeOffsets(baseOffset : Int) : Int = {
    throw new InvalidGlobalFieldTableMethodException("You shouldn't be calculating offsets for GFT!")
  }

  override def isGlobal(id : String) : Boolean = {
    return symbolTableMap contains id
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
