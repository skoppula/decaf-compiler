package compile.symboltables

import compile.descriptors._
import compile.exceptionhandling.IdentifierAlreadyExistsException
import scala.collection.mutable

class SymbolTable(parentSymbolTable : SymbolTable, sType : ScopeTypes.EnumVal) {
  var symbolTableMap : mutable.LinkedHashMap[String, BaseDescriptor] = mutable.LinkedHashMap.empty[String, BaseDescriptor]
  var childrenSymbolTables : mutable.ListBuffer[SymbolTable] = mutable.ListBuffer.empty[SymbolTable]
  val scopeType = sType

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
      childrenSymbolTables += symbolTable
  }

  def isGlobal(id : String) : Boolean = {
    if(symbolTableMap contains id) {
      return false
    } else {
      return parentSymbolTable.isGlobal(id)
    }
  }

  def computeOffsetOfDescriptor(descriptor: BaseDescriptor, offset : Int): Int = {
    var currOffset = offset
    descriptor match {
      case int : IntTypeDescriptor => {
        int.offsetBytes =  currOffset - int.sizeBytes
        currOffset -= int.sizeBytes
      }
      case bool : BoolTypeDescriptor => {
        bool.offsetBytes =  currOffset - bool.sizeBytes
        currOffset -= bool.sizeBytes
      }
      case boolArr : BoolArrayTypeDescriptor => {
        boolArr.offsetBytes =  currOffset - boolArr.sizeBytes
        currOffset -= boolArr.sizeBytes
      }
      case intArr : IntArrayTypeDescriptor => {
        intArr.offsetBytes =  currOffset - intArr.sizeBytes
        currOffset -= intArr.sizeBytes
      }
    }
    return currOffset
  }

  def computeSizeOfDescriptor(descriptor: BaseDescriptor) : Int = {
    descriptor match {
      case int : IntTypeDescriptor => return int.sizeBytes
      case bool : BoolTypeDescriptor => return bool.sizeBytes
      case boolArr : BoolArrayTypeDescriptor => return boolArr.sizeBytes
      case intArr : IntArrayTypeDescriptor => return intArr.sizeBytes
    }
  }

  def computeOffsets(baseOffset: Int): Int = {
    var currOffset = baseOffset
    for((name, descriptor) <- symbolTableMap) {
      currOffset = computeOffsetOfDescriptor(descriptor, currOffset)
    }

    for(table <- childrenSymbolTables) {
      currOffset = table.computeOffsets(currOffset)
    }

    return currOffset
  }

  def getTotalByteSize(): Int = {
    var currByteSize = 0
    for((name, descriptor) <- symbolTableMap) {
      currByteSize += computeSizeOfDescriptor(descriptor)
    }

    for(table <- childrenSymbolTables) {
      currByteSize += table.getTotalByteSize()
    }

    return currByteSize
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
