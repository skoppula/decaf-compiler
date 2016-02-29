package compile.symboltables

import compile.IdentifierAlreadyExistsException
import compile.descriptors.{IntTypeDescriptor, BoolArrayTypeDescriptor, BaseDescriptor}
import util.CLI

import scala.collection.mutable

/**
  * Class inherits from SymbolTable and captures local method variables directly inside the method
  *   and the parameters of the corresponding method
  */
class ParametersTable(
     parentsSymbolTable : GlobalFieldTable,
     tableInfo : String,
     parametersTable : mutable.LinkedHashMap[String, BaseDescriptor])
  extends SymbolTable(parentsSymbolTable, tableInfo) {

  if(CLI.irdebug)
    println("Creating parameters field table")

  override def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id) || parametersTable.contains(id)) {
      throw new IdentifierAlreadyExistsException("Identifier " + id + " already exists in " + tableInfo)
    } else {
      symbolTableMap(id) = descriptor
    }
  }

  override def lookupID(id : String) : BaseDescriptor = {
    /**
      * Recursively finds the identifier in this symbol table,
      * or parent symbol table(s). Returns descriptor or null if
      * identifier not found.
      */
    if(symbolTableMap.contains(id)) {
      symbolTableMap(id)
    } else if (parametersTable.contains(id)) {
      parametersTable(id)
    } else {
      parentsSymbolTable.lookupID(id)
    }
  }

  def getParametersTable : mutable.LinkedHashMap[String, BaseDescriptor] = {
    parametersTable
  }
}
