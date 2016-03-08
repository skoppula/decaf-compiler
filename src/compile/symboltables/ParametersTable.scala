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
                       parametersMap : mutable.LinkedHashMap[String, BaseDescriptor])
  extends SymbolTable(parentsSymbolTable, tableInfo, scopeType = null) {

  if(CLI.irdebug)
    println("Creating parameters field table")

  override def insert(id : String, descriptor : BaseDescriptor): Unit = {
    /**
      * Attempts to insert identifier, descriptor pair into symbol table
      * Throws Exception if already identifier already exists
      */
    if(symbolTableMap.contains(id) || parametersMap.contains(id)) {
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
    } else if (parametersMap.contains(id)) {
      parametersMap(id)
    } else {
      parentsSymbolTable.lookupID(id)
    }
  }

  def getParametersTable : mutable.LinkedHashMap[String, BaseDescriptor] = {
    parametersMap
  }
}
