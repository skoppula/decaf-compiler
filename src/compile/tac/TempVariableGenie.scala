package compile.tac

import scala.collection.mutable

class TempVariableGenie {

  val varNames = mutable.Set.empty[String]
  val labelNames = mutable.Set.empty[String]
  var varCount = 0
  var labelCount = 0
  var tacCount = 0 

  def generateName() : String  = {
    varCount = varCount + 1
    (".T" + varCount)
  }
  def generateLabel() : String = {
    labelCount = labelCount + 1
    (".L" + labelCount) // shouldn't actually need the #, but w/e 
  }
 
  def generateTacNumber() : Int = {
    tacCount = tacCount + 1
    return tacCount
  }

  def isVarName(name : String) : Boolean = {
    varNames contains name
  }
  def isLabelName(name : String) : Boolean = {
    labelNames contains name
  }

}
