package compile.tac

import scala.collection.mutable

object TempVariableGenie {

  val varNames = mutable.Set.empty[String]
  val labelNames = mutable.Set.empty[String]
  var varCount = 0
  var labelCount = 0
  def generateName() : String  = {
    varCount = varCount + 1
    ("#t" + varCount)
  }
  def generateLabel() : String = {
    labelCount = labelCount + 1
    ("#l" + labelCount) // shouldn't actually need the #, but w/e 
  }
  def isVarName(name : String) : Boolean = {
    varNames contains name
  }
  def isLabelName(name : String) : Boolean = {
    labelNames contains name
  }

}
