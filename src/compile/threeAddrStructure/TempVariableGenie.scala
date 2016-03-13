package compile.threeAddrStructure

import scala.collection.mutable

object TempVariableGenie {

  val names = mutable.Set.empty[String]
  val count = 0
  def generateName() : String  = {
    count++
    return "#t" + count
  }

  def isName(name : String) : Boolean = {
    names contains name
  }

}
