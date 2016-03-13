package compile.threeAddrStructure

import scala.collection.mutable

object TempVariableGenie {

  val names = mutable.Set.empty[String]

  def generateName() = {

  }

  def isName(name : String) = {
    names contains name
  }

}
