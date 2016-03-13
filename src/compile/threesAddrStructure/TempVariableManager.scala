package compile.threesAddrStructure

import scala.collection.mutable

object TempVariableManager {

  val names = mutable.Set.empty[String]

  def generateName() = {

  }

  def isName(name : String) = {
    names contains name
  }

}
