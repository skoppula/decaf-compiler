package compile.cfg

import scala.collection.mutable

object BasicBlockGenie {

  val bbNames = mutable.Set.empty[String]
  var bbCount = 0

  def generateBBName() : String  = {
    bbCount = bbCount + 1
    (".BB" + bbCount)
  }

  def isBBName(name : String) : Boolean = {
    bbNames contains name
  }

}
