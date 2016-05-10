package compile.analysis

import scala.collection.mutable

class WebGenie {
  val webToReg = mutable.Map.empty[Int, String]
  var webCount = 0

  def generateWebNumber() : Int  = {
    webCount = webCount + 1
    webToReg(webCount) = "None"
    webCount
  }

}
