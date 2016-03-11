package compile

import scala.collection.mutable.ArrayBuffer

class ExceptionGenie {
  var exceptions : ArrayBuffer[Exception] = ArrayBuffer.empty[Exception]
  def insert(e : Exception) {
    exceptions += e
    println(e)
    sys.exit(1)
  }

  def mkString() : String = {
    exceptions.mkString
  }
}
