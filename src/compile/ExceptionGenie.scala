package compile

import scala.collection.mutable.ArrayBuffer

class ExceptionGenie {
  var exceptions : ArrayBuffer[Exception] = ArrayBuffer.empty[Exception]
  def insert(e : Exception) {
    exceptions.insert(1, e)
    println(e.getMessage)
  }

  def mkString() : String = {
    exceptions.mkString
  }
}
