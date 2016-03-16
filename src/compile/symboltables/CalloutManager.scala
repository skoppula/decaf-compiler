package compile.symboltables

import compile.exceptionhandling.{ExceptionGenie, InvalidCalloutException, CalloutAlreadyExistsException}

import scala.collection.mutable

class CalloutManager(exceptionGenie: ExceptionGenie) {
  var calloutSet : mutable.Set[String] = mutable.Set.empty[String]
  var closed = false;

  def addCallout(calloutName : String): Unit = {

    if(closed) {
      exceptionGenie.insert(new InvalidCalloutException("You cannot declare any callouts!"))
    }

    if(calloutSet contains calloutName) {
      exceptionGenie.insert(new CalloutAlreadyExistsException("Sorry, you have already defined that callout"))
    } else {
      calloutSet = calloutSet + calloutName
    }
  }

  def isCallout(s : String) : Boolean = {
    return calloutSet contains s
  }

  def closeCallouts {
    closed = true;
  }

  override def toString : String = {
    return "Callouts(" + calloutSet.mkString(",") + ")"
  }

}