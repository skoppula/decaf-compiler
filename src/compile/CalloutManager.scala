package compile

import scala.collection.mutable

class CalloutManager {
  var calloutSet : mutable.Set[String] = mutable.Set.empty[String]
  var closed = false;

  def addCallout(calloutName : String): Unit = {
    if(closed) {
      throw new InvalidCalloutException("")
    }
    if(calloutSet contains calloutName) {
      throw new CalloutAlreadyExistsException("")
    } else {
      calloutSet = calloutSet + calloutName
    }
  }

  def closeCallouts {
    closed = true;
  }

}
