package compile.symboltables

import compile.descriptors.MethodDescriptor
import scala.collection.mutable.HashMap
import compile.{CalloutAlreadyExistsException, CalloutManager, MethodAlreadyExistsException}

class MethodsTable(calloutManager : CalloutManager) {
  var methodTable: HashMap[String, MethodDescriptor] = HashMap.empty[String, MethodDescriptor];

  def insert(name : String, methodDescriptor: MethodDescriptor): Unit = {
    /**
      * Inserts the method name and identifier in this method table,
      * Throws exception if identifier already in table
      */
    if(methodTable.contains(name)) {
      throw new MethodAlreadyExistsException("Method " + name + " already exists")

    } else if(this.isCallout(name)) {
      throw new CalloutAlreadyExistsException("Callout with name " + name + "already exists")

    } else {
      methodTable(name) = methodDescriptor
    }
  }

  def lookupID(id : String) : MethodDescriptor = {
    /**
      * Finds the method identifier in method symbol table,
      * Returns descriptor or null if identifier not found.
      */
    if(methodTable.contains((id))) {
      methodTable(id)
    } else {
      null
    }
  }

  def validate() : Boolean = {
    /**
      *  - Checks for a main method
      *  - Recursively calls validate on each method entry/descriptor
      */
    if(!methodTable.contains("main")) return false

    var check : Boolean = true
    for((id, methodDescriptor) <- methodTable) {
      check &= methodDescriptor.validate()
    }
    return check
  }

  def isCallout(name: String): Boolean  = {
    calloutManager.isCallout(name)
  }


  override def toString : String = {
    return "MethodTable(" + methodTable.mkString(",") + ")"
  }
}
