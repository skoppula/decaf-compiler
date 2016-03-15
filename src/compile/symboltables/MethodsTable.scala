package compile.symboltables

import compile.descriptors.MethodDescriptor
import scala.collection.mutable.HashMap
import compile._

class MethodsTable(calloutManager : CalloutManager, globalFieldTable: GlobalFieldTable) {
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

    } else if(globalFieldTable.lookupID(name) != null) {
      throw new IdentifierAlreadyExistsException("Method name conflicts with global field identifier with same name" + name)

    } else {
      methodTable(name) = methodDescriptor
    }
  }

  def computeOffsets() {
    for((name, methodDescriptor) <- methodTable) {
      methodDescriptor.computeOffsets()
    }
  }

  def getTotalByteSize(methodName : String) : Int = {
    if(methodTable.contains(methodName)) {
      return methodTable(methodName).getTotalByteSize()
    } else {
      throw new MethodDoesNotExistException("Method you requested does not exist!")
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

  def isCallout(name: String): Boolean  = {
    calloutManager.isCallout(name)
  }

  override def toString : String = {
    return "MethodTable(" + methodTable.mkString(",") + ")"
  }
}
