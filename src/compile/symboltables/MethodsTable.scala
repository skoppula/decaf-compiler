package compile.symboltables

import compile.descriptors.MethodDescriptor
import scala.collection.mutable.HashMap
import compile.{MethodAlreadyExistsException, MethodNotFoundException}

class MethodsTable {
  var methodTable: HashMap[String, MethodDescriptor] = HashMap.empty[String, MethodDescriptor];

  def insert(name : String, methodDescriptor: MethodDescriptor): Unit = {
    /**
      * Inserts the method name and identifier in this method table,
      * Throws exception if identifier already in table
      */
    if(methodTable.contains(name)) {
      throw new MethodAlreadyExistsException("Method " + name + " already exists")
    } else {
      methodTable(name) = methodDescriptor
    }
  }

  def lookupID(id : String) : MethodDescriptor = {
    /**
      * Finds the method identifier in method symbol table,
      * Returns descriptor or throws exception if identifier not found.
      */
    if(methodTable.contains((id))) {
      return methodTable(id)
    } else {
      throw new MethodNotFoundException("Method " + id + " not found in methods table")
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

  override def toString : String = {
    return "MethodTable(" + methodTable.mkString(",") + ")"
  }
}
