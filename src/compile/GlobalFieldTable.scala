package compile

class GlobalFieldTable extends SymbolTable(parentSymbolTable = null) {
  var fieldTable: Map[String, Descriptor] = Map();

  def checkNoMethodDescriptors(): Boolean = {
    return true
  }
}
