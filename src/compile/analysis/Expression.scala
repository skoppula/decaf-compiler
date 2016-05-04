package compile.analysis

import compile.symboltables.SymbolTable
import compile.tac.OpTypes._

import scala.collection.mutable.ArrayBuffer

case class Expression (op: OpEnumVal, setVars: Set[(String, SymbolTable)], listVars: ArrayBuffer[(String,SymbolTable)]){
  override def equals(that: Any) : Boolean =
    that match {
      case that: Expression => {
        if (op != that.op || (setVars != that.setVars) ) {
          return false
        }
        that.op match {
          case ADD | MULT | OR | AND | EQ | NEQ => {
            return true
          }
          case SUB | DIV | MOD | LT | LTE | GT | GTE | SIZE | MINUS | NOT => {
            return (listVars.equals(that.listVars))
          }
          case _ => return false
        }
      }
      case _ => return false
    }

  override def toString : String = {
    var listStr = ""
    for(lvar <- listVars) {
      listStr += lvar._1 + " "
    }
    return "%s(%s)".format(op, listStr)
  }
}
