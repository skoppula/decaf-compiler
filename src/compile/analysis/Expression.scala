package compile.analysis

import compile.symboltables.SymbolTable
import compile.tac.OpTypes._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by skoppula on 5/3/16.
  */
class Expression {
  abstract class Expression
  case class Expr (op: OpEnumVal, setVars: Set[(String, SymbolTable)], listVars: ArrayBuffer[(String,SymbolTable)], table: SymbolTable) extends Expression {
    override def equals(that: Any) : Boolean =
      that match {
        case that: Expr => {
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
      return "%s(%s)".format(op,listVars.mkString(", "))
    }
  }
  case class EmptyExpr() extends Expression{}
}
