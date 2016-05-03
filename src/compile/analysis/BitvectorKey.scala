package compile.analysis

import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}
import compile.symboltables.{SymbolTable}

// TODO we need to modify so it includes a struct with symbol var -> symbol table
object BitvectorKey {
  abstract class BitvectorKey
  case class Bvk (op: OpEnumVal, setVars: Set[(String, SymbolTable)], listVars: ArrayBuffer[(String,SymbolTable)], table: SymbolTable) extends BitvectorKey{  
    override def equals(that: Any) : Boolean =
    that match { 
      case that: Bvk => {
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
  case class EmptyBvk() extends BitvectorKey{} 
}
