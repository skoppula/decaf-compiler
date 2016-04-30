package compile.analysis

import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}

object BitvectorKey {
  abstract class BitvectorKey
  case class Bvk (op: OpEnumVal, setVars: Set[String], listVars: ArrayBuffer[String]) extends BitvectorKey{  
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
    }
  }
  case class EmptyBvk() extends BitvectorKey{} 
}
