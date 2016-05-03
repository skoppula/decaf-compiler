package compile.analysis

import compile.cfg.NormalBB
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode.{Tac, TacUnOp, TacBinOp}
import scala.collection.mutable.{ArrayBuffer}

class CSE {

  // TODO : Untested
  def available(map : Map[String, Expression], tVar : String, expr : Expression) : Map[String, Expression] = {
    // This should be called iteratively on each tac in a block
    // expr is our symbolic expression
    // map is the expression map, assuming immutable for now
    var mapOut : Map[String, Expression] = map

    for ((tlhs,e) <- map) {
      if (e.equals(expr)) {
        mapOut -= tlhs
      }
    }

    mapOut += {tVar -> expr}

    return mapOut
  }


  // TODO : Untested
  def kill(map : Map[String, Expression], slhs : String, table : SymbolTable) : Map[String, Expression] = {
    // map is the in expression map, assuming immutable for now
    // slhs is the symbolic variable
    // table is the symbol table of the bb

    // We need to get lhs' symbol table to perform the kill
    // Look up the table.getContainingSymbolTable(slhs)  
    var mapOut : Map[String, Expression] = map
    var slhsTable : SymbolTable = table.getContainingSymbolTable(slhs)
    for ((tlhs,e) <- map) {
      if (e.setVars contains (slhs, slhsTable)) {
        mapOut -= tlhs
      }
    }

    return mapOut
  }

  // TODO: Untested
  def join(map1: Map[String, Expression], map2: Map[String, Expression]) : Map[String, Expression] = {
    // Both map1 and map2 only have one temp var that maps to a particular expression
    // We should have a one-to-one mapping in both
    var inverseMap1 : Map[Expression, String] = map1.map(_.swap) // This requires one-to-one mapping

    var mapOut : Map[String, Expression] = map1

    for ((tlhs,e) <- map2) {
      // We checked that contains does use the .equals method to check containment
      if (inverseMap1.contains(e)) {
        var t : String = inverseMap1(e)
        if (t != tlhs) {
          mapOut -= t
        }
      } else {
        mapOut += {tlhs -> e}
      }
    }

    return mapOut
  }

  // TODO : It would be useful to have the temp -> symbol map here as a global
  def newComputeAvailableExpr(
                               bb : NormalBB
                             ) : Unit = {
    // 1. for tac in bb.instrs
    // 2. if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)

    val globalMap : Map[String, (String, SymbolTable)] = null

    for (tac <- bb.instrs) {
      if (tac.isAssign) {
        tac match {
          case t:TacBinOp => {
            var expr = convertTacToSymbolicExpr(t, globalMap, bb.symbolTable)
            // TODO
          }
          case t:TacUnOp => {
            var expr = convertTacToSymbolicExpr(t, globalMap, bb.symbolTable)
            // TODO
          }
          case _ => {

          }
        }

      }
    }

  }

  def convertTacToSymbolicExpr(
                               tac: Tac,
                               map: Map[String, (String, SymbolTable)],
                               table: SymbolTable // This should be the symbol table of the block this Tac lives in
                             ) : Expression = {
    // map is the global temp -> (symbol, symboltable)
    // TODO: If the Tac is not convertible then null is returned so make sure you check for that
    
    tac match {
      case b : TacBinOp => {
        var v1 : String = b.addr2 
        var t1 : SymbolTable = null
        var v2 : String = b.addr3
        var t2 : SymbolTable = null

        if (CSEUtils.isTempVar(b.addr2)) {
          map.get(b.addr2) match {
            
            case Some((v,t)) => {
              v1 = v
              t1 = t
            }
            case None => {
              return null
            } // This shouldn't be a problem if the map passed in is actually correct
          }
        } else {
          t1 = table.getContainingSymbolTable(v1)
        }
        if (CSEUtils.isTempVar(b.addr3)) {
          map.get(b.addr3) match {
            
            case Some((v,t)) => {
              v2 = v
              t2 = t
            }
            case None => {
              return null
            }
          }
        } else {
          t2 = table.getContainingSymbolTable(v2)
        }

        return Expression(b.op, Set((v1,t1), (v2,t2)), ArrayBuffer[(String,SymbolTable)]((v1,t1), (v2,t2)))
      }
      case u : TacUnOp => {
        var v1 : String = u.addr2
        var t1 : SymbolTable = null

        if (CSEUtils.isTempVar(u.addr2)) {
          map.get(u.addr2) match {
            case Some((v,t)) => {
              v1 = v
              t1 = t
            }
            case None => {
              return null
            }
          }
        } else {
          t1 = table.getContainingSymbolTable(v1)
        }

        return Expression(u.op, Set((v1,t1)), ArrayBuffer[(String,SymbolTable)]((v1,t1)))
      }
      case _ => {
        return null
      }
    }

  }

}
