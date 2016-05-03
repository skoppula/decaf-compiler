package compile.analysis

import compile.cfg.NormalBB
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode.{Tac, TacUnOp, TacBinOp}

class CSE {

  def available(map : Map[String, Expression], expr : Expression) : Map[String, Expression] = {
    // bvk is our symbolic expression
    // map is the expression map, assuming immutable for now

    for ((k,v) <- map) {

    }

    return null
  }


  def kill(map : Map[String, Expression], lhs : String, table : SymbolTable) : Map[String, Expression] = {
    // map is the expression map, assuming immutable for now
    // lhs is the symbolic variable
    // table is the symbol table of the bb

    // We need to get lhs' symbol table to perform the kill
    for ((k,bvk) <- map) {

    }

    return null
  }

  // TODO : It would be useful to have the temp -> symbol map here as a global
  def newComputeAvailableExpr(
                               bb : NormalBB
                             ) : Unit = {
    // 1. for tac in bb.instrs
    // 2. if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)

    val map : Map[String, Expression] = null

    for (tac <- bb.instrs) {
      if (tac.isAssign) {
        tac match {
          case t:TacBinOp => {
            var expr = convertTacToSymbolicExpr(t, map, bb.symbolTable)
          }
          case t:TacUnOp => {
            var expr = convertTacToSymbolicExpr(t, map, bb.symbolTable)
          }
          case _ => {

          }
        }

      }
    }

  }

  def convertTacToSymbolicExpr(
                               tac: Tac,
                               map: Map[String, Expression],
                               table: SymbolTable // This should be the symbol table of the block this Tac lives in
                             ) : Expression = {
    /*
    tac match {
      case b : TacBinOp => {
        var v1, t1 : (String, SymbolTable) = null
        var v2, t2 : (String, SymbolTable) = null

        if (CSEUtils.isTempVar(b.addr2)) {
          map.get(b.addr2) match {
              /*
            case Some(v,t) => {
              v1 = v
              t1 = t
            } */
            case None => {} // This shouldn't be a problem if the map passed in is actually correct
          }
        } else {
          // t1 = table.getContainingSymbolTable(v1)
        }
        if (CSEUtils.isTempVar(b.addr3)) {
          map.get(b.addr3) match {
              /*
            case Some(v,t) => {
              v2 = v
              t2 = t
            } */
            case None => {}
          }
        } else {
          // t2 = table.getContainingSymbolTable(v2)
        }

        val t : SymbolTable = null // t1.getMinSymbolTable(t2)

        // return Bvk(b.op, Set(v1, v2), ArrayBuffer[String](v1, v2), t)
      }
      case u : TacUnOp => {
        var v1 : String = u.addr2
        var t1 : SymbolTable = null

        if (CSEUtils.isTempVar(u.addr2)) {
          map.get(u.addr2) match {
            case Some(v,t) => {
              v1 = v
              t1 = t
            }
            case None => {}
          }
        } else {
          t1 = table.getContainingSymbolTable(v1)
        }

        return Bvk(u.op, Set(v1), ArrayBuffer[String](v1), t1)
      }
      case _ => {
        return EmptyBvk()
      }
    }
    */
    return null
  }

}
