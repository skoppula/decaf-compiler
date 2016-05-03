package compile.analysis

import compile.cfg.{NormalBB, MergeBB, JumpDestBB}
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode._
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
  def computeAvailableExpr(bb : NormalBB) : Unit = {
    // 0. join parents to update availin
    // 1. for tac in bb.instrs
    //     if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)
    // 2. Update availout

    // Step 0
    var availin : Map[String, Expression] = Map()
    if (bb.isInstanceOf[MergeBB]) {
      val MBB : MergeBB = bb.asInstanceOf[MergeBB]
      for (pbb <- MBB.getParents()) {
        availin = join(availin, pbb.cse_hash_out)
      }
    } else if (bb.isInstanceOf[JumpDestBB]) {
      val JDBB : JumpDestBB = bb.asInstanceOf[JumpDestBB]
      for (pbb <- JDBB.getParents()) {
        availin = join(availin, pbb.cse_hash_out)
      }
    } else {
      for (pbb <- bb.getParents()) {
        availin = join(availin, pbb.cse_hash_out)
      }
    }

    bb.cse_hash_in = availin

    // Step 1
    var avail : Map[String, Expression] = bb.cse_hash_in
    val globalMap : Map[String, (String, SymbolTable)] = null // TODO : We need to fetch this global map from somewhere

    for (tac <- bb.instrs) {
      if (tac.isAssign) {
        tac match {
          case t:TacBinOp => {
            var expr = convertTacToSymbolicExpr(t, bb.symbolTable)
            // GEN step
            avail = available(avail, t.addr1, expr) 
            // KILL step
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case t:TacUnOp => {
            var expr = convertTacToSymbolicExpr(t, bb.symbolTable)
            // GEN step
            avail = available(avail, t.addr1, expr) 
            // KILL step
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case t:TacCopy => {
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            // KILL step
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case t:TacCopyInt => {
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            // KILL step
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case t:TacCopyBoolean => {
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            // KILL step
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case t:TacMethodCallExpr => {
            var (symbol,_) = getSymbolAndTable(t.addr1, bb.symbolTable)
            // KILL step
            avail = kill(avail, symbol, bb.symbolTable)
          }
          case _ => {
          }
        }
      }
    }

    // Step 2
    bb.cse_hash_out = avail
  }

  def convertTacToSymbolicExpr(
                               tac: Tac,
                               table: SymbolTable // This should be the symbol table of the block this Tac lives in
                             ) : Expression = {
    // map is the global temp -> (symbol, symboltable)
    // TODO: If the Tac is not convertible then null is returned so make sure you check for that
    
    val globalMap : Map[String, (String, SymbolTable)] = null // TODO : We need to fetch this global map from somewhere

    tac match {
      case b : TacBinOp => {
        var (v1, t1) : (String, SymbolTable) = getSymbolAndTable(b.addr2, table)
        var (v2, t2) : (String, SymbolTable) = getSymbolAndTable(b.addr3, table)
        return Expression(b.op, Set((v1,t1), (v2,t2)), ArrayBuffer[(String,SymbolTable)]((v1,t1), (v2,t2)))
      }
      case u : TacUnOp => {
        var (v1, t1) : (String, SymbolTable) = getSymbolAndTable(u.addr2, table)
        return Expression(u.op, Set((v1,t1)), ArrayBuffer[(String,SymbolTable)]((v1,t1)))
      }
      case _ => {
        return null
      }
    }

  }

    def getSymbolAndTable(variable : String, table : SymbolTable) : (String, SymbolTable) = {
      var map: Map[String, (String, SymbolTable)]= null // TODO : We need to fetch this global map from somewhere
      // table needs to be the symbol table of the corresponding bb block
      var v : String = variable
      var t : SymbolTable = null
      if (CSEUtils.isTempVar(variable)) {
        map.get(variable) match {          
          case Some((sym,symTbl)) => {
            v = sym
            t = symTbl
          }
          case None => {
            return null
          } // This shouldn't be a problem if the map passed in is actually correct
        }
      } else {
        t = table.getContainingSymbolTable(v)
      }
      return (v,t)
    }

}
