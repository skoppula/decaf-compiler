package compile.analysis

import compile.cfg._
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}
import compile.util.Util.dprintln

object CSE {

  def runCSEFixedPointAlgorithm(
                                  methodStart : NormalBB
                               ) {
    val methodIdBBMap = getMethodIdBBMap(methodStart)

    var changed = Set[String]()
    for ((id, bb) <- methodIdBBMap) {
      changed += id
    }

    // We do not generate cseIn on the root node
    computeCSEOutPerBlock(methodStart)
    changed -= methodStart.id

    dprintln("\n Starting availability fixed point algorithm...")
    while (!changed.isEmpty) {
      changed = iterationOfCSEAlg(changed, methodIdBBMap)
    }

    dprintln("Finished availability fixed point algorithm...")
  }

  def getMethodIdBBMap(methodStart : NormalBB) : Map[String, NormalBB] = {
    var methodIdBBMap = Map.empty[String, NormalBB]
    val methodIds = CFGUtil.cfgBBs(methodStart, List())
    for(id <- methodIds) {
      methodIdBBMap = methodIdBBMap + {id -> BasicBlockGenie.idToBBReference(id)}
    }
    return methodIdBBMap
  }

  def iterationOfCSEAlg(changed : Set[String], methodIdBBMap : Map[String, NormalBB]): Set[String] = {

    var new_changed = Set[String]() ++ changed

    for ( id <- changed ) {
      new_changed -= id
      val bb = methodIdBBMap(id)
      val oldCSEOut = bb.cseOut
      computeCSEInPerBlock(bb)
      computeCSEOutPerBlock(bb)
      val newCSEOut = bb.cseOut

      // Check if IN for children of block need to be updated; if so change
      if(oldCSEOut != newCSEOut) {
        for (child <- bb.getChildren()) {
          new_changed += child.id
        }
      }
    }

    return new_changed

  }

  // TODO : Untested
  def genPerTac(
                 map : Map[String, Expression],
                 tVar : String,
                 expr : Expression
               ) : Map[String, Expression] = {
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
  def killPerTac(map : Map[String, Expression], slhs : String, table : SymbolTable) : Map[String, Expression] = {
    // map is the in expression map, assuming immutable for now
    // slhs is the symbolic variable
    // table is the symbol table of the bb

    // We need to get lhs' symbol table to perform the kill
    // Look up the table.getContainingSymbolTable(slhs)
    var mapOut : Map[String, Expression] = map
    var slhsTable : SymbolTable = table.getContainingSymbolTable(slhs)
    // dprintln("the t->e map is currently " + mapOut.mkString(", "))
    for ((tlhs,e) <- map) {
      // dprintln("processing " + tlhs + " -> " + e.toString)
      if (e.setVars contains ((slhs, slhsTable))) {
        // dprintln("subtracting  "+ tlhs)
        mapOut -= tlhs
      }
    }

    return mapOut
  }

  def mapToSet(map : Map[String, Expression]) : Set[(String, Expression)] = {
    var set : Set[(String, Expression)] = Set()
    for ((k,v) <- map) {
      set += ((k,v))
    }
    return set
  }

  // TODO: Untested
  def join(map1: Map[String, Expression], map2: Map[String, Expression]) : Map[String, Expression] = {
    // Both map1 and map2 only have one temp var that maps to a particular expression
    // We should have a one-to-one mapping in both

    val set1 : Set[(String, Expression)] = mapToSet(map1)
    val set2 : Set[(String, Expression)] = mapToSet(map2)
    val set : Set[(String, Expression)] = set1.intersect(set2)
    var mapOut : Map[String, Expression] = Map()

    for ((k,v) <- set) {
      mapOut = mapOut + {k -> v}
    }

/*    val inverseMap1 : Map[Expression, String] = map1.map(_.swap) // This requires one-to-one mapping

    var mapOut : Map[String, Expression] = map1

    for ((tlhs,e) <- map2) {
      // We checked that contains does use the .equals method to check containment
      if (inverseMap1.contains(e)) {
        var t : String = inverseMap1(e)
        if (t != tlhs) {
          mapOut -= t
        } 
      } else {
        // mapOut += {tlhs -> e} // WRONG
      }
    }
 */
    return mapOut
  }

def computeCSEAfterTac(map:Map[String, Expression], tac:Tac, table:SymbolTable) : Map[String, Expression] = {
  // if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
  // process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
  // process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)
  // Austin: Now I don't think that symbol(LHS) is the right thing to use for killing. Changed for now
  // TODO: Figure out the correct KILL procedure

  var avail : Map[String, Expression] = map

  if (tac.isAssign) {
    val expr = convertTacToSymbolicExpr(tac, table)
    
    tac match {
      case t:TacBinOp => {
        if (expr != null) {
          // GEN step
          avail = genPerTac(avail, t.addr1, expr)
        }
        // KILL step
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        avail = killPerTac(avail, t.addr1, table)
      }
      case t:TacUnOp => {
        if (expr != null) {
          // GEN step
          avail = genPerTac(avail, t.addr1, expr)
        }
        // KILL step
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        avail = killPerTac(avail, t.addr1, table)
      }
      case t:TacCopy => {
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        // KILL step
        avail = killPerTac(avail, t.addr1, table)
      }
      case t:TacCopyInt => {
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        // KILL step
        avail = killPerTac(avail, t.addr1, table)
      }
      case t:TacCopyBoolean => {
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        // KILL step
        avail = killPerTac(avail, t.addr1, table)
      }
      case t:TacMethodCallExpr => {
        // val (symbol,_) = getSymbolAndTable(t.addr1, table)
        // KILL step
        avail = killPerTac(avail, t.addr1, table)
      }
      case _ => {
      }
    }
  }

  return avail
}

  def computeCSEInPerBlock(bb : NormalBB) : Unit = {
    // This requires that the block that is passed in actually has a parent (i.e. don't pass the root node)
    //
    // 0. join parents to update availin

    // Step 0
    var availin : Map[String, Expression] = Map()
    if (bb.isInstanceOf[MergeBB]) {
      // MergeBB should never be the root node so it should be guaranteed to have *a* parent
      val MBB : MergeBB = bb.asInstanceOf[MergeBB]
      var parents = MBB.getParents()
      if (parents.size == 0) {
        throw new Exception("I am an orphan")
      }
      dprintln("I am mergeBB " + MBB.id + " and my parents are " + MBB.getParents())
      availin = parents(0).cseOut
      dprintln("My initial availin is " + availin)
      parents = parents.drop(1)
      for (pbb <- parents) {
        dprintln("Processing parent  " + pbb.id)
        dprintln("its cseOut is " + pbb.cseOut.mkString)
        availin = join(availin, pbb.cseOut)
        dprintln("My next availin is " + availin)
      }
    } else if (bb.isInstanceOf[JumpDestBB]) {
      // JumpDestBB should never be the root node so it should be guaranteed to have *a* parent
      val JDBB : JumpDestBB = bb.asInstanceOf[JumpDestBB]
      var parents = JDBB.getParents()
      if (parents.size == 0) {
        throw new Exception("I am an orphan")
      }
      availin = parents(0).cseOut
      parents = parents.drop(1)
      for (pbb <- parents) {
        availin = join(availin, pbb.cseOut)
      }
    } else {
      if (bb.getParents().size > 1) {
        throw new Exception("I should not have both a mommy and a daddy, down with heteronormative something")
      }
      var parents = bb.getParents()
      availin = parents(0).cseOut
    }

    bb.cseIn = availin
  }

  def computeCSEOutPerBlock(bb : NormalBB) : Unit = {
    // 1. for tac in bb.instrs
    //     if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)
    // 2. Update availout

    // Step 1
    var avail : Map[String, Expression] = bb.cseIn

    for (tac <- bb.instrs) {
      avail = computeCSEAfterTac(avail, tac, bb.symbolTable)
    }

    // Step 2
    bb.cseOut = avail
  }



  def convertTacToSymbolicExpr(
                               tac: Tac,
                               table: SymbolTable // This should be the symbol table of the block this Tac lives in
                             ) : Expression = {
    // TODO: If the Tac is not convertible then null is returned so make sure you check for that
    tac match {
      case b : TacBinOp => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(b.addr2, table)
        val (v2, t2) : (String, SymbolTable) = getSymbolAndTable(b.addr3, table)
        if (v1 == null || v2 == null) {
          return null // TODO: This null is vetted and actually intended. Whether it's good design or not is ?able
        }
        return Expression(b.op, Set((v1,t1), (v2,t2)), ArrayBuffer[(String,SymbolTable)]((v1,t1), (v2,t2)))
      }
      case u : TacUnOp => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(u.addr2, table)
        if (v1 == null) return null // TODO: This null is vetted and actually intended. Whether it's good design or not is ?able
        return Expression(u.op, Set((v1,t1)), ArrayBuffer[(String,SymbolTable)]((v1,t1)))
      }
      case _ =>
        return null // TODO: This null is vetted and actually intended. Whether it's good design or not is ?able
    }
  }

    def getSymbolAndTable(variable : String, table : SymbolTable) : (String, SymbolTable) = {
      // table needs to be the symbol table of the corresponding bb block
      var v : String = variable
      var t : SymbolTable = null
      if (CSEUtils.isTempVar(variable)) {
        CSEUtils.tempSymbolMap.get(variable) match {
          case Some((sym,symTbl)) => {
            v = sym
            t = symTbl
          }
          case None => {
            return (null, null) // TODO: This null is vetted and actually intended. Whether it's good design or not is ?able
          } // This shouldn't be a problem if the map passed in is actually correct
        }
      } else {
        t = table.getContainingSymbolTable(v)
      }
      return (v,t)
    }

}
