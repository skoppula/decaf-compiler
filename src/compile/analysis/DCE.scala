package compile.analysis

import compile.cfg._
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}
import compile.util.Util.dprintln

object DCE {

  def runDCEFixedPointAlgorithm(
                                  methodStart : NormalBB,
                                  methodEnd : NormalBB
                               ) = {
    val methodIdBBMap = getMethodIdBBMap(methodStart)

    var changed = Set[String]()
    for ((id, bb) <- methodIdBBMap) {
      changed += id
    }

    // We do not generate dceIn on the root node
    computeDCEInPerBlock(methodEnd)
    changed -= methodEnd.id

    dprintln("\n Starting liveness fixed point algorithm...")
    while (!changed.isEmpty) {
      changed = iterationOfDCEAlg(changed, methodIdBBMap)
    }

    dprintln("Finished liveness fixed point algorithm...")
  }

  def getMethodIdBBMap(methodStart : NormalBB) : Map[String, NormalBB] = {
    var methodIdBBMap = Map.empty[String, NormalBB]
    val methodIds = CFGUtil.cfgBBs(methodStart, List())
    for(id <- methodIds) {
      methodIdBBMap = methodIdBBMap + {id -> BasicBlockGenie.idToBBReference(id)}
    }
    return methodIdBBMap
  }

  def iterationOfDCEAlg(changed : Set[String], methodIdBBMap : Map[String, NormalBB]): Set[String] = {

    var new_changed = Set[String]() ++ changed

    for ( id <- changed ) {
      new_changed -= id
      val bb = methodIdBBMap(id)
      val oldDCEIn = bb.dceIn
      computeDCEOutPerBlock(bb) //TODO: figure these out
      computeDCEInPerBlock(bb)
      val newDCEIn = bb.dceIn

      // Check if IN for parents of block need to be updated; if so change
      if(oldDCEIn != newDCEIn) {
        for (parent <- bb.getParents()) {
          new_changed += parent.id
        }
      }
    }
    return new_changed
  }

  // A use is pretty much where a variable is read
  def usePerTac(
                 set : Set[(String, SymbolTable)],
                 tac : Tac,
                 table : SymbolTable
               ) : Set[(String, SymbolTable)] = {
    return convertTacToSymbolicVarSet(tac, table).union(set) 
  }

  // TODO : Untested
  // Given the current set of live variables, S, and the symbolic variable from the LHS of a TAC, x, and the TAC's basic block's symbol table,
  // if x in S, we return S-x, else we return S 
  def defPerTac(set : Set[(String, SymbolTable)], slhs : String, table : SymbolTable) : Set[(String, SymbolTable)] = {
    var setOut : Set[(String, SymbolTable)] = set
    val slhsTable : SymbolTable = table.getContainingSymbolTable(slhs)
    return setOut -- Set[(String, SymbolTable)]((slhs, slhsTable))
  }

  def join(set1: Set[(String, SymbolTable)], set2: Set[(String, SymbolTable)]) : Set[(String, SymbolTable)] = {
    val set : Set[(String, SymbolTable)] = set1.union(set2)
    return set
  }

  def computeDCEAfterTac(set: Set[(String, SymbolTable)], tac: Tac, table: SymbolTable) : Set[(String, SymbolTable)] = {
  // if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
  // process RHS: add any variabes used 
  // process LHS: delete any matching variable from the set. 
    //TODO: LHS then RHS
    var live : Set[(String, SymbolTable)] = set

    //TODO: this needs to probably look at variables passed to method calls and stuff 
    tac match {
      case t:TacBinOp => {
        live = defPerTac(live, t.addr1, table)  
        live = usePerTac(live, t, table)
      }
      case t:TacUnOp => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacCopy => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacCopyInt => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacCopyBoolean => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacIf => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacIfFalse => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacReturnValue => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacArrayLeft => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacArrayRight => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t:TacMethodCallExpr => {
        live = defPerTac(live, t.addr1, table)
        live = usePerTac(live, t, table)
      }
      case t: TacMethodCallStmt => {
        live = usePerTac(live, t, table)
      }
      case _ => {
      }
    }
    return live
  }

  def computeDCEOutPerBlock(bb : NormalBB) : Unit = {
    // This requires that the block that is passed in actually has a child (i.e. don't pass the exit node)
    //
    // 0. join children to update liveOut

    // Step 0
    var liveOut : Set[(String, SymbolTable)] = Set()
    if (bb.isInstanceOf[BranchBB]) {
      // BranchBB should never be the root node so it should be guaranteed to have *a* parent
      val BBB : BranchBB = bb.asInstanceOf[BranchBB]
      var children = BBB.getChildren()
      if (children.size == 0) {
        throw new Exception("I am a BranchBB with no BBs")
      }
      liveOut = children(0).dceIn
      children = children.drop(1)
      for (child <- children) {
        liveOut = join(liveOut, child.dceIn)
      }
    } else {
      if (bb.getChildren().size > 1) {
        throw new Exception("I am subject to the One Child Policy- this is illegal")
      }
      var children = bb.getParents()
      liveOut = children(0).dceIn
    }
    bb.dceOut = liveOut
  }

  def computeDCEInPerBlock(bb : NormalBB) : Unit = {
    // 1. for tac in bb.instrs
    //     if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to livein : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)
    // 2. Update liveout

    // Step 1
    var live : Set[(String, SymbolTable)] = bb.dceIn

    for (tac <- bb.instrs.reverse) {
      live = computeDCEAfterTac(live, tac, bb.symbolTable)
    }

    // Step 2
    bb.dceOut = live
  }

  def convertTacToSymbolicVarSet( // returns a set of (String, SymbolTable) pairs corresponding to the symbolic variables used in the TAC
                                  tac: Tac,
                                  table: SymbolTable // This should be the symbol table of the block this Tac lives in
                               ) : Set[(String, SymbolTable)] = {
    //TODO: figure out what the other cases are and write them
    tac match {
      case b : TacBinOp => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(b.addr2, table)
        val (v2, t2) : (String, SymbolTable) = getSymbolAndTable(b.addr3, table)
        if (v1 == null || v2 == null) {
          return null // TODO: This null is not vetted
        }
        return Set((v1, t1), (v2, t2))
      }
      case u : TacUnOp => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(u.addr2, table)
        if (v1 == null) return null // TODO: This null is not vetted
        return Set((v1, t1))
      }
      case i : TacIf => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(i.addr1, table)
        if (v1 == null) return null // TODO: This null is not vetted
        return Set((v1, t1))
      }
      case iff : TacIfFalse => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(iff.addr1, table)
        if (v1 == null) return null // TODO: This null is not vetted
        return Set((v1, t1))
      }
      case mce : TacMethodCallExpr => {
        return null
        //TODO: figure out what to do with mce.args? 
      }
      case mcs : TacMethodCallStmt => {
        return null
        //TODO: figure out what to do with mcs.args?
      }
      case rv : TacReturnValue => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(rv.addr1, table)
        if (v1 == null) return null // TODO: This null is not vetted
        return Set((v1, t1))
      }
      case al : TacArrayLeft => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(al.addr1, table)
        if (v1 == null) return null // TODO: This null is not vetted
        return Set((v1, t1))
      }
      case ar : TacArrayRight => {
        val (v1, t1) : (String, SymbolTable) = getSymbolAndTable(ar.addr1, table)
        val (v2, t2) : (String, SymbolTable) = getSymbolAndTable(ar.addr2, table)
        if (v1 == null || v2 == null) {
          return null // TODO: This null is not vetted
        }
        return Set((v1, t1), (v2, t2))
      }
      case _ =>
        return null // TODO: This null is not vetted
    }
  }

  def getSymbolAndTable(variable : String, table : SymbolTable) : (String, SymbolTable) = {
    // table needs to be the symbol table of the corresponding bb block
    var v : String = variable
    var t : SymbolTable = null
    if (DCEUtil.isTempVar(variable)) {
      DCEUtil.tempSymbolMap.get(variable) match {
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

