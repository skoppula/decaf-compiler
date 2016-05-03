package compile.analysis

import compile.cfg.{CFGUtil, NormalBB, BasicBlockGenie}
import compile.tac.ThreeAddressCode._
import compile.analysis.BitvectorKey._
import compile.util.Util.dprintln
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashMap, Map}
import compile.analysis.CSEUtils

// Map[String, "Expression"] // temp var name to expression data type
// Use Bitvectorkey for Expression type except with symbolic variables now
// =>  Map[String, BitvectorKey]

object AvailableExpr {

  def replaceAvailableExpr(vecMap : HashMap[BitvectorKey, Int]) : Unit = {
    var latestExprValue : HashMap[String, Any] = new HashMap[String, Any] 
    //TODO: this doesn't actually do anything yet
  }

/*
  def convertTacToSymbolicBvk(
    tac: Tac,
    map: Map[String, (String, SymbolTable)],
    table: SymbolTable // This should be the symbol table of the block this Tac lives in

 */

  def available(map : Map[String, BitvectorKey], bvk : BitvectorKey) : Map[String, BitvectorKey] = {
    // bvk is our symbolic expression
    // map is the expression map, assuming immutable for now
    
    for ((k,v) <- map) {

    }

  }

  
  def kill(map : Map[String, BitvectorKey], lhs : String, table : SymbolTable) : Map[String, BitvectorKey] = {
    // map is the expression map, assuming immutable for now
    // lhs is the symbolic variable
    // table is the symbol table of the bb

    // We need to get lhs' symbol table to perform the kill
    for ((k,bvk) <- map) {

    }

  }

  // TODO : It would be useful to have the temp -> symbol map here as a global
  def newComputeAvailableExpr(
    bb : NormalBB
  ) : Unit = {
    // 1. for tac in bb.instrs 
    // 2. if it's an assign statement (TacCopy, TacCopyInt, TacCopyBoolean, TacMethodCallExpr, TacBinOp, TacUnOp)
    //     process RHS first (if binop/unop): just add the symbolic expression to availin : temp(LHS) -> symbol(RHS)
    //     process LHS next: delete the symbolic expressions whose RHS contains symbol(LHS)

    for (tac <- bb.instrs) {
      if (tac.isAssign) {
        tac match {
          case t:TacBinOp => {
            var bvk = convertTacToSymbolicBvk(t, map, bb.symboltable)
          }
          case t:TacUnOp => {
            var bvk = convertTacToSymbolicBvk(t, map, bb.symboltable)
          }
          case _ => {

          }
        }

      }
    }

  }

  def computeAvailableExpr(
                            programStartBB : NormalBB,
                            bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)]
                          ) : HashMap[String, HashMap[BitvectorKey, Int]] = {

    // Computer availability vectors for all basic blocks in a methods CFG tree
    //    iterates over all methods
    //    ignores programStartBB because no optimizations can be done on that

    val methodToBvkPositionMap = HashMap.empty[String, HashMap[BitvectorKey, Int]]

    for((methodName, (methodStart, methodFinish)) <- bbMethodMap) {

      val methodIdBBMap = getMethodIdBBMap(methodStart)
      val bvkPositionMap = initBitVectorMap(methodIdBBMap)
      val length = bvkPositionMap.size

      var changed = Set[String]()
      for ((id, bb) <- methodIdBBMap) {
        changed += id
        bb.avail_out = ArrayBuffer.fill(length)(0)
      }

      methodStart.avail_in = ArrayBuffer.fill(length)(0)
      methodStart.avail_out = availableGen(methodStart, bvkPositionMap)

      changed -= methodStart.id

      dprintln("\n Starting availability fixed point algorithm for method %s...".format(methodName))
      while (!changed.isEmpty) {
        changed = iterationOfAvailabilityChangeAlg(changed, bvkPositionMap, methodIdBBMap)
      }

      methodToBvkPositionMap(methodName) = bvkPositionMap

      dprintln("Finished availability fixed point algorithm for method %s...".format(methodName))
    }

    return methodToBvkPositionMap
  }

  def getMethodIdBBMap(methodStart : NormalBB) : Map[String, NormalBB] = {
    val methodIdBBMap = Map.empty[String, NormalBB]
    val methodIds = CFGUtil.cfgBBs(methodStart, List())
    for(id <- methodIds) {
      methodIdBBMap(id) = BasicBlockGenie.idToBBReference(id)
    }
    return methodIdBBMap
  }

  def iterationOfAvailabilityChangeAlg(changed : Set[String], bvkPositionMap : HashMap[BitvectorKey, Int], methodIdBBMap : Map[String, NormalBB]): Set[String] = {

    var new_changed = Set[String]() ++ changed
    val length = bvkPositionMap.size
    // dprintln("Changed set: " + changed.mkString(","))

    for ( id <- changed ) {
      // dprintln("\tHandling BB in changeset:" + id)
      val bb = methodIdBBMap(id)
      new_changed -= id

      bb.avail_in = ArrayBuffer.fill(length)(0)

      var parents_in = ""
      for(parent <- bb.getParents()) {
        parents_in += " " + parent.id + "_availin:" + parent.avail_out.mkString("")
      }
      // dprintln("\t\tcurrent bb:" + bb + "\t parent:" + parents_in)

      for ( parent <- bb.getParents() ) {
        bb.avail_in = intersect(ArrayBuffer.fill(length)(1), parent.avail_out)
      }
      val oldOut = bb.avail_out
      val bbKill = availableKill(bb, bvkPositionMap)
      val bbGen = availableGen(bb, bvkPositionMap)
      // dprintln("\t\tbb in:" + bb.avail_in.mkString("") + " bb out:" + bb.avail_out.mkString("") + " bbKill: " + bbKill.mkString("") + " bbGen: " + bbGen.mkString(""))
      bb.avail_out = union(bbGen, minus(bb.avail_in, bbKill))
      // dprintln("\t\tnew bb out:" + bb.avail_out)
      if (oldOut != bb.avail_out) {
        // dprintln("\t\t" + bb + "'s avail_out changed to " + bb.avail_out)
        // dprintln("\t\t Adding children to changeset:" + bb.getChildren().mkString(","))
        for ( child <- bb.getChildren()) {
          new_changed += child.id
        }
      }
    }

    return new_changed

  }

  // walks through the basic blocks and *somehow* figures out the different types of expressions.
  // Then, these are stored and mapped to their position in the bitvector
  def initBitVectorMap(methodIdBBMap : Map[String, NormalBB]) : HashMap[BitvectorKey, Int] = {
    var vecMap = new HashMap[BitvectorKey, Int]
    for ( (id, bb) <- methodIdBBMap) {
      for (instr <- bb.instrs) {
        val code = convertTacToBvk(instr)
        // used in the legend to map bitvectors back to BB for easy graph reading
        BasicBlockGenie.bvkToBB.put(code, bb)
        if (!code.isInstanceOf[EmptyBvk] && !vecMap.contains(code)) {
          vecMap += (code -> vecMap.size)
        }
      }
    }
    return vecMap
  }

  def newInitBitVectorMap(methodIdBBMap : Map[String, NormalBB]) : HashMap[BitvectorKey, Int] = {
    var vecMap = new HashMap[BitvectorKey, Int]
    for ( (id, bb) <- methodIdBBMap) {
      for (instr <- bb.instrs) {
        val code = convertTacToBvk(instr)
        // used in the legend to map bitvectors back to BB for easy graph reading
        BasicBlockGenie.bvkToBB.put(code, bb)
        if (!code.isInstanceOf[EmptyBvk] && !vecMap.contains(code)) {
          vecMap += (code -> vecMap.size)
        }
      }
    }
    return vecMap
  }
 
/*
  def convertTacToBvk(
    tac: Tac
  ) : BitvectorKey = {
    tac match {
      case b : TacBinOp => {
        return Bvk(b.op, Set(b.addr2, b.addr3), ArrayBuffer[String](b.addr2, b.addr3))
      }
      case u : TacUnOp => {
        return Bvk(u.op, Set(u.addr2), ArrayBuffer[String](u.addr2))
      }
      case _ => {
        return EmptyBvk()
      }
    }
  }
 */

  def convertTacToSymbolicBvk(
    tac: Tac,
    map: Map[String, (String, SymbolTable)],
    table: SymbolTable // This should be the symbol table of the block this Tac lives in
  ) : BitvectorKey = {
    tac match {
      case b : TacBinOp => {
        var v1 : String = b.addr2
        var t1 : SymbolTable = null
        var v2 : String = b.addr3
        var t2 : SymbolTable = null

        if (CSEUtils.isTempVar(b.addr2)) {
          map.get(b.addr2) match {
            case Some(v,t) => {
              v1 = v
              t1 = t
            }
            case None => {} // This shouldn't be a problem if the map passed in is actually correct
          }
        } else {
          t1 = table.getContainingSymbolTable(v1)
        }
        if (CSEUtils.isTempVar(b.addr3)) {
          map.get(b.addr3) match {
            case Some(v,t) => {
              v2 = v
              t2 = t
            }
            case None => {}
          }
        } else {
          t2 = table.getContainingSymbolTable(v2)
        }

        val t : SymbolTable = t1.getMinSymbolTable(t2)

        return Bvk(b.op, Set(v1, v2), ArrayBuffer[String](v1, v2), t)
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

  }
  
  // takes in a block and the expression -> bitvector index map.
  // Returns a bitvector with GEN[i] = 1 if expression i is used.
  def availableGen(
                   bb: NormalBB, 
                   vecMap: HashMap[BitvectorKey, Int]
                  ) : ArrayBuffer[Int] = {
      var bitvector = ArrayBuffer.fill(vecMap.size)(0)
      for (tac <- bb.instrs) {
        val code = convertTacToBvk(tac)
        for ( (bvk, int) <- vecMap ) {
          if ( bvk.equals(code) ) {
            bitvector(int) = 1 // TODO: remove this comment. scala y u no have break/continue
          }
        } 
      }
      return bitvector
  }

  // takes in a block and the expression -> bitvector index map. 
  // Returns a bitvector with KILL[i] = 1 if a variable in expression i is redefined.
  def availableKill(
                    bb: NormalBB, 
                    vecMap: HashMap[BitvectorKey, Int]
                   ) : ArrayBuffer[Int] = {
    val bitvector = ArrayBuffer.fill(vecMap.size)(0)
    for (tac <- bb.instrs) { 
      for ( (key, index) <- vecMap) {
        if (tac.isAssign && (key.asInstanceOf[Bvk].setVars contains tac.lhs)) {
          bitvector(index) = 1
        }
      }
    }
    return bitvector
  }

  // TODO : New intersect method?
  def intersect(
                vec1 : ArrayBuffer[Int],
                vec2 : ArrayBuffer[Int]
               ) : ArrayBuffer[Int] =
  {
    if (vec1.size != vec2.size) {
      dprintln("Bad intersect shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
    }  
    var newVec = new ArrayBuffer[Int]()

    for ( i <- 0 until vec1.size ) {
      newVec += (vec1(i) * vec2(i))
    }
    return newVec
  } 

  // TODO : New union method?
  def union(
            vec1 : ArrayBuffer[Int],
            vec2 : ArrayBuffer[Int]
           ) : ArrayBuffer[Int] =
  {
    if (vec1.size != vec2.size) {
      dprintln("Bad union shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
    }
    var newVec = new ArrayBuffer[Int]()

    for ( i <- 0 until vec1.size ) {
      newVec += (math.max(vec1(i),vec2(i)))
    }
    return newVec
  }

  def minus(
            vec1 : ArrayBuffer[Int],
            vec2 : ArrayBuffer[Int]
           ) : ArrayBuffer[Int] =
  {
    if (vec1.size != vec2.size) {
      dprintln("Bad minus shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
    }
    var newVec = new ArrayBuffer[Int]()

    for ( i <- 0 until vec1.size ) {
      newVec += (math.max(vec1(i)-vec2(i), 0))
    }  
    return newVec
  }

}
