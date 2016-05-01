package compile.analysis

import compile.cfg.NormalBB
import compile.cfg.BasicBlockGenie
import compile.tac.ThreeAddressCode._
import compile.analysis.BitvectorKey._
import compile.util.Util.dprintln
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashMap}

object AvailableExpr {

  def replaceAvailableExpr(vecMap : HashMap[BitvectorKey, Int]) : Unit = {
    var latestExprValue : HashMap[String, Any] = new HashMap[String, Any] 
    //TODO: this doesn't actually do anything yet
  }

  def computeAvailableExpr(
                            bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)]
                          ) : Unit = {
    val vecMap = initBitVectorMap()
    val length = vecMap.size
    val bbIdMap = BasicBlockGenie.idToBBReference

    var changed = Set[String]()
    for ((id, bb) <- bbIdMap) {
      changed += id
      bb.avail_out = ArrayBuffer.fill(length)(0)
    }

    val (start, _) = bbMethodMap("main")
    start.avail_in = ArrayBuffer.fill(length)(0)
    start.avail_out = availableGen(start, vecMap)

    changed -= start.id 

    while (!changed.isEmpty) {
      for ( id <- changed ) {
        val bb = BasicBlockGenie.idToBBReference(id)
        changed -= id

        bb.avail_in = ArrayBuffer.fill(length)(0)

        for ( parent <- bb.getParents() ) {
          println("Current bb:" + bb + "its parent(s):" + parent)
          bb.avail_in = intersect(bb.avail_in, parent.avail_out)
        }
        val oldOut = bb.avail_out
        val bbKill = availableKill(bb, vecMap)
        val bbGen = availableGen(bb, vecMap)
        println("bb in:" + bb.avail_in + " bb out:" + bb.avail_out + " bbKill: " + bbKill + " bbGen: " + bbGen)
        bb.avail_out = union(bbGen, minus(bb.avail_in, bbKill))
        println("new bb out:" + bb.avail_out)
        if (oldOut != bb.avail_out) {
          println("bb: " + bb + " children: " + bb.getChildren())
          for ( child <- bb.getChildren()) {
            changed += child.id
          }
        }
      }
    }
  }

  // walks through the basic blocks and *somehow* figures out the different types of expressions.
  // Then, these are stored and mapped to their position in the bitvector
  def initBitVectorMap() : HashMap[BitvectorKey, Int] = {
    var vecMap = new HashMap[BitvectorKey, Int]
    for ( (id, bb) <- BasicBlockGenie.idToBBReference) {
      for (instr <- bb.instrs) {
        val code = convertTacToBvk(instr)
        if (!code.isInstanceOf[EmptyBvk] && !vecMap.contains(code)) {
          vecMap += (code -> vecMap.size)
        }
      }
    }
    return vecMap
  }
 
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
    var bitvector = ArrayBuffer.fill(vecMap.size)(0)
    for (tac <- bb.instrs) { 
      for ( (key, index) <- vecMap) {
        if (tac.isAssign && (key.asInstanceOf[Bvk].setVars contains tac.lhs)) {
          bitvector(index) = 1
        }
      }
    }
    return bitvector
  }

  def intersect(
                vec1 : ArrayBuffer[Int],
                vec2 : ArrayBuffer[Int]
               ) : ArrayBuffer[Int] =
  {
    if (vec1.size != vec2.size) {
      println("Bad intersect shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
    }  
    var newVec = new ArrayBuffer[Int]()

    for ( i <- 0 until vec1.size ) {
      newVec += (vec1(i) * vec2(i))
    }
    return newVec
  } 

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
