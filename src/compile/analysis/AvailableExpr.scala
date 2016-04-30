package compile.analysis

import compile.cfg.NormalBB
import compile.cfg.BasicBlockGenie
import compile.tac.ThreeAddressCode._
import compile.analysis.BitvectorKey._
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashMap}

object AvailableExpr {
  def computeAvailableExpr(
                            bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)]
                          ) : Unit = {
    val vecMap = initBitVectorMap()
    val length = vecMap.size

    val (start, _) = bbMethodMap("main")
    start.avail_in = ArrayBuffer.fill(length)(0)
    start.avail_out = availableGen(start, vecMap)

    val bbIdMap = BasicBlockGenie.idToBBReference

    var changed = Set[String]()
    for ((id, bb) <- bbIdMap) {
      changed += id
    }

    while (!changed.isEmpty) {
      for ( id <- changed ) {
        val bb = BasicBlockGenie.idToBBReference(id)
        changed -= id

        bb.avail_in = ArrayBuffer.fill(length)(0)

        for ( parent <- bb.getParents() ) {
          bb.avail_in = intersect(bb.avail_in, parent.avail_out)
        }
        val oldOut = bb.avail_out
        bb.avail_out = union(availableGen(bb, vecMap), minus(bb.avail_in, availableKill(bb, vecMap)))

        if (oldOut != bb.avail_out) {
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
        val code = convertTAC(instr)
        if (!vecMap.contains(code)) {
          vecMap += (code -> vecMap.size)
        }
      }
    }
    return vecMap
  }
 
  def convertTAC(
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
      var bitvector = ArrayBuffer[Int](vecMap.size)
      for (tac <- bb.instrs) {
        val code = convertTAC(tac)
        if (vecMap.contains(code)) {
           //TODO: implement some sort of equivalence function instead of contains.
           // Should have casework for different ops, e.g. + or * will be commutative, so a+b is the same as b+a 
           bitvector(vecMap(code)) = 1
        } else if (!code.isInstanceOf[EmptyBvk]) { 
          println("Bad shit- found an expression not in the mapping:" + code)
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
    var bitvector = ArrayBuffer[Int](vecMap.size)
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
      println("Bad shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
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
      println("Bad shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
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
      println("Bad shit- vectors are different sizes: " + vec1.size + "vs. " + vec2.size)
    }
    var newVec = new ArrayBuffer[Int]()

    for ( i <- 0 until vec1.size ) {
      newVec += (math.min(vec1(i)-vec2(i), 0))
    }  

    return newVec
  }

}