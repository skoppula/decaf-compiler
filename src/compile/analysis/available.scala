package compile.analysis

import math.max
import compile.cfg.NormalBB
import compile.cfg.BasicBlockGenie
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap, Map}

object available {
  def available(
                bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)]
               ) : Unit = {
    val vecMap = vec(bbMethodMap)
    val l = vecMap.size

    var (start, _) = bbMethodMap("main")
    start.in = ArrayBuffer.fill(l)(0)
    start.out = availableGen(start, vecMap)

    val bbIdMap = BasicBlockGenie.idToBBReference 

    var changed = Set[String]()
    for ( (id, bb) <- bbIdMap ) {
      changed += id
    }

    while (!changed.isEmpty) {
      for ( (id, bb) <- bbIdMap ) {
        changed -= id

        bb.in = ArrayBuffer.fill(l)(0)
        
        for ( parent <- bb.getParents() ) {
          bb.in = intersect(bb.in, parent.out) 
        }
        val oldOut = bb.out  
        bb.out = union(availableGen(bb, vecMap), minus(bb.in, availableKill(bb, vecMap)))

        if (oldOut != bb.out) {
          for ( child <- bb.getChildren()) {
            changed += child.id 
          }
        }
      }
    } 
  } 

  // walks through the basic blocks and *somehow* figures out the different types of expressions. 
  // Then, these are stored and mapped to their position in the bitvector
  def vec( 
          bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)]
         ) : Map[String, Int] = {
    return null
  }
  
  // takes in a block and the expression -> bitvector index map.
  // Returns a bitvector with GEN[i] = 1 if expression i is used.
  def availableGen(
                   bb: NormalBB, 
                   vecMap: Map[String, Int]
                  ) : ArrayBuffer[Int] = {
    return null 
  }

  // takes in a block and the expression -> bitvector index map. 
  // Returns a bitvector with KILL[i] = 1 if a variable in expression i is redefined.
  def availableKill(
                    bb: NormalBB, 
                    vecMap:  Map[String, Int]
                   ) : ArrayBuffer[Int] = {
    return null
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
