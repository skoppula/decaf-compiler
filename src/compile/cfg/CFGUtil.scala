package compile.cfg

import compile.tac.ThreeAddressCode.{TacStringLiteral, Tac}
import compile.symboltables.SymbolTable
import compile.tac.{AsmGen}

object CFGUtil {
  // TODO Method to print out CFG
  // TODO compress the CFG
  // TODO Remove NOP, check new parent symbol table call didn't affect anythign
  // Make sure that on every call to genBlockBB, check if end BB of block already has a child or is null

  def getStringLiteralTacs(bb : NormalBB, doNotTraverseBBs : List[String]) : List[TacStringLiteral] = {
    var currentBB : NormalBB = bb
    var slTacs : List[TacStringLiteral] = List()

    while (currentBB != null) {
      for (tac <- currentBB.instrs) {
        if(tac.isInstanceOf[TacStringLiteral]) {
          slTacs = slTacs :+ tac.asInstanceOf[TacStringLiteral]
          currentBB.instrs -= tac
        }
      }
      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(currentBB.child.id)) {
          if(BBB.preincrement == null) {
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.child.id)
          } else {
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
            slTacs = slTacs ::: getStringLiteralTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
          }
        } else if (BBB.child_else == null) {
          print("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }
    }

    return slTacs

  }

  // TODO : Untested
  def compressCfg(bb: NormalBB, doNotTraverseBBs : List[String]) : Boolean = { // Dummy boolean return type
    var currentBB : NormalBB = bb
    var tacs : List[(Tac, SymbolTable)] = List()

    while(currentBB != null && currentBB.child != null) {

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(currentBB.child.id)) {
          if(BBB.preincrement == null) {
            compressCfg(BBB.child_else, doNotTraverseBBs :+ BBB.child.id)
          } else {
            compressCfg(BBB.child_else, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
            compressCfg(BBB.preincrement, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
          }
        }

        currentBB = currentBB.child

      } else { // MethodCallBB, MergeBB, JumpDestBB, or an ordinary NormalBB
        if (isOrdinaryBB(currentBB.child)) { // Just an ordinary NormalBB
          if (doNotTraverseBBs.contains(currentBB.child.id)) {
            currentBB = null
          } else {
            for (tac <- currentBB.child.instrs) {
              currentBB.instrs += tac
            }
            currentBB.child = currentBB.child.child
            if (currentBB.child != null) {
              currentBB.child.parent = currentBB
            }
          }
        } else {
          currentBB = currentBB.child
        }

      }

    }

    return true
    
  }


  def isOrdinaryBB(bb: NormalBB) : Boolean = {
    // Checks that we don't have any of the fancy basic blocks
    return (!bb.isInstanceOf[MethodCallBB]) && (!bb.isInstanceOf[BranchBB]) && (!bb.isInstanceOf[MergeBB]) && (!bb.isInstanceOf[JumpDestBB])
  }

  def compressNormalBB(bb1: NormalBB, bb2: NormalBB) {

  }

  // TODO : Untested
  def cfgToTacs(bb: NormalBB, doNotTraverseBBs : List[String]): List[(Tac, SymbolTable)] = {
    var currentBB : NormalBB = bb
    var tacs : List[(Tac, SymbolTable)] = List()

    while (currentBB != null) {
      if (currentBB.isInstanceOf[MethodCallBB]) {
        val MCBB : MethodCallBB = currentBB.asInstanceOf[MethodCallBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, MCBB.symbolTable)
        }

      } else if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, BBB.symbolTable)
        }

        if(BBB.child_else != null && !doNotTraverseBBs.contains(currentBB.child.id)) {
          if(BBB.preincrement == null) {
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.child.id)
          } else {
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
            tacs = tacs ::: cfgToTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
          }
        } else if (BBB.child_else == null) {
          print("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      } else if (currentBB.isInstanceOf[MergeBB]) {
        val MBB : MergeBB = currentBB.asInstanceOf[MergeBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, MBB.symbolTable)
        }

      } else if (currentBB.isInstanceOf[JumpDestBB]) {
        val JDBB : JumpDestBB = currentBB.asInstanceOf[JumpDestBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, JDBB.symbolTable)
        }

      } else { // Just an ordinary NormalBB otherwise
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, currentBB.symbolTable)
        }
      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }
    }

    return tacs
  }

  def tacsToAsm(tacs: List[(Tac, SymbolTable)]): List[String] = {
    var asm : List[String] = List()

    for((tac, symbolTable) <- tacs) {
      asm = asm ::: AsmGen.asmGen(tac, symbolTable)
      asm = asm :+ "\n"
    }

    return asm
  }

  def mapToDot(map : Map[String, Set[String]]) : List[String] = {
    var dot : List[String] = List()
    dot = dot :+ "digraph G {\n"

    for ((parent,children) <- map) {
      val instrs = BasicBlockGenie.idToBBReference(parent).instrs
      dot = dot :+ "\t%s [shape=box,label=\"%s\\n\\n%s\"];\n".format(parent.substring(1), parent.substring(1), instrs.mkString("\\n"))
      for (child <- children) {
        dot = dot :+ "\t%s -> %s;\n".format(parent.substring(1), child.substring(1))
      }
    }

    dot = dot :+ "}\n"
    return dot
  }

  def cfgToMap(bb: NormalBB, doNotTraverseBBs : List[String]) : Map[String,Set[String]] = {
    // This returns a map of bb ids -> set of bb ids
    var currentBB : NormalBB = bb
    var map : Map[String, Set[String]] = Map()

    while (currentBB != null) {
      if (currentBB.isInstanceOf[MethodCallBB]) {
        val MCBB : MethodCallBB = currentBB.asInstanceOf[MethodCallBB]
        map = addNodeToMap(map, MCBB)
      } else if (currentBB.isInstanceOf[BranchBB]) {

        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        map = addNodeToMap(map, BBB)

        if(BBB.child_else != null && !doNotTraverseBBs.contains(currentBB.child.id)) {
          if(BBB.preincrement == null) {
            val elseMap : Map[String,Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.child.id)
            map = mergeMaps(map, elseMap)
          } else {
            val elseMap1 : Map[String, Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
            val elseMap2 : Map[String, Set[String]] = cfgToMap(BBB.preincrement, doNotTraverseBBs :+ BBB.child.id :+ BBB.preincrement.id)
            map = mergeMaps(map, elseMap1)
            map = mergeMaps(map, elseMap2)

          }
        } else if (BBB.child_else == null) {
          print("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      } else if (currentBB.isInstanceOf[MergeBB]) {
        val MBB : MergeBB = currentBB.asInstanceOf[MergeBB]
        map = addNodeToMap(map, MBB)
      } else if (currentBB.isInstanceOf[JumpDestBB]) {
        val JDBB : JumpDestBB = currentBB.asInstanceOf[JumpDestBB]
        map = addNodeToMap(map, JDBB)
      } else { // Just an ordinary NormalBB otherwise
        map = addNodeToMap(map, currentBB)
      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }

    }

    return map
  }

  def addNodeToMap(map:Map[String,Set[String]], node:NormalBB) : Map[String,Set[String]] = {   
    val parent : String = node.id
    var set : Set[String] =
      map.get(parent) match {
        case Some(s) => s
        case None => Set()
      }

    if (node.child != null) {
      val child : String = node.child.id
      set += child
    } 
      return (map - parent) + {parent -> set}
  }

  def mergeMaps(map1:Map[String, Set[String]], map2:Map[String, Set[String]]) : Map[String, Set[String]]= {
    var map : Map[String, Set[String]] = map1

    for ((k,v) <- map2) {
      var set : Set[String] =
        map.get(k) match {
          case Some(s) => s
          case None => Set()
        }
      set = set | v
      map = (map - k) + {k -> set}
    }

    return map
  }

}
