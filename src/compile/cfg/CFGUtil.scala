package compile.cfg

import compile.tac.ThreeAddressCode.{TacStringLiteral, Tac}
import compile.symboltables.SymbolTable
import compile.tac.{AsmGen}

object CFGUtil {
  // Method to convert CFG to assembly (Austin : tentative implementation done)
  // TODO Method to print out CFG
  // TODO eventually BasicBlockremove AsmGen calls in TacGen, instead create TAC list, then CFG, then optimizations if any, then ASM
  // TODO COPY SYMBOL TABLES so we can run TacGen and CFGGen simulataneously
  // TODO compress the CFG
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

    println(map)

    for ((parent,children) <- map) {
      dot = dot :+ "\t%s;\n".format(parent)
      for (child <- children) {
        dot = dot :+ "\t%s -> %s;\n".format(parent, child)
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
      set = set & v
      map = (map - k) + {k -> set}
    }
    return map
  }

}
