package compile.cfg

import compile.exceptionhandling.{NotForIfWhileStmtException, NullElseBlockException}
import compile.tac.ThreeAddressCode.{TacStringLiteral, Tac}
import compile.symboltables.SymbolTable
import compile.tac.{AsmGen}
import compile.util.Util.dprintln

object CFGUtil {
  // TODO Method to print out CFG
  // TODO compress the CFG
  // TODO Remove NOP, check new parent symbol table call didn't affect anythign
  // Make sure that on every call to genBlockBB, check if end BB of block already has a child or is null
  // TODO change the symboltable.getparenttable 'Changed from TacGen'
  // TODO remove Noops, efficient use of constant zero

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
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            slTacs = slTacs ::: getStringLiteralTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
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
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            compressCfg(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            compressCfg(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            compressCfg(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            compressCfg(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
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

        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            tacs = tacs ::: cfgToTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
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
      val parentBB = BasicBlockGenie.idToBBReference(parent)
      val instrs = parentBB.instrs
      if(parentBB.isInstanceOf[BranchBB]) {
        val parentBranchBB = parentBB.asInstanceOf[BranchBB]
        val merge = if(parentBranchBB.merge == null) "" else parentBranchBB.merge.id
        val preinc = if(parentBranchBB.preincrement == null) "" else parentBranchBB.preincrement.id
        val ws = if(parentBranchBB.whilestart == null) "" else parentBranchBB.whilestart.id
        dot = dot :+ "\t%s [shape=box,label=\"%s\\n\\n%s\\n%s\\n%s\\n\\n%s\"];\n".format(
          parent.substring(1),
          parent.substring(1),
          "merge: " + merge,
          "preinc: " + preinc,
          "whilest: " + ws,
          instrs.mkString("\\n")
        )
      } else {
        dot = dot :+ "\t%s [shape=box,label=\"%s\\n\\n%s\"];\n".format(parent.substring(1), parent.substring(1), instrs.mkString("\\n"))
      }
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

        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          map = addChildElseNodeToMap(map, BBB)
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            val elseMap : Map[String,Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
            map = mergeMaps(map, elseMap)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            val elseMap : Map[String,Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
            map = mergeMaps(map, elseMap)
          } else if (BBB.whilestart == null) {
            val elseMap1 : Map[String, Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            val elseMap2 : Map[String, Set[String]] = cfgToMap(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id)
            map = mergeMaps(map, elseMap1)
            map = mergeMaps(map, elseMap2)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
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

  def addChildElseNodeToMap(map:Map[String,Set[String]], node:BranchBB) : Map[String,Set[String]] = {
    val parent : String = node.id
    var set : Set[String] =
      map.get(parent) match {
        case Some(s) => s
        case None => Set()
      }

    if (node.child_else != null) {
      val child : String = node.child_else.id
      set += child
    }
    return (map - parent) + {parent -> set}
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
