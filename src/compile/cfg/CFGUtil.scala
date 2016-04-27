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

}
