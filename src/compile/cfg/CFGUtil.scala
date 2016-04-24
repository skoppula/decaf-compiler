package compile.cfg

import compile.tac.{AsmGen}

class CFGUtil {
  // Method to convert CFG to assembly (Austin : tentative implementation done)
  // TODO Method to print out CFG
  // TODO eventually BasicBlockremove AsmGen calls in TacGen, instead create TAC list, then CFG, then optimizations if any, then ASM
  // TODO COPY SYMBOL TABLES SO THAT IT ACTUALLY WORKS
  // TODO compress the CFG
  // SSA
  // Make sure that on every call to genBlockBB, check if end BB of block already has a child or is null

  def cfgToAsm(bb: NormalBB): List[String] = {
    // TODO : Untested
    var currentBB : NormalBB = bb
    var asm : List[String] = List()

    while (currentBB != null) {
        if (currentBB.isInstanceOf[MethodCallBB]) {
          var MCBB : MethodCallBB = currentBB.asInstanceOf[MethodCallBB]
          asm = asm ::: cfgToAsm(MCBB.methodStartBB)
          for (tac <- currentBB.instrs) {
            asm = asm ::: AsmGen.asmGen(tac, MCBB.symbolTable)
          }
          asm = asm ::: cfgToAsm(MCBB.methodEndBB)

        } else if (currentBB.isInstanceOf[BranchBB]) {
          var BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
          for (tac <- currentBB.instrs) {
            asm = asm ::: AsmGen.asmGen(tac, BBB.symbolTable)
          }
          asm = asm ::: cfgToAsm(BBB.child_else)

        } else if (currentBB.isInstanceOf[MergeBB]) {
          // TODO : What do we do with parent_else?
          var MBB : MergeBB = currentBB.asInstanceOf[MergeBB]
          for (tac <- currentBB.instrs) {
            asm = asm ::: AsmGen.asmGen(tac, MBB.symbolTable)
          }

        } else if (currentBB.isInstanceOf[JumpDestBB]) {
          // TODO : Need to actually do anything with the label field?
          // TODO : What do we do with jmpParents?
          var JDBB : JumpDestBB = currentBB.asInstanceOf[JumpDestBB]
          for (tac <- currentBB.instrs) {
            asm = asm ::: AsmGen.asmGen(tac, JDBB.symbolTable)
          }

        } else { // Just an ordinary NormalBB otherwise
          for (tac <- currentBB.instrs) {
            asm = asm ::: AsmGen.asmGen(tac, currentBB.symbolTable)
          }
        }

      currentBB = currentBB.child
    }

    return asm
  }

}
