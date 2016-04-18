package compile.cfg

import compile.Ir._
import compile.exceptionhandling._
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.descriptors._
import compile.tac.OpTypes._
import compile.tac.AsmGen._
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode._
import compile.util.Util.combineLinkedHashMaps
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}

class CFGGen {

  def genCFG(
              program: IrProgram,
              tempGenie: TempVariableGenie,
              methodsTable: MethodsTable,
              tacAsmMap: LinkedHashMap[Tac, List[String]]
            ): (ProgramStartBB, LinkedHashMap[String, BasicBlock]) = {

    val bbMethodMap = LinkedHashMap.empty[String, BasicBlock]

    for (method <- program.methodDecls) {
      val methodDesc = methodsTable.lookupID(method.name)
      val methodBB = genMethodDeclBB(method, tempGenie, methodDesc)
      bbMethodMap(method.name) = methodBB
    }

    val firstTac = new TacProgramEnter(tempGenie.generateTacNumber())
    val psBB: ProgramStartBB = new ProgramStartBB(methodsTable.getGlobalFieldTable)

    val strStart = new TacStringLiteralStart(tempGenie.generateTacNumber())
    psBB.instrs += strStart

    val tacs = tacAsmMap.keySet
    for (tac <- tacs) {
      if (tac.isInstanceOf[TacStringLiteral]) {
        psBB.instrs += tac
        tacAsmMap.remove(tac)
      }
    }

    val strEnd = new TacStringLiteralEnd(tempGenie.generateTacNumber())
    psBB.instrs += strEnd

    return (psBB, bbMethodMap)
  }

  def genMethodDeclBB(
                       methodDecl: IrMethodDecl,
                       tempGenie: TempVariableGenie,
                       methodDesc: MethodDescriptor
                     ): MethodTopBB = {

    val methodParamTable = methodDesc.getParamTable

    val mtBB = new MethodTopBB(methodParamTable)

    val nopTac = new TacNop(tempGenie.generateTacNumber(), "")
    mtBB.instrs += nopTac

    // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
    if (methodDecl.name == "main") {
      val mainGloblTac = new TacGlobl(tempGenie.generateTacNumber(), methodDecl.name)
      mtBB.instrs += mainGloblTac
    }

    val methodLabelTac = new TacLabel(tempGenie.generateTacNumber(), methodDecl.name)
    mtBB.instrs += methodLabelTac

    val endMethodBB = new NormalBB(methodParamTable, null) // parent as null is not good practice, but hard to circumvent

    val childBlockBB: BasicBlock = genBlockBB(methodDecl.bodyBlock, mtBB, endMethodBB, tempGenie, methodParamTable)
    mtBB.child = Option(childBlockBB)

    val methodEnterTac = new TacMethodEnter(tempGenie.generateTacNumber(), methodDesc)
    mtBB.instrs += methodEnterTac

    if (methodDesc.methodType.isInstanceOf[VoidTypeDescriptor]) {
      val voidExit = new TacReturn(tempGenie.generateTacNumber())
      endMethodBB.instrs += voidExit
    } else {
      val mainExit = new TacSystemExit(tempGenie.generateTacNumber(), -2)
      endMethodBB.instrs += mainExit
    }

    return mtBB
  }

  def genBlockBB(
                  block: IrBlock,
                  parentStart: BasicBlock,
                  parentEnd: BasicBlock,
                  tempGenie: TempVariableGenie,
                  symbolTable: SymbolTable): BasicBlock = {
    return null
  }
}
