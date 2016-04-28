package compile.cfg

import compile.symboltables.SymbolTable

import scala.collection.mutable
import _root_.compile.tac.ThreeAddressCode.Tac


class NormalBB(
                currSymbolTable : SymbolTable
              ) {
  val instrs : mutable.ArrayBuffer[Tac] = mutable.ArrayBuffer.empty
  var child : NormalBB = null
  var parent : NormalBB = null
  var methodTop = false
  var programStart = false
  var symbolTable = currSymbolTable
  var id = BasicBlockGenie.generateBBName()
  BasicBlockGenie.idToBBReference.put(id, this)
}

// Stores the start and end basic blocks of the method that it's calling
class MethodCallBB(
                currSymbolTable : SymbolTable,
                methodStart : NormalBB,
                methodEnd : NormalBB
              ) extends NormalBB(currSymbolTable) {
  var methodStartBB : NormalBB = methodStart
  var methodEndBB : NormalBB = methodEnd
}

class BranchBB(
                currSymbolTable : SymbolTable
              ) extends NormalBB(currSymbolTable) {
  // There should be only one instr in the list above (invariant)
  var child_else : NormalBB = null

  var preincrement : NormalBB = null // Pointer to Preincrement in a For Branch to generate TAC/Asm
  var merge : NormalBB = null // Pointer to Preincrement in a For Branch to generate TAC/Asm
  var whilestart : NormalBB = null // Pointer to Continue BB in a While
  var forstart : NormalBB = null // Pointer to start of a loop BB in a For
}

class MergeBB(
                currSymbolTable : SymbolTable
              ) extends NormalBB(currSymbolTable) {
  var parent_else : NormalBB = null
}

class JumpDestBB(
               currSymbolTable : SymbolTable
             ) extends NormalBB(currSymbolTable) {
  var label : String = null
  val jmpParents : mutable.ArrayBuffer[NormalBB] = mutable.ArrayBuffer.empty
}

