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
}

// Stores the start and end basic blocks of the method that it's calling
class MethodCallBB(
                currSymbolTable : SymbolTable,
                methodStart : NormalBB,
                methodEnd : NormalBB
              ) extends NormalBB(currSymbolTable) {
}

class BranchBB(
                currSymbolTable : SymbolTable
              ) extends NormalBB(currSymbolTable) {
  // There should be only one instr in the list above (invariant)
  var child_else : NormalBB = null
}

class MergeBB(
                currSymbolTable : SymbolTable
              ) extends NormalBB(currSymbolTable) {
  var parent_else : NormalBB = null
  var child_else : NormalBB = null
}

class JumpDestBB(
               currSymbolTable : SymbolTable
             ) extends NormalBB(currSymbolTable) {
  var label : String = null
  val jmpParents : mutable.ArrayBuffer[NormalBB] = mutable.ArrayBuffer.empty
}

