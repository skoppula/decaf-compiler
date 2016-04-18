package compile.cfg

import compile.symboltables.SymbolTable

import scala.collection.mutable
import _root_.compile.tac.ThreeAddressCode.Tac

abstract class BasicBlock(
                  currSymbolTable : SymbolTable
                ) {
  val instrs : mutable.ArrayBuffer[Tac] = mutable.ArrayBuffer.empty
}

class NormalBB(
                currSymbolTable : SymbolTable,
                parent : BasicBlock
              ) extends BasicBlock(currSymbolTable) {
  var child : Option[BasicBlock] = None
}

class MethodCallBB(
                currSymbolTable : SymbolTable,
                parent : BasicBlock,
                method : MethodTopBB,
                child : BasicBlock
              ) extends BasicBlock(currSymbolTable) {
}

class MethodTopBB(
                   currSymbolTable : SymbolTable
                 ) extends BasicBlock(currSymbolTable) {
  // May not need to be option because every method has child
  var child : Option[BasicBlock] = None
  var endBlock : Option[BasicBlock] = None
}

class BranchBB(
                currSymbolTable : SymbolTable,
                parent : BasicBlock,
                child_one : BasicBlock,
                child_two : BasicBlock
              ) extends BasicBlock(currSymbolTable) {

}

class ProgramStartBB(
                currSymbolTable : SymbolTable
              ) extends BasicBlock(currSymbolTable) {

}
