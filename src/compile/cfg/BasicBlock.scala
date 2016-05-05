package compile.cfg

import compile.analysis.Expression
import compile.symboltables.SymbolTable

import scala.collection.mutable
import _root_.compile.tac.ThreeAddressCode.Tac


class NormalBB(
                currSymbolTable : SymbolTable
              ) {
  val instrs : mutable.ArrayBuffer[Tac] = mutable.ArrayBuffer.empty[Tac]
  var child : NormalBB = null
  var parent : NormalBB = null

  var cseIn = Map.empty[String, Expression]
  var cseOut = Map.empty[String, Expression]

  var availBitVectorIn : mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]
  var availBitVectorOut : mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]

  var methodTop = false
  var programStart = false
  var symbolTable = currSymbolTable
  var id = BasicBlockGenie.generateBBName()
  BasicBlockGenie.idToBBReference.put(id, this)

  def getParents() : mutable.ArrayBuffer[NormalBB] = if (parent == null) mutable.ArrayBuffer.empty else mutable.ArrayBuffer[NormalBB](parent)
  def getChildren() : mutable.ArrayBuffer[NormalBB] = if (child == null) mutable.ArrayBuffer.empty else mutable.ArrayBuffer[NormalBB](child)

  override def toString : String = {
    return "NormalBB(" + this.id + ")"
  }
}

// Stores the start and end basic blocks of the method that it's calling
class MethodCallBB(
                currSymbolTable : SymbolTable,
                name : String,
                methodStart : NormalBB,
                methodEnd : NormalBB
              ) extends NormalBB(currSymbolTable) {
  var methodName : String = name
  var methodStartBB : NormalBB = methodStart
  var methodEndBB : NormalBB = methodEnd

  override def toString : String = {
    return "MethodCallBB(" + this.id + ")"
  }
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

  override def getChildren() : mutable.ArrayBuffer[NormalBB] = {
    val children = mutable.ArrayBuffer.empty[NormalBB] 
    if (child != null) {
      children += child
    } 
    if (child_else != null) {
      children += child_else
    }
    return children
  }

  override def toString : String = {
    val m : String = if(merge == null) "" else merge.id
    val w : String = if(whilestart == null) "" else whilestart.id
    val f : String = if(preincrement == null) "" else preincrement.id
    val p : String = if(forstart == null) "" else forstart.id
    return "BranchBB(" + this.id + ", merge: " + m + ", whilestart: " + w + ", forstart: " + f + ", preincrement: " + p + ")"
  }
}

class MergeBB(
                currSymbolTable : SymbolTable
              ) extends NormalBB(currSymbolTable) {
  var parent_else : NormalBB = null

  override def getParents() : mutable.ArrayBuffer[NormalBB] = {
    if (parent == null && parent_else == null ) {
      return mutable.ArrayBuffer.empty[NormalBB]
    } else if (parent == null) {
      return mutable.ArrayBuffer[NormalBB](parent_else)
    } else if (parent_else == null) {
      return mutable.ArrayBuffer[NormalBB](parent)
    } else {
      return mutable.ArrayBuffer[NormalBB](parent, parent_else)
    }
  }
  override def toString : String = {
    return "MergeBB(" + this.id + ")"
  }
}


class JumpDestBB(
               currSymbolTable : SymbolTable
             ) extends NormalBB(currSymbolTable) {
  var label : String = null
  val jmpParents : mutable.ArrayBuffer[NormalBB] = mutable.ArrayBuffer.empty

  override def getParents() : mutable.ArrayBuffer[NormalBB] = {
    return mutable.ArrayBuffer.empty[NormalBB] ++ (jmpParents.toList :+ this.parent).toArray
  }

  override def toString : String = {
    return "JumpDestBB(" + this.id + ")"
  }
}

