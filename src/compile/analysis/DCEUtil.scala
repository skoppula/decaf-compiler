package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.SymbolTable
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode._
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer

object DCEUtil {
  def deleteDCEInBlock( currentBB : NormalBB
                        ) : Unit = {

    val newInstrs : ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    var dce : Set[(String, SymbolTable)] = currentBB.dceOut

    dprintln("I am block: " + currentBB.id + "with vars:")
    for (x <- dce) {
      dprintln(x._1)
    }

    for(instr <- currentBB.instrs){
      var update : Boolean = true
      instr match {
        case tac : TacBinOp => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacUnOp => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacCopy => { 
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacCopyInt => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacCopyBoolean => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacMethodCallExpr => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacArrayLeft => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case tac : TacArrayRight => {
          var v : String = tac.lhs
          var table : SymbolTable = currentBB.symbolTable.getContainingSymbolTable(v)
          update = dce.contains((v,table))
        }
        case _ => {

        }
      }
    
      // Update the dce map only if we did not delete the tac (i.e. if the lhs var was not dead)
      if (update) {
        newInstrs += instr
        dce = DCE.computeDCEAfterTac(dce, instr, currentBB.symbolTable)
      }
    }

    currentBB.instrs.clear()
    currentBB.instrs ++= newInstrs
  }
}
