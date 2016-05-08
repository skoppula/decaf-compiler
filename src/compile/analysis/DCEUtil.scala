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

  def mergeSymbolMaps(
                       map1: Map[String, (String, SymbolTable)],
                       map2 : Map[String, (String, SymbolTable)]
                     ) : Map[String, (String, SymbolTable)] = {
    var map : Map[String, (String, SymbolTable)] = map1

    for ((k,v) <- map2) {
      if(map.contains(k)) {
        throw new TempVariableAlreadyExistsInGlobalMapException("Uh oh spaghetti")
      } else {
        map = map + {k -> v}
      }
    }
    return map
  }

  def isTempVar(id : String) : Boolean = {
    return id.length >= 2 && id.substring(0,2) == ".T"
  }


  def processTempsInBlock(bb : NormalBB) : Map[String, (String, SymbolTable)] = {
    // Processes TACS in block and generates a temp to symbol mapping

    var tempToSymbolMap = Map.empty[String, (String, SymbolTable)]

    for(instr <- bb.instrs) {
      instr match {
        case tac : TacCopy => {
          // make sure addr2 is real var and addr1 is temp var
          if(isTempVar(tac.addr1) && !isTempVar(tac.addr2)) {
            val containingST = bb.symbolTable.getContainingSymbolTable(tac.addr2)
            if(containingST == null) {
              throw new SymbolVariableIsNullException("something went horribly wrong")
            }
            tempToSymbolMap = tempToSymbolMap + {tac.addr1 -> (tac.addr2, containingST)}
          }
        }
        case _ => {}
      }
    }

    return tempToSymbolMap
  }

  def genTempToSymbolMap(
              bb: NormalBB,
              doNotTraverseBBs : List[String]
            ) : Map[String, (String, SymbolTable)] = {
    // Returns a map from temp variables to symbol variables and their corresponding symbol table scope
    var currentBB : NormalBB = bb

    var tempToSymbolMap = Map.empty[String, (String, SymbolTable)]

    while (currentBB != null) {
      tempToSymbolMap = mergeSymbolMaps(tempToSymbolMap, processTempsInBlock(currentBB))

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            tempToSymbolMap = mergeSymbolMaps(tempToSymbolMap, genTempToSymbolMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id))
          } else if (BBB.preincrement == null) {
            // Must be while statement
            tempToSymbolMap = mergeSymbolMaps(tempToSymbolMap, genTempToSymbolMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id))
          } else if (BBB.whilestart == null) {
            tempToSymbolMap = mergeSymbolMaps(tempToSymbolMap, genTempToSymbolMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id))
            tempToSymbolMap = mergeSymbolMaps(tempToSymbolMap, genTempToSymbolMap(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id))
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
    return tempToSymbolMap
  }

}
