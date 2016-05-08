package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.SymbolTable
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode.{TacUnOp, Tac, TacBinOp, TacCopy}
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer

object DCEUtil {

  var tempSymbolMap = Map.empty[String, (String, SymbolTable)]

  def DCESubstitionGenie(
                     startBB : NormalBB,
                     tempToSymbolMap : Map[String, (String, SymbolTable)],
                     doNotTraverseBBs : List[String],
                     tempGenie: TempVariableGenie
                   ) {

    var currentBB = startBB

    while (currentBB != null) {
      substituteDCEInBlock(currentBB, tempGenie,tempToSymbolMap)

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            DCESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id,tempGenie)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            DCESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id,tempGenie)
          } else if (BBB.whilestart == null) {
            DCESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id,tempGenie)
            DCESubstitionGenie(BBB.preincrement, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id,tempGenie)
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
  }

  def substituteDCEIntraBlock(
                               currentBB : NormalBB,
                               tempGenie : TempVariableGenie,
                               tempSymbolMap : Map[String, (String, SymbolTable)]
                             ) : Unit = {
    // For all tacs in the bb:
    // Step 0: Attempt substitution on the tac
    // Step 1: Update the dceIn after the tac
    val newInstrs : ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]

    // Intra block DCE means the starting DCE In is empty
    var dceIn : Set[(String, SymbolTable)] = Set() 
    for(instr <- currentBB.instrs.reverse) {
      // Step 0
      instr match {
        case tac : TacBinOp => {
          val lhsTemp = tac.addr2
          val rhsTemp = tac.addr3

          if(tempSymbolMap.get(lhsTemp) != None && tempSymbolMap.get(rhsTemp) != None) {
            val lhsSymbol = tempSymbolMap.get(lhsTemp).get
            val rhsSymbol = tempSymbolMap.get(rhsTemp).get
            //TODO: stuff here
          } else {
            newInstrs += tac
          }
        }
        case _ => {
          newInstrs += instr
        }
      }

      // Step 1
      dceIn = DCE.computeDCEAfterTac(dceIn, instr, currentBB.symbolTable)
      dprintln(instr.toString)
      dprintln(dceIn.mkString)
    }
    currentBB.instrs.clear()
    currentBB.instrs ++= newInstrs
  }

  def substituteDCEInBlock(
                            currentBB : NormalBB,
                            tempGenie : TempVariableGenie,
                            tempSymbolMap : Map[String, (String, SymbolTable)]
                          ) : Unit = {

    val newInstrs : ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    var dce : Set[(String, SymbolTable)] = currentBB.dceIn

    for(instr <- currentBB.instrs){
      instr match {
        case tac : TacBinOp => {
          val lhsTemp = tac.addr2
          val rhsTemp = tac.addr3

          if(tempSymbolMap.get(lhsTemp) != None && tempSymbolMap.get(rhsTemp) != None) {
            val lhsSymbol = tempSymbolMap.get(lhsTemp).get
            val rhsSymbol = tempSymbolMap.get(rhsTemp).get

            val expr = new Expression(tac.op, Set(lhsSymbol, rhsSymbol), ArrayBuffer(lhsSymbol, rhsSymbol))

          } else {
            newInstrs += tac
          }
        }
        case tac : TacUnOp => {
          val temp = tac.addr2

          if(tempSymbolMap.get(temp) != None) {
            val tempSymbol = tempSymbolMap.get(temp).get
            //TODO
          } else {
            newInstrs += tac
          }
        }
        case _ => {
          newInstrs += instr
        }
      }

      // Update the dce map
      dce = DCE.computeDCEAfterTac(dce, instr, currentBB.symbolTable)
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
