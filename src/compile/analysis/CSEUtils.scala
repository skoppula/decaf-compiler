package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.SymbolTable
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode.{TacUnOp, Tac, TacBinOp, TacCopy}
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer


object CSEUtils {

  var tempSymbolMap = Map.empty[String, (String, SymbolTable)]

  def CSESubstitionGenie(
                     startBB : NormalBB,
                     tempToSymbolMap : Map[String, (String, SymbolTable)],
                     doNotTraverseBBs : List[String],
                     tempGenie: TempVariableGenie
                   ) {

    var currentBB = startBB

    while (currentBB != null) {
      substituteCSEInBlock(currentBB, tempGenie,tempToSymbolMap)

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            CSESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id,tempGenie)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            CSESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id,tempGenie)
          } else if (BBB.whilestart == null) {
            CSESubstitionGenie(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id,tempGenie)
            CSESubstitionGenie(BBB.preincrement, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id,tempGenie)
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

  def getKilledGlobalsInBB(bb : NormalBB): Set[(String, SymbolTable)] = {
    var killedGlobalSymbols = Set.empty[(String, SymbolTable)]

    for(instr <- bb.instrs) {
      instr match {
        case tac : TacCopy => {
          // make sure addr2 is real var and addr1 is temp var
          if(!isTempVar(tac.addr1)) {
            val containingST = bb.symbolTable.getContainingSymbolTable(tac.addr1)
            val globalFieldTable = bb.symbolTable.getGlobalFieldTable
            if(containingST == null) {
              throw new SymbolVariableIsNullException("something went horribly wrong")
            } else if(globalFieldTable == null) {
              throw new SymbolVariableIsNullException("global table is null...something went horribly wrong")
            } else if (globalFieldTable == containingST) {
              killedGlobalSymbols = killedGlobalSymbols ++ Set((tac.addr1, containingST))
            }
          }
        }
        case _ => {}
      }
    }

    return killedGlobalSymbols
  }

  def getKilledGlobalsInTree(
                                startBB : NormalBB,
                                doNotTraverseBBs : List[String]
                              ): Set[(String, SymbolTable)] = {
    var currentBB = startBB
    var killedGlobalSymbols = Set.empty[(String, SymbolTable)]

    while (currentBB != null) {
      killedGlobalSymbols = killedGlobalSymbols ++ getKilledGlobalsInBB(currentBB)

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            killedGlobalSymbols = killedGlobalSymbols ++ getKilledGlobalsInTree(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            killedGlobalSymbols = killedGlobalSymbols ++ getKilledGlobalsInTree(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            killedGlobalSymbols = killedGlobalSymbols ++ getKilledGlobalsInTree(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            killedGlobalSymbols = killedGlobalSymbols ++ getKilledGlobalsInTree(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)
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

    return killedGlobalSymbols
  }

  def substituteCSEIntraBlock(
                               currentBB : NormalBB,
                               tempGenie : TempVariableGenie,
                               tempSymbolMap : Map[String, (String, SymbolTable)]
                             ) : Unit = {
    // For all tacs in the bb:
    // Step 0: Attempt substitution on the tac
    // Step 1: Update the cseIn after the tac
    val newInstrs : ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]

    // Intra block CSE means the starting CSE In is empty
    var cseIn : Map[String, Expression] = Map() 
    for(instr <- currentBB.instrs){
      // Step 0
      instr match {
        case tac : TacBinOp => {
          val lhsTemp = tac.addr2
          val rhsTemp = tac.addr3

          if(tempSymbolMap.get(lhsTemp) != None && tempSymbolMap.get(rhsTemp) != None) {
            val lhsSymbol = tempSymbolMap.get(lhsTemp).get
            val rhsSymbol = tempSymbolMap.get(rhsTemp).get

            val expr = new Expression(tac.op, Set(lhsSymbol, rhsSymbol), ArrayBuffer(lhsSymbol, rhsSymbol))

            val exprToTempBBMap : Map[Expression, String] = cseIn.map(_.swap)

            val getExprResult = exprToTempBBMap.get(expr)

            if(getExprResult != None) {
              val newTemp = getExprResult.get
              // An symbolic expression already exists, so we replace it with the temp found
              val tacCopy : ThreeAddressCode.Tac = new TacCopy(tempGenie.generateTacNumber(), tac.addr1, newTemp)
              newInstrs += tacCopy
            } else{
              newInstrs += tac
            }
          } else {
            newInstrs += tac
          }
        }
        case tac : TacUnOp => {
          val temp = tac.addr2

          if(tempSymbolMap.get(temp) != None) {
            val tempSymbol = tempSymbolMap.get(temp).get

            val expr = new Expression(tac.op, Set(tempSymbol), ArrayBuffer(tempSymbol))

            val exprToTempBBMap : Map[Expression, String] = cseIn.map(_.swap)

            val getExprResult = exprToTempBBMap.get(expr)

            if(getExprResult != None) {
              val newTemp = getExprResult.get
              val tacCopy : ThreeAddressCode.Tac = new TacCopy(tempGenie.generateTacNumber(), tac.addr1, newTemp)
              newInstrs += tacCopy
            } else{
              newInstrs += tac
            }
          } else {
            newInstrs += tac
          }
        }
        case _ => {
          newInstrs += instr
        }
      }

      // Step 1
      cseIn = CSE.computeCSEAfterTac(cseIn, instr, currentBB.symbolTable)
      // dprintln(instr.toString)
      // dprintln(cseIn.mkString)
    }

    currentBB.instrs.clear()
    currentBB.instrs ++= newInstrs

  }

  def substituteCSEInBlock(
                            currentBB : NormalBB,
                            tempGenie : TempVariableGenie,
                            tempSymbolMap : Map[String, (String, SymbolTable)]
                          ) : Unit = {

    val newInstrs : ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    var cse : Map[String, Expression] = currentBB.cseIn

    for(instr <- currentBB.instrs){
      instr match {
        case tac : TacBinOp => {
          val lhsTemp = tac.addr2
          val rhsTemp = tac.addr3

          if(tempSymbolMap.get(lhsTemp) != None && tempSymbolMap.get(rhsTemp) != None) {
            val lhsSymbol = tempSymbolMap.get(lhsTemp).get
            val rhsSymbol = tempSymbolMap.get(rhsTemp).get

            val expr = new Expression(tac.op, Set(lhsSymbol, rhsSymbol), ArrayBuffer(lhsSymbol, rhsSymbol))

            val exprToTempBBMap : Map[Expression, String] = cse.map(_.swap)

            val getExprResult = exprToTempBBMap.get(expr)

            if(getExprResult != None) {
              val newTemp = getExprResult.get
              // An symbolic expression already exists, so we replace it with the temp found
              val tacCopy : ThreeAddressCode.Tac = new TacCopy(tempGenie.generateTacNumber(), tac.addr1, newTemp)
              newInstrs += tacCopy
            } else{
              newInstrs += tac
            }
          } else {
            newInstrs += tac
          }
        }
        case tac : TacUnOp => {
          val temp = tac.addr2

          if(tempSymbolMap.get(temp) != None) {
            val tempSymbol = tempSymbolMap.get(temp).get

            val expr = new Expression(tac.op, Set(tempSymbol), ArrayBuffer(tempSymbol))

            val exprToTempBBMap : Map[Expression, String] = cse.map(_.swap)

            val getExprResult = exprToTempBBMap.get(expr)

            if(getExprResult != None) {
              val newTemp = getExprResult.get
              val tacCopy : ThreeAddressCode.Tac = new TacCopy(tempGenie.generateTacNumber(), tac.addr1, newTemp)
              newInstrs += tacCopy
            } else{
              newInstrs += tac
            }
          } else {
            newInstrs += tac
          }
        }
        case _ => {
          newInstrs += instr
        }
      }

      // Update the cse map
      cse = CSE.computeCSEAfterTac(cse, instr, currentBB.symbolTable)
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
