package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.SymbolTable
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.ThreeAddressCode.{TacBinOp, TacCopy}

import scala.collection.mutable.ArrayBuffer

object CSEUtils {

  def substituteCSE(
                     startBB : NormalBB,
                     tempToSymbolMap : Map[String, (String, SymbolTable)],
                     doNotTraverseBBs : List[String]
                   ) {

    var currentBB = startBB

    while (currentBB != null) {
      substituteCSEInBlock(currentBB, tempToSymbolMap)

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            substituteCSE(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            substituteCSE(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            substituteCSE(BBB.child_else, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            substituteCSE(BBB.preincrement, tempToSymbolMap, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)
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

  def substituteCSEInBlock(
                            currentBB : NormalBB,
                            tempSymbolMap : Map[String,(String, SymbolTable)]
                          ): Unit = {

    for(instr <- currentBB.instrs){
      instr match {
        case tac : TacBinOp => {
          val lhsTemp = tac.addr2
          val rhsTemp = tac.addr3

          val lhsSymbol = tempSymbolMap.get(lhsTemp).get
          val rhsSymbol = tempSymbolMap.get(rhsTemp).get

          // case class Expr (op: OpEnumVal, setVars: Set[(String, SymbolTable)], listVars: ArrayBuffer[(String,SymbolTable)]) extends Expression {
          val expr = new Expression(tac.op, Set(lhsSymbol, rhsSymbol), ArrayBuffer(lhsSymbol, rhsSymbol))

          // two symbol variables -> symbol expr
          // look symbol expr cse_hash_in to see if there's temp that already contains the symbolic expression
          // if so replace the binop wiht a copy
          if(currentBB.avail_in.contains()){

          }
        }
        case _ => {}
      }
    }
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
