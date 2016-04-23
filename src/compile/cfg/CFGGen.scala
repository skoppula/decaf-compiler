package compile.cfg

import compile.Ir._
import compile.exceptionhandling._
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.descriptors._
import compile.tac.OpTypes.{ADD, SUB, LT, SIZE}
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}

object CFGGen {

  val bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)] = LinkedHashMap.empty[String, (NormalBB, NormalBB)]

  def genCFG(
              program: IrProgram,
              tempGenie: TempVariableGenie,
              methodsTable: MethodsTable,
              tacAsmMap: LinkedHashMap[Tac, List[String]]
            ): (NormalBB, LinkedHashMap[String, (NormalBB, NormalBB)]) = {

    for (method <- program.methodDecls) {
      val methodDesc = methodsTable.lookupID(method.name)
      val methodBBs = genMethodDeclBB(method, tempGenie, methodDesc)
      bbMethodMap(method.name) = methodBBs
    }

    val firstTac = new TacProgramEnter(tempGenie.generateTacNumber())
    val psBB = new NormalBB(methodsTable.getGlobalFieldTable)
    psBB.instrs += firstTac

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
                     ): (NormalBB, NormalBB) = {

    val methodParamTable = methodDesc.getParamTable

    val mtBB = new NormalBB(methodParamTable)
    mtBB.methodTop = true

    val nopTac = new TacNop(tempGenie.generateTacNumber(), "")
    mtBB.instrs += nopTac

    // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
    if (methodDecl.name == "main") {
      val mainGloblTac = new TacGlobl(tempGenie.generateTacNumber(), methodDecl.name)
      mtBB.instrs += mainGloblTac
    }

    val methodLabelTac = new TacLabel(tempGenie.generateTacNumber(), methodDecl.name)
    mtBB.instrs += methodLabelTac

    val endMethodBB = new NormalBB(methodParamTable)

    val (childStartBB, childEndBB) = genBlockBB(methodDecl.bodyBlock, null, null, tempGenie, methodParamTable)
    mtBB.child = childStartBB
    childStartBB.parent = mtBB
    childEndBB.child = endMethodBB
    endMethodBB.parent = childEndBB

    val methodEnterTac = new TacMethodEnter(tempGenie.generateTacNumber(), methodDesc)
    mtBB.instrs += methodEnterTac

    if (methodDesc.methodType.isInstanceOf[VoidTypeDescriptor]) {
      val voidExit = new TacReturn(tempGenie.generateTacNumber())
      endMethodBB.instrs += voidExit
    } else {
      val mainExit = new TacSystemExit(tempGenie.generateTacNumber(), -2)
      endMethodBB.instrs += mainExit
    }

    return (mtBB, endMethodBB)
  }

  def initializeLocalField(
                            name : String,
                            desc : BaseDescriptor,
                            tempGenie: TempVariableGenie,
                            symbolTable: SymbolTable
                          ): ArrayBuffer[Tac] = {
    val tacs = ArrayBuffer.empty[Tac]

    desc match {
      case itd: IntTypeDescriptor => {
        val copyIntTac = new TacCopyInt(tempGenie.generateTacNumber(), name, 0)
        tacs += copyIntTac
      }
      case btd: BoolTypeDescriptor => {
        val copyBoolTac = new TacCopyBoolean(tempGenie.generateTacNumber(), name, false)
        tacs += copyBoolTac
      }
      case iad: IntArrayTypeDescriptor => {
        val indexTemp = tempGenie.generateName()
        symbolTable.insert(indexTemp, new IntTypeDescriptor)

        val constantZeroTemp = tempGenie.generateName()
        symbolTable.insert(constantZeroTemp, new IntTypeDescriptor)

        // Good for optimization
        val copyConstantZero = new TacCopyInt(tempGenie.generateTacNumber(), constantZeroTemp, 0)
        tacs += copyConstantZero

        for(i <- 0 until iad.size.toInt) {
          val copyIndex = new TacCopyInt(tempGenie.generateTacNumber(), indexTemp, i)
          tacs += copyIndex

          val arrayAssign = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, constantZeroTemp)
          tacs += arrayAssign
        }
      }
      case bad: BoolArrayTypeDescriptor => {
        val indexTemp = tempGenie.generateName()
        symbolTable.insert(indexTemp, new IntTypeDescriptor)

        val constantFalseTemp = tempGenie.generateName()
        symbolTable.insert(constantFalseTemp, new BoolTypeDescriptor)

        val copyConstantFalse = new TacCopyBoolean(tempGenie.generateTacNumber(), constantFalseTemp, false)
        tacs += copyConstantFalse

        for(i <- 0 until bad.size.toInt) {
          val copyIndex = new TacCopyInt(tempGenie.generateTacNumber(), indexTemp, i)
          tacs += copyIndex

          val arrayAssign = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, constantFalseTemp)
          tacs += arrayAssign
        }
      }
    }
    return tacs
  }

  def genBlockBB(
                  block: IrBlock,
                  parentStart: NormalBB,
                  parentEnd: NormalBB,
                  tempGenie: TempVariableGenie,
                  symbolTable: SymbolTable): (NormalBB, NormalBB) = {

    val blockStartBB = new NormalBB(symbolTable)

    for((name, desc) <- symbolTable.symbolTableMap) {
      if(name.size < 2 || name.substring(0,2) != ".T") {
        blockStartBB.instrs ++= initializeLocalField(name, desc, tempGenie, symbolTable)
      }
    }

    val childrenTables = symbolTable.getChildrenSymbolTables
    val expectedNumSubBlocks = childrenTables.size
    var subblockCount = 0

    // set child of the thing above
    // set the parent of the upcoming thing
    var currParent = blockStartBB
    var stmtBBs : (NormalBB, NormalBB) = (null, null)
    var stmtStartBB : NormalBB = null
    var stmtEndBB : NormalBB = null
    var jmpEncountered : Boolean = false //check to make sure no stmts after return/continue/break found
    var jmpCheck : Boolean = true

    for (stmt <- block.stmts) {
      if(jmpEncountered) {
        jmpCheck = false
      }
      // pass down the correct symbol table
      if(stmt.isInstanceOf[IrIfStmt] && stmt.asInstanceOf[IrIfStmt].elseBlock.isDefined) {
        stmtBBs = genStmtBB(stmt, parentStart, parentEnd, tempGenie, childrenTables(subblockCount), childrenTables(subblockCount+1))
        subblockCount += 2
      } else if(stmt.isInstanceOf[IrForStmt] || stmt.isInstanceOf[IrWhileStmt] || stmt.isInstanceOf[IrIfStmt]) {
        stmtBBs = genStmtBB(stmt, parentStart, parentEnd, tempGenie, childrenTables(subblockCount))
        subblockCount += 1
      } else {
        stmtBBs = genStmtBB(stmt, parentStart, parentEnd, tempGenie, symbolTable)
      }

      // the specific genStmtBB handles the parent-child setting relationships depending if stmt is continue/break/return
      stmtStartBB = stmtBBs._1
      stmtEndBB = stmtBBs._2
      stmtStartBB.parent = currParent
      currParent.child = stmtStartBB

      if(stmt.isInstanceOf[IrReturnStmt] || !stmt.isInstanceOf[IrContinueStmt] || !stmt.isInstanceOf[IrBreakStmt]) {
        jmpEncountered = true
      }

      currParent = stmtEndBB
    }

    if(expectedNumSubBlocks != subblockCount) {
      throw new ExpectedSubBlockCountNotActualException("Expected " + expectedNumSubBlocks.toString  + " but got " + subblockCount.toString)
    } else if (!jmpCheck) {
      throw new StmtAfterContinueBreakReturnException("Statement after continue/break/return")
    }

    return (currParent, stmtEndBB)
  }

  def genStmtBB(
               stmt: IrStatement,
               parentStart: NormalBB,
               parentEnd: NormalBB,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable,
               symbolTable2: SymbolTable = null
             ) : (NormalBB, NormalBB) = {
    stmt match {
      case s: IrAssignStmt => {
        return genIrAssignStmtBB(s, tempGenie, symbolTable)
      }
      case s: IrMethodCallStmt => {
        return genIrMethodCallStmtBB(s, tempGenie, symbolTable)
      }
      case s: IrIfStmt => {
        if(s.elseBlock.isDefined) {
          return genIrIfStmtBB(s, parentStart, parentEnd, tempGenie, symbolTable, symbolTable2)
        } else {
          return genIrIfStmtBB(s, parentStart, parentEnd, tempGenie, symbolTable)
        }
      }
      case s: IrForStmt => {
        return genIrForStmtBB(s, tempGenie, symbolTable)
      }
//      case s: IrWhileStmt => {
//        return genIrWhileStmt(s, tempGenie, symbolTable)
//      }
      case s: IrReturnStmt => {
        return genIrReturnStmtBB(s, tempGenie, symbolTable)
      }
      case s: IrBreakStmt => {
        return genIrBreakStmtBB(s, parentEnd, tempGenie, symbolTable)
      }
      case s: IrContinueStmt => {
        return genIrContinueStmtBB(s, parentStart, tempGenie, symbolTable)
      }
      case _ => {
        throw new NoMatchingStatementException("No matching statement", stmt.nodeLoc)
      }
    }
  }

  def genIrForStmtBB(
                    stmt: IrForStmt,
                    tempGenie: TempVariableGenie,
                    symbolTable: SymbolTable
                  ) : (NormalBB, NormalBB) = {

    val forStartBB = new NormalBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val forEndBB = new MergeBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val forPreIncrementBB = new NormalBB(symbolTable.getParentSymbolTable) // Changed from TacGen

    val trueStartLabel: String = tempGenie.generateLabel()
    val jumpStartLabel: String = tempGenie.generateLabel()
    forPreIncrementBB.label = jumpStartLabel
    val endLabel: String = tempGenie.generateLabel()
    forEndBB.label = endLabel

    val (initValTemp, initValStartBB, initValEndBB) = genExprBB(stmt.initVal, tempGenie, symbolTable)
    forStartBB.child = initValStartBB
    initValStartBB.parent = forStartBB

    val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
    val initValBB = new NormalBB(symbolTable.getParentSymbolTable)
    initValBB.parent = initValEndBB
    initValEndBB.child = initValBB
    initValBB.instrs += copyIntoVar

    val forLoopBeginBB = new NormalBB(symbolTable.getParentSymbolTable)
    forLoopBeginBB.label = trueStartLabel
    forLoopBeginBB.parent = initValBB
    initValBB.child = forLoopBeginBB

    val forBeginTAC = new TacLabel(tempGenie.generateTacNumber(), trueStartLabel) // beginning of the for loop
    forLoopBeginBB.instrs += forBeginTAC

    val (endValTemp, endValStartBB, endValEndBB) = genExprBB(stmt.endVal, tempGenie, symbolTable)
    endValStartBB.parent = forLoopBeginBB
    forLoopBeginBB.child = endValStartBB

    // lessThan is boolean representing if index < endVal
    val lessThan: String = tempGenie.generateName()
    symbolTable.insert(lessThan, new BoolTypeDescriptor())
    val forCondCmpTAC = new TacBinOp(tempGenie.generateTacNumber(), lessThan, initValTemp, LT, endValTemp)
    forLoopBeginBB.instrs += forCondCmpTAC

    val forJmpBB = new BranchBB(symbolTable.getParentSymbolTable)
    forJmpBB.parent = forLoopBeginBB
    val forIfFalseTAC = new TacIfFalse(tempGenie.generateTacNumber(), lessThan, endLabel) //if index >= endVal, exit for loop
    forJmpBB.instrs += forIfFalseTAC

    val (blockStartBB, blockEndBB) = genBlockBB(stmt.bodyBlock, forPreIncrementBB, forEndBB, tempGenie, symbolTable)
    blockStartBB.parent = forJmpBB
    forJmpBB.child = blockStartBB
    forJmpBB.child_else = forEndBB
    forEndBB.parent = forJmpBB

    blockEndBB.child = forPreIncrementBB
    forPreIncrementBB.parent = blockEndBB

    val incrementBB = new NormalBB(symbolTable.getParentSymbolTable)

    if (stmt.inc.isDefined) {
      val (incTemp, incStartBB, incEndBB) = genExprBB(stmt.inc.get, tempGenie, symbolTable)
      forPreIncrementBB.child = incStartBB
      incStartBB.parent = forPreIncrementBB

      val forIncTAC = new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by inc
      incrementBB.instrs += forIncTAC

      val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
      incrementBB.instrs += copyIntoVar

      incrementBB.parent = incEndBB
      incEndBB.child = incrementBB

    } else {
      val incTemp : String = tempGenie.generateName()
      symbolTable.insert(incTemp, new IntTypeDescriptor())
      val loadIncTAC = new TacCopyInt(tempGenie.generateTacNumber(), incTemp, 1)
      val incOpTAC = new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by 1
      val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)

      incrementBB.instrs += loadIncTAC
      incrementBB.instrs += incOpTAC
      incrementBB.instrs += copyIntoVar
      incrementBB.parent = forPreIncrementBB
      forPreIncrementBB.child = incrementBB
    }
    val loopTAC = new TacGoto(tempGenie.generateTacNumber(), trueStartLabel) // continue looping
    incrementBB.instrs += loopTAC
    incrementBB.child = forLoopBeginBB

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    forEndBB.instrs += endLabelTAC

    return (forStartBB, forEndBB)
  }


  def genIrIfStmtBB(
                   stmt: IrIfStmt,
                   parentStart: NormalBB,
                   parentEnd: NormalBB,
                   tempGenie: TempVariableGenie,
                   symbolTable: SymbolTable,
                   symbolTable2: SymbolTable = null
                 ) : (NormalBB, NormalBB) = {

    val ifStartBB = new NormalBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val ifEndBB = new MergeBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val (condTemp, condStartBB, condEndBB) = genExprBB(stmt.cond, tempGenie, symbolTable.getParentSymbolTable)
    ifStartBB.child = condStartBB
    condStartBB.parent = ifStartBB

    val ifJmpBB = new BranchBB(symbolTable.getParentSymbolTable)
    condEndBB.child = ifJmpBB
    ifJmpBB.parent = condEndBB

    val endLabel: String = tempGenie.generateLabel()
    val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    ifEndBB.instrs += endLabelTac

    if (stmt.elseBlock.isDefined) {
      // Generate jmp-if-false and if-block
      val elseLabel: String = tempGenie.generateLabel()
      val ifFalseTac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, elseLabel) // jump to the else block
      ifJmpBB.instrs += ifFalseTac

      val (ifTrueBlockStartBB, ifTrueBlockEndBB) = genBlockBB(stmt.ifBlock, parentStart, parentEnd, tempGenie, symbolTable)
      ifJmpBB.child = ifTrueBlockStartBB
      ifTrueBlockStartBB.parent = ifJmpBB

      val gotoEndTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
      ifTrueBlockEndBB.instrs += gotoEndTac
      if(ifTrueBlockEndBB.child == null) {
        ifTrueBlockEndBB.child = ifEndBB
        ifEndBB.parent = ifTrueBlockEndBB
      }

      val elseBlockStart = new NormalBB(symbolTable2)
      val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
      elseBlockStart.instrs += elseLabelTac
      elseBlockStart.parent = ifJmpBB
      ifJmpBB.child_else = elseBlockStart


      val (ifFalseBlockStartBB, ifFalseBlockEndBB) = genBlockBB(stmt.elseBlock.get, parentStart, parentEnd, tempGenie, symbolTable2)
      elseBlockStart.child = ifFalseBlockStartBB
      ifFalseBlockStartBB.parent = elseBlockStart

      if(ifTrueBlockEndBB.child == null) {
        ifFalseBlockEndBB.child = ifEndBB
        ifEndBB.parent_else = ifFalseBlockEndBB
      }

    } else {
      val ifFalseTac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel) // jump to the end of the if
      ifJmpBB.instrs += ifFalseTac

      val (ifTrueBlockStartBB, ifTrueBlockEndBB) = genBlockBB(stmt.ifBlock, parentStart, parentEnd, tempGenie, symbolTable)
      ifJmpBB.child = ifTrueBlockStartBB
      ifTrueBlockStartBB.parent = ifJmpBB

      if(ifTrueBlockEndBB.child == null) {
        ifTrueBlockEndBB.child = ifEndBB
        ifEndBB.parent = ifTrueBlockEndBB
      }

      ifJmpBB.child_else = ifEndBB
      ifEndBB.parent_else = ifJmpBB
    }

    return (ifStartBB, ifEndBB)
  }

  def genIrMethodCallStmtBB(
                           stmt: IrMethodCallStmt,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (NormalBB, NormalBB) = {

    val callExpr: IrCallExpr = stmt.methCall
    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]

    val methodStartBB = new NormalBB(symbolTable)
    var methodCallBB : NormalBB = null
    var currParent = methodStartBB

    callExpr match {
      case IrMethodCallExpr(name, args, _) => {
        for (arg <- args) {
          arg match {
            case IrCallExprArg(argExpr, _) => {
              val (argTemp, argStartBB, argEndBB) = genExprBB(argExpr, tempGenie, symbolTable)
              currParent.child = argStartBB
              argStartBB.parent = currParent

              currParent = argEndBB
              tempArgs += argTemp
            }

            case IrCallStringArg(strLit, _) => {
              val stringArgBB = new NormalBB(symbolTable)
              val strLitLabel = tempGenie.generateLabel()
              val strTac = new TacStringLiteral(tempGenie.generateTacNumber(), strLitLabel, strLit.value)
              stringArgBB.instrs += strTac
              currParent.child = stringArgBB
              stringArgBB.parent = currParent

              currParent = stringArgBB
              tempArgs += strLitLabel
            }
          }
        }
        methodCallBB = new MethodCallBB(symbolTable, bbMethodMap.get(name).get._1, bbMethodMap.get(name).get._2)
        val tac = new TacMethodCallStmt(tempGenie.generateTacNumber(), name, tempArgs.toList)
        methodCallBB.parent = currParent
        currParent.child = methodCallBB
      }
    }
    return (methodStartBB, methodCallBB)
  }


  def checkArrayBoundsBB(
                        arrayName : String,
                        indexTemp: String,
                        tempGenie : TempVariableGenie,
                        symbolTable : SymbolTable
                      ) : (NormalBB, NormalBB) = {

    val checkBB = new NormalBB(symbolTable)

    // Get the size of the array
    val sizeOfArray: String = tempGenie.generateName()
    symbolTable.insert(sizeOfArray, new IntTypeDescriptor())
    val tacSizeOfArray = new TacUnOp(tempGenie.generateTacNumber(), sizeOfArray, SIZE, arrayName)
    checkBB.instrs += tacSizeOfArray

    // Check whether the array index is too big
    val sizeCheckBool: String = tempGenie.generateName()
    symbolTable.insert(sizeCheckBool, new BoolTypeDescriptor())
    val sizeCondCmpTAC = new TacBinOp(tempGenie.generateTacNumber(), sizeCheckBool, indexTemp, LT, sizeOfArray)
    checkBB.instrs += sizeCondCmpTAC

    // Initialize constant zero for lower bound check
    val constantZeroTemp = tempGenie.generateName()
    symbolTable.insert(constantZeroTemp, new IntTypeDescriptor)
    // TODO Optimize
    val copyConstantZero = new TacCopyInt(tempGenie.generateTacNumber(), constantZeroTemp, 0)
    checkBB.instrs += copyConstantZero

    // Check whether the array index is too small
    val lowerSizeCheckBool: String = tempGenie.generateName()
    symbolTable.insert(lowerSizeCheckBool, new BoolTypeDescriptor())
    val lowerSizeCondTAC = new TacBinOp(tempGenie.generateTacNumber(), lowerSizeCheckBool, indexTemp, LT, constantZeroTemp)
    checkBB.instrs += lowerSizeCondTAC

    val accessIndexLabel: String = tempGenie.generateLabel()
    val errorOutLabel: String = tempGenie.generateLabel()

    // If index is not > 0, jump to the system exit (-1)
    val lowerboundsTAC = new TacIf(tempGenie.generateTacNumber(), lowerSizeCheckBool, errorOutLabel)
    checkBB.instrs += lowerboundsTAC

    // If index is within bounds, jump over the system exit (-1)
    val sizeIfTAC = new TacIf(tempGenie.generateTacNumber(), sizeCheckBool, accessIndexLabel)
    checkBB.instrs += sizeIfTAC

    // Label right before error out
    val errorOutLabelTAC = new TacLabel(tempGenie.generateTacNumber(), errorOutLabel)
    checkBB.instrs += errorOutLabelTAC

    val sysExitOne = new TacSystemExit(tempGenie.generateTacNumber(), -1)
    checkBB.instrs += sysExitOne

    // Label for everything is all good
    val accessIndexLabelTAC = new TacLabel(tempGenie.generateTacNumber(), accessIndexLabel)
    checkBB.instrs += accessIndexLabelTAC

    return (checkBB, checkBB)
  }


  def genIrAssignStmtBB(
                       stmt: IrAssignStmt,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (NormalBB, NormalBB) = {

    val (exprTemp, exprStartBB, exprEndBB) = genExprBB(stmt.expr, tempGenie, symbolTable)

    val assignStartBB = new NormalBB(symbolTable)
    val assignEndBB = new NormalBB(symbolTable)

    stmt match {
      case IrEqualsAssignStmt(irLoc, expr, _) => {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val singleLocTac = new TacCopy(tempGenie.generateTacNumber(), name, exprTemp)
            assignStartBB.instrs += singleLocTac
            return (assignStartBB, assignStartBB)
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            assignStartBB.child = indexStartBB
            indexStartBB.parent = assignStartBB

            val (checkBoundsStartBB, checkBoundsEndBB) = checkArrayBoundsBB(name, indexTemp, tempGenie, symbolTable)
            indexEndBB.child = checkBoundsStartBB
            checkBoundsStartBB.parent = indexEndBB
            checkBoundsEndBB.child = assignEndBB
            assignEndBB.parent = checkBoundsEndBB

            // Copy RHS into array location
            val arrayLocTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, exprTemp)
            assignEndBB.instrs += arrayLocTac

            return (assignStartBB, assignEndBB)
          }
        }
      }

      case IrMinusAssignStmt(irLoc, expr, _) =>  {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val minusTac = new TacBinOp(tempGenie.generateTacNumber(), name, name, SUB, exprTemp)
            assignStartBB.instrs += minusTac
            return (assignStartBB, assignStartBB)
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            val (checkStartBB, checkEndBB) = checkArrayBoundsBB(name, indexTemp, tempGenie, symbolTable)

            assignStartBB.child = indexStartBB
            indexStartBB.parent = assignStartBB

            indexEndBB.child = checkStartBB
            checkStartBB.parent = indexEndBB

            val temp: String = tempGenie.generateName()
            symbolTable.insert(temp, new IntTypeDescriptor())

            val arrayRightTac = new TacArrayRight(tempGenie.generateTacNumber(), temp, name, indexTemp)
            val arrayOpTac = new TacBinOp(tempGenie.generateTacNumber(), temp, temp, SUB, exprTemp)
            val arrayLeftTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, temp)
            assignEndBB.instrs += arrayRightTac
            assignEndBB.instrs += arrayOpTac
            assignEndBB.instrs += arrayLeftTac

            checkEndBB.child = assignEndBB
            assignEndBB.parent = checkEndBB

            return (assignStartBB, assignEndBB)
          }
        }
      }

      case IrPlusAssignStmt(irLoc, expr, _) => {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val plusTac = new TacBinOp(tempGenie.generateTacNumber(), name, name, ADD, exprTemp)
            assignStartBB.instrs += plusTac
            return (assignStartBB, assignStartBB)
          }

          case IrArrayLocation(name, index, _) => {
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            val (checkStartBB, checkEndBB) = checkArrayBoundsBB(name, indexTemp, tempGenie, symbolTable)

            assignStartBB.child = indexStartBB
            indexStartBB.parent = assignStartBB

            indexEndBB.child = checkStartBB
            checkStartBB.parent = indexEndBB

            val temp: String = tempGenie.generateName()
            symbolTable.insert(temp, new IntTypeDescriptor())

            val arrayRightTac = new TacArrayRight(tempGenie.generateTacNumber(), temp, name, indexTemp)
            val arrayOpTac = new TacBinOp(tempGenie.generateTacNumber(), temp, name, ADD, exprTemp)
            val arrayLeftTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, temp)

            assignEndBB.instrs += arrayRightTac
            assignEndBB.instrs += arrayOpTac
            assignEndBB.instrs += arrayLeftTac

            checkEndBB.child = assignEndBB
            assignEndBB.parent = checkEndBB

            return (assignStartBB, assignEndBB)
          }
        }
      }
    }

    throw new NoMatchingStatementException("in genIrAssignStmt()", stmt.expr.nodeLoc)
  }

  def genIrReturnStmtBB(
                       stmt: IrReturnStmt,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (NormalBB, NormalBB) = {

    val startContBB = new NormalBB(symbolTable)
    val endContBB = new NormalBB(symbolTable)

    if (stmt.value.isDefined) {
      val (retTemp, exprStartBB, exprEndBB) = genExprBB(stmt.value.get, tempGenie, symbolTable)
      startContBB.child = exprStartBB
      exprStartBB.parent = startContBB
      exprEndBB.child = endContBB
      endContBB.parent = exprEndBB
      val returnValueTac = new TacReturnValue(tempGenie.generateTacNumber(), retTemp)
      endContBB.instrs += returnValueTac
    } else {
      startContBB.child = endContBB
      endContBB.parent = startContBB
    }

    val returnTac = new TacReturn(tempGenie.generateTacNumber())
    endContBB.instrs += returnTac
    return (startContBB, endContBB)
  }

  def genIrBreakStmtBB(
                      stmt: IrBreakStmt,
                      parentEnd: NormalBB,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : (NormalBB, NormalBB) = {
    val breakBB = new NormalBB(symbolTable)
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentEnd.label)
    breakBB.instrs += gotoTac
    breakBB.child = parentEnd
    return (breakBB, breakBB)
  }

  def genIrContinueStmtBB(
                         stmt: IrContinueStmt,
                         parentStart: NormalBB,
                         tempGenie: TempVariableGenie,
                         symbolTable: SymbolTable
                       ): (NormalBB, NormalBB) = {
    val continueBB = new NormalBB(symbolTable)
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentStart.label)
    continueBB.instrs += gotoTac
    continueBB.child = parentStart
    return (continueBB, continueBB)
  }

  def genExprBB(
               expr: IrExpression,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable
             ) : (String, NormalBB, NormalBB) = {
    expr match {
//      case singleLoc: IrSingleLocation => {
//        return genIrSingleLocation(singleLoc, tempGenie, symbolTable)
//      }
//      case arrayLoc: IrArrayLocation => {
//        return genIrArrayLocation(arrayLoc, tempGenie, symbolTable)
//      }
//      case methodCall: IrMethodCallExpr => {
//        return genIrMethodCallExpr(methodCall, tempGenie, symbolTable)
//      }
//      case intLit: IrIntLiteral => {
//        return genIrIntLiteral(intLit, tempGenie, symbolTable)
//      }
//      case charLit: IrCharLiteral => {
//        return genIrCharLiteral(charLit, tempGenie, symbolTable)
//      }
//      case boolLit: IrBooleanLiteral => {
//        return genIrBooleanLiteral(boolLit, tempGenie, symbolTable)
//      }
//      case binOpExpr: IrBinOpExpr => {
//        return genIrBinOpExpr(binOpExpr, tempGenie, symbolTable)
//      }
//      case unOpExpr: IrUnOpExpr => {
//        return genIrUnOpExpr(unOpExpr, tempGenie, symbolTable)
//      }
//      case ternOpExpr: IrTernOpExpr => {
//        return genIrTernOpExpr(ternOpExpr, tempGenie, symbolTable)
//      }
      case _ => {
        return null
      }
    }
  }
}
