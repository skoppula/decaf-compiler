package compile.cfg

import java.io.{PrintWriter, File}

import compile.Ir._
import compile.exceptionhandling._
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.descriptors._
import compile.tac.OpTypes._
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode._
import util.CLI
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}
import compile.util.Util.dprintln

object CFGGen {

  val bbMethodMap : LinkedHashMap[String, (NormalBB, NormalBB)] = LinkedHashMap.empty[String, (NormalBB, NormalBB)]

  def genCFG(
              program: IrProgram,
              tempGenie: TempVariableGenie,
              methodsTable: MethodsTable
            ): (NormalBB, LinkedHashMap[String, (NormalBB, NormalBB)]) = {

    var stringLiteralTacs : List[TacStringLiteral] = List()
    for (method <- program.methodDecls) {
      val methodDesc = methodsTable.lookupID(method.name)
      val methodBBs = genMethodDeclBB(method, tempGenie, methodDesc)

      stringLiteralTacs = stringLiteralTacs ::: CFGUtil.getStringLiteralTacs(methodBBs._1, List())
      bbMethodMap(method.name) = methodBBs
    }

    val firstTac = new TacProgramEnter(tempGenie.generateTacNumber())
    val psBB = new NormalBB(methodsTable.getGlobalFieldTable)
    psBB.instrs += firstTac

    val strStart = new TacStringLiteralStart(tempGenie.generateTacNumber())
    psBB.instrs += strStart

    for (slTac <- stringLiteralTacs) {
        psBB.instrs += slTac
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
                  parentStart: JumpDestBB,
                  parentEnd: JumpDestBB,
                  tempGenie: TempVariableGenie,
                  symbolTable: SymbolTable): (NormalBB, NormalBB) = {

    val blockStartBB = new NormalBB(symbolTable)
    val blockEndBB = new NormalBB(symbolTable)

    for((name, desc) <- symbolTable.symbolTableMap) {
      if(name.length() < 2 || name.substring(0,2) != ".T") {
        blockStartBB.instrs ++= initializeLocalField(name, desc, tempGenie, symbolTable)
      }
    }

    val childrenTables = symbolTable.getChildrenSymbolTables
    val expectedNumSubBlocks = childrenTables.size
    var subblockCount = 0

    // set child of the thing above
    // set the parent of the upcoming thing

    // blank initializations
    var stmtStartBB : NormalBB = new NormalBB(symbolTable)
    var stmtEndBB : NormalBB = new NormalBB(symbolTable)
    blockStartBB.child = stmtStartBB
    stmtStartBB.parent = blockStartBB
    stmtStartBB.child = stmtEndBB
    stmtEndBB.parent = stmtStartBB
    var currParent = stmtEndBB

    var jmpEncountered : Boolean = false //check to make sure no stmts after return/continue/break found
    var jmpCheck : Boolean = true

    for (stmt <- block.stmts) {
      if(jmpEncountered) {
        println("bad jmpcheck!!!")
        println(stmt)
        jmpCheck = false
      }
      // pass down the correct symbol table
      var stmtBBs : (NormalBB, NormalBB) = (null, null)
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

      if(stmt.isInstanceOf[IrReturnStmt] || stmt.isInstanceOf[IrContinueStmt] || stmt.isInstanceOf[IrBreakStmt]) {
        jmpEncountered = true
      }

      currParent = stmtEndBB
    }

    if(expectedNumSubBlocks != subblockCount) {
      throw new ExpectedSubBlockCountNotActualException("Expected " + expectedNumSubBlocks.toString  + " but got " + subblockCount.toString)
    } else if (!jmpCheck) {
      throw new StmtAfterContinueBreakReturnException("Statement after continue/break/return")
    }

    stmtEndBB.child = blockEndBB
    blockEndBB.parent = stmtEndBB

    return (blockStartBB, blockEndBB)
  }

  def genStmtBB(
               stmt: IrStatement,
               parentStart: JumpDestBB,
               parentEnd: JumpDestBB,
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
      case s: IrWhileStmt => {
        return genIrWhileStmtBB(s, tempGenie, symbolTable)
      }
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

  def genIrWhileStmtBB(
                      stmt: IrWhileStmt,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : (NormalBB, NormalBB) = {

    val whileStartBB = new JumpDestBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val whileEndBB = new JumpDestBB(symbolTable) // Changed from TacGen

    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()

    whileStartBB.label = startLabel
    whileEndBB.label = endLabel

    val startLabelTAC = new TacLabel(tempGenie.generateTacNumber(), startLabel)
    whileStartBB.instrs += startLabelTAC

    val (condTemp, condStartBB, condEndBB) = genExprBB(stmt.boolExpr, tempGenie, symbolTable.getParentSymbolTable)
    whileStartBB.child = condStartBB
    condStartBB.parent = whileStartBB

    val whileJmp = new BranchBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    whileJmp.parent = condEndBB
    condEndBB.child = whileJmp

    val jmpIfFalseTAC = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel)
    whileJmp.instrs += jmpIfFalseTAC

    whileJmp.whilestart = whileStartBB
    whileJmp.merge = whileEndBB

    val (blockStartBB, blockEndBB) = genBlockBB(stmt.bodyBlock, whileStartBB, whileEndBB, tempGenie, symbolTable)

    // Connect whileJmp to blockStartBB
    whileJmp.child_else = blockStartBB
    blockStartBB.parent = whileJmp

    // TODO: do we need to do the .child check here necessary?
    // I believe the end needs to be the same as the while end here for breaks
    if (whileJmp.child == null) {
      whileJmp.child = whileEndBB
      whileEndBB.parent = whileJmp
    }

    val loopBackTAC = new TacGoto(tempGenie.generateTacNumber(), startLabel)
    blockEndBB.instrs += loopBackTAC

    // TODO: .child check here necessary?
    if (blockEndBB.child == null) {
      // TODO: Add a new parent to whileStartBB?
      blockEndBB.child = whileStartBB
    }

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    whileEndBB.instrs += endLabelTAC

    return (whileStartBB, whileEndBB)
  }

  def genIrForStmtBB(
                    stmt: IrForStmt,
                    tempGenie: TempVariableGenie,
                    symbolTable: SymbolTable
                  ) : (NormalBB, NormalBB) = {
    val forStartBB = new NormalBB(symbolTable) // Changed from TacGen
    // init value creation
    // label: start_loop
    // check condition
    // .... block shit
    val forPreIncrementBB = new JumpDestBB(symbolTable) // Changed from TacGen
    // increment, loop back to start_loop
    val forEndBB = new JumpDestBB(symbolTable) // Changed from TacGen

    val trueStartLabel: String = tempGenie.generateLabel()

    val jumpStartLabel: String = tempGenie.generateLabel()
    forPreIncrementBB.label = jumpStartLabel
    val preIncLabelTAC = new TacLabel(tempGenie.generateTacNumber(), jumpStartLabel)
    forPreIncrementBB.instrs += preIncLabelTAC

    val endLabel: String = tempGenie.generateLabel()
    forEndBB.label = endLabel

    val (initValTemp, initValStartBB, initValEndBB) = genExprBB(stmt.initVal, tempGenie, symbolTable)
    forStartBB.child = initValStartBB
    initValStartBB.parent = forStartBB

    val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
    val initValBB = new NormalBB(symbolTable) // Changed from TacGen
    initValBB.parent = initValEndBB
    initValEndBB.child = initValBB
    initValBB.instrs += copyIntoVar

    // Austin: changed forStartBB to a JumpDestBB type; don't forget to check that this is OK / still correct. This is necessary to prevent the compressor from merging forLoopBeginBB with earlier blocks
    val forLoopBeginBB = new JumpDestBB(symbolTable) // Changed from TacGen
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
    endValEndBB.instrs += forCondCmpTAC

    val forJmpBB = new BranchBB(symbolTable) // Changed from TacGen
    forJmpBB.parent = endValEndBB
    endValEndBB.child = forJmpBB
    val forIfFalseTAC = new TacIfFalse(tempGenie.generateTacNumber(), lessThan, endLabel) //if index >= endVal, exit for loop
    forJmpBB.instrs += forIfFalseTAC

    val (blockStartBB, blockEndBB) = genBlockBB(stmt.bodyBlock, forPreIncrementBB, forEndBB, tempGenie, symbolTable)
    blockStartBB.parent = forJmpBB
    forJmpBB.child_else = blockStartBB
    forJmpBB.child = forEndBB
    forEndBB.parent = forJmpBB

    forJmpBB.preincrement = forPreIncrementBB
    forJmpBB.merge = forEndBB
    forJmpBB.forstart = forLoopBeginBB

    blockEndBB.child = forPreIncrementBB
    forPreIncrementBB.parent = blockEndBB

    val incrementBB = new NormalBB(symbolTable)

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
                   parentStart: JumpDestBB,
                   parentEnd: JumpDestBB,
                   tempGenie: TempVariableGenie,
                   symbolTable: SymbolTable,
                   symbolTable2: SymbolTable = null
                 ) : (NormalBB, NormalBB) = {

    val ifStartBB = new NormalBB(symbolTable.getParentSymbolTable) // Changed from TacGen
    val ifEndBB = new MergeBB(symbolTable) // Changed from TacGen

    val (condTemp, condStartBB, condEndBB) = genExprBB(stmt.cond, tempGenie, symbolTable.getParentSymbolTable) // Changed from TacGen
    ifStartBB.child = condStartBB
    condStartBB.parent = ifStartBB

    val ifJmpBB = new BranchBB(symbolTable.getParentSymbolTable) // Changed from TacGen
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
      ifJmpBB.child_else = ifTrueBlockStartBB
      ifTrueBlockStartBB.parent = ifJmpBB

      if(ifTrueBlockEndBB.child == null) {
        ifTrueBlockEndBB.child = ifEndBB
        ifEndBB.parent = ifTrueBlockEndBB
      }

      val gotoEndTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
      ifTrueBlockEndBB.instrs += gotoEndTac

      val elseBlockStart = new NormalBB(symbolTable2)
      val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
      elseBlockStart.instrs += elseLabelTac
      elseBlockStart.parent = ifJmpBB
      ifJmpBB.child = elseBlockStart

      ifJmpBB.merge = ifEndBB


      val (ifFalseBlockStartBB, ifFalseBlockEndBB) = genBlockBB(stmt.elseBlock.get, parentStart, parentEnd, tempGenie, symbolTable2)
      elseBlockStart.child = ifFalseBlockStartBB
      ifFalseBlockStartBB.parent = elseBlockStart

      if(ifFalseBlockEndBB.child == null) {
        ifFalseBlockEndBB.child = ifEndBB
        ifEndBB.parent_else = ifFalseBlockEndBB
      }

    } else {
      val ifFalseTac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel) // jump to the end of the if
      ifJmpBB.instrs += ifFalseTac

      val (ifTrueBlockStartBB, ifTrueBlockEndBB) = genBlockBB(stmt.ifBlock, parentStart, parentEnd, tempGenie, symbolTable)
      ifJmpBB.child_else = ifTrueBlockStartBB
      ifTrueBlockStartBB.parent = ifJmpBB

      if(ifTrueBlockEndBB.child == null) {
        ifTrueBlockEndBB.child = ifEndBB
        ifEndBB.parent = ifTrueBlockEndBB
      }

      ifJmpBB.merge = ifEndBB

      ifJmpBB.child = ifEndBB
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
        if(bbMethodMap.get(name).isEmpty) {
          // Must be a callout
          methodCallBB = new MethodCallBB(symbolTable, null, null)
        } else {
          methodCallBB = new MethodCallBB(symbolTable, bbMethodMap.get(name).get._1, bbMethodMap.get(name).get._2)
        }
        val tac = new TacMethodCallStmt(tempGenie.generateTacNumber(), name, tempArgs.toList)
        methodCallBB.parent = currParent
        currParent.child = methodCallBB
        methodCallBB.instrs += tac
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

    val assignStartBB = new NormalBB(symbolTable)
    val assignEndBB = new NormalBB(symbolTable)

    val (exprTemp, exprStartBB, exprEndBB) = genExprBB(stmt.expr, tempGenie, symbolTable)

    assignStartBB.child = exprStartBB
    exprStartBB.parent = assignStartBB

    stmt match {
      case IrEqualsAssignStmt(irLoc, expr, _) => {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val singleLocTac = new TacCopy(tempGenie.generateTacNumber(), name, exprTemp)
            assignEndBB.instrs += singleLocTac
            exprEndBB.child = assignEndBB
            assignEndBB.parent = exprEndBB
            return (assignStartBB, assignEndBB)
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            exprEndBB.child = indexStartBB
            indexStartBB.parent = exprEndBB

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
            assignEndBB.instrs += minusTac
            exprEndBB.child = assignEndBB
            assignEndBB.parent = exprEndBB
            return (assignStartBB, assignEndBB)
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            val (checkStartBB, checkEndBB) = checkArrayBoundsBB(name, indexTemp, tempGenie, symbolTable)

            exprEndBB.child = indexStartBB
            indexStartBB.parent = exprEndBB

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
            assignEndBB.instrs += plusTac
            exprEndBB.child = assignEndBB
            assignEndBB.parent = exprEndBB
            return (assignStartBB, assignEndBB)
          }

          case IrArrayLocation(name, index, _) => {
            val (indexTemp, indexStartBB, indexEndBB) = genExprBB(index, tempGenie, symbolTable)
            val (checkStartBB, checkEndBB) = checkArrayBoundsBB(name, indexTemp, tempGenie, symbolTable)

            exprEndBB.child = indexStartBB
            indexStartBB.parent = exprEndBB

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
                      parentEnd: JumpDestBB,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : (NormalBB, NormalBB) = {
    val breakBB = new NormalBB(symbolTable)
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentEnd.label)
    breakBB.instrs += gotoTac
    breakBB.child = parentEnd
    parentEnd.jmpParents += breakBB
    return (breakBB, breakBB)
  }

  def genIrContinueStmtBB(
                         stmt: IrContinueStmt,
                         parentStart: JumpDestBB,
                         tempGenie: TempVariableGenie,
                         symbolTable: SymbolTable
                       ): (NormalBB, NormalBB) = {
    val continueBB = new NormalBB(symbolTable)
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentStart.label)
    continueBB.instrs += gotoTac
    continueBB.child = parentStart
    parentStart.jmpParents += continueBB
    return (continueBB, continueBB)
  }

  def genExprBB(
               expr: IrExpression,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable
             ) : (String, NormalBB, NormalBB) = {
    expr match {
      case singleLoc: IrSingleLocation => {
        return genIrSingleLocationBB(singleLoc, tempGenie, symbolTable)
      }
      case arrayLoc: IrArrayLocation => {
        return genIrArrayLocationBB(arrayLoc, tempGenie, symbolTable)
      }
      case methodCall: IrMethodCallExpr => {
        return genIrMethodCallExprBB(methodCall, tempGenie, symbolTable)
      }
      case intLit: IrIntLiteral => {
        return genIrIntLiteralBB(intLit, tempGenie, symbolTable)
      }
      case charLit: IrCharLiteral => {
        return genIrCharLiteralBB(charLit, tempGenie, symbolTable)
      }
      case boolLit: IrBooleanLiteral => {
        return genIrBooleanLiteralBB(boolLit, tempGenie, symbolTable)
      }
      case binOpExpr: IrBinOpExpr => {
        return genIrBinOpExprBB(binOpExpr, tempGenie, symbolTable)
      }
      case unOpExpr: IrUnOpExpr => {
        return genIrUnOpExprBB(unOpExpr, tempGenie, symbolTable)
      }
      case ternOpExpr: IrTernOpExpr => {
        return genIrTernOpExprBB(ternOpExpr, tempGenie, symbolTable)
      }
      case _ => {
        return null
      }
    }
  }

  def genIrTernOpExprBB(
                       ternOpExpr: IrTernOpExpr,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (String, NormalBB, NormalBB) = {

    val ternOpStartBB = new NormalBB(symbolTable)
    val ternOpEndBB = new MergeBB(symbolTable)

    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val elseLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()

    val (condTemp, condStartBB, condEndBB) = genExprBB(ternOpExpr.cond, tempGenie, symbolTable)
    condStartBB.parent = ternOpStartBB
    ternOpStartBB.child = condStartBB

    val endLabelGotoTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
    val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
    val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)

    val jmpTernOpBB = new BranchBB(symbolTable)
    val tac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, elseLabel)
    condEndBB.child = jmpTernOpBB
    jmpTernOpBB.parent = condEndBB
    jmpTernOpBB.instrs += tac

    jmpTernOpBB.merge = ternOpEndBB

    val (leftTemp, leftStartBB, leftEndBB) = genExprBB(ternOpExpr.leftExpr, tempGenie, symbolTable)
    jmpTernOpBB.child_else = leftStartBB
    leftStartBB.parent = jmpTernOpBB
    val leftCopy = TacCopy(tempGenie.generateTacNumber(), temp, leftTemp)
    leftEndBB.instrs += leftCopy
    leftEndBB.instrs += endLabelGotoTac
    leftEndBB.child = ternOpEndBB

    val elseBB = new NormalBB(symbolTable)
    elseBB.parent = jmpTernOpBB
    jmpTernOpBB.child = elseBB
    elseBB.instrs += elseLabelTac

    val (rightTemp, rightStartBB, rightEndBB) = genExprBB(ternOpExpr.rightExpr, tempGenie, symbolTable)
    elseBB.child = rightStartBB
    rightStartBB.parent = elseBB
    val rightCopy = TacCopy(tempGenie.generateTacNumber(), temp, rightTemp)
    rightEndBB.instrs += rightCopy
    rightEndBB.child = ternOpEndBB

    ternOpEndBB.instrs += endLabelTac
    ternOpEndBB.parent = leftEndBB
    ternOpEndBB.parent_else = rightEndBB

    return (temp, ternOpStartBB, ternOpEndBB)
  }

  def genIrUnOpExprBB(
                     unOpExpr: IrUnOpExpr,
                     tempGenie: TempVariableGenie,
                     symbolTable: SymbolTable
                   ) : (String, NormalBB, NormalBB) = {

    val unOpStartBB = new NormalBB(symbolTable)
    val unOpEndBB = new NormalBB(symbolTable)

    val temp: String = tempGenie.generateName()

    val (exprTemp, exprStartBB, exprEndBB) = genExprBB(unOpExpr.expr, tempGenie, symbolTable)
    unOpStartBB.child = exprStartBB
    exprStartBB.parent = unOpStartBB

    exprEndBB.child = unOpEndBB
    unOpEndBB.parent = exprEndBB

    var op: UnOpEnumVal = null
    unOpExpr.unop match {
      case minus: IrMinusOp => {
        op = MINUS
        symbolTable.insert(temp, new IntTypeDescriptor)
      }
      case not: IrNotOp => {
        op = NOT
        symbolTable.insert(temp, new BoolTypeDescriptor)
      }
      case size: IrArraySizeOp => {
        op = SIZE
        symbolTable.insert(temp, new IntTypeDescriptor)
      }
    }

    val tac = new TacUnOp(tempGenie.generateTacNumber(), temp, op, exprTemp)
    unOpEndBB.instrs += tac

    return (temp, unOpStartBB, unOpEndBB)
  }

  def genIrBinOpExprBB(
                      binOpExpr: IrBinOpExpr,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : (String, NormalBB, NormalBB) = {

    val binOpStartBB = new NormalBB(symbolTable)
    val binOpEndBB = new MergeBB(symbolTable)

    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val (leftTemp, leftStartBB, leftEndBB) = genExprBB(binOpExpr.leftExpr, tempGenie, symbolTable)
    binOpStartBB.child = leftStartBB
    leftStartBB.parent = binOpStartBB

    val endLabel = tempGenie.generateLabel()

    val (rightTemp, rightStartBB, rightEndBB) = genExprBB(binOpExpr.rightExpr, tempGenie, symbolTable)

    val scBB = new NormalBB(symbolTable)
    val scJmpBB = new BranchBB(symbolTable)

    if(binOpExpr.binOp.isInstanceOf[IrAndOp] || binOpExpr.binOp.isInstanceOf[IrOrOp]) {
      // If leftTemp is false, and Op is AND, copy leftTemp to temp, and skip over binOp evaluation and short circuit 2
      // If leftTemp is true, and Op is OR, copy leftTemp to temp, and skip over binOp evaluation
      scBB.parent = leftEndBB
      leftEndBB.child = scBB
      scBB.child = scJmpBB
      scJmpBB.parent = scBB
      scJmpBB.child = binOpEndBB

      val copyTac = new TacCopy(tempGenie.generateTacNumber(), temp, leftTemp)
      scBB.instrs += copyTac

      if(binOpExpr.binOp.isInstanceOf[IrAndOp]) {
        val ifTac = new TacIfFalse(tempGenie.generateTacNumber(), leftTemp, endLabel)
        scJmpBB.instrs += ifTac
      } else if(binOpExpr.binOp.isInstanceOf[IrOrOp]) {
        val ifTac = new TacIf(tempGenie.generateTacNumber(), leftTemp, endLabel)
        scJmpBB.instrs += ifTac
      }

      scJmpBB.child_else = rightStartBB
      rightStartBB.parent = scJmpBB

      scJmpBB.merge = binOpEndBB

    } else {
      leftEndBB.child = rightStartBB
      rightStartBB.parent = leftEndBB
    }

    var op : BinOpEnumVal = null
    binOpExpr.binOp match {
      case IrMulOp() => op = MULT
      case IrDivOp() => op = DIV
      case IrModOp() => op = MOD
      case IrAddOp() => op = ADD
      case IrSubOp() => op = SUB
      case IrAndOp() => op = AND
      case IrOrOp()  => op = OR
      case IrLtOp()  => op = LT
      case IrLteOp() => op = LTE
      case IrGtOp()  => op = GT
      case IrGteOp() => op = GTE
      case IrEqualOp()  => op = EQ
      case IrNotEqualOp() => op = NEQ
    }

    val opBB = new NormalBB(symbolTable)
    rightEndBB.child = opBB
    opBB.parent = rightEndBB
    opBB.child = binOpEndBB
    binOpEndBB.parent_else = opBB


    val tac = new TacBinOp(tempGenie.generateTacNumber(), temp, leftTemp, op, rightTemp)
    opBB.instrs += tac

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    binOpEndBB.instrs += endLabelTAC

    return (temp, binOpStartBB, binOpEndBB)
  }

  def genIrMethodCallExprBB(
                           methodExpr: IrMethodCallExpr,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, NormalBB, NormalBB) =  {

    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]

    val methodStartBB = new NormalBB(symbolTable)
    var methodCallBB : NormalBB = null
    var currParent = methodStartBB
    val name : String = methodExpr.name

    for (arg <- methodExpr.args) {
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
    if(bbMethodMap.get(name).isEmpty) {
      // Must be a callout
      methodCallBB = new MethodCallBB(symbolTable, null, null)
    } else {
      methodCallBB = new MethodCallBB(symbolTable, bbMethodMap.get(name).get._1, bbMethodMap.get(name).get._2)
    }
    methodCallBB.parent = currParent
    currParent.child = methodCallBB

    val tac = new TacMethodCallExpr(tempGenie.generateTacNumber(), temp, name, tempArgs.toList)
    methodCallBB.instrs += tac

    return (temp, methodStartBB, methodCallBB)
  }

  def genIrArrayLocationBB(
                          arrayLoc: IrArrayLocation,
                          tempGenie: TempVariableGenie,
                          symbolTable: SymbolTable
                        ) : (String, NormalBB, NormalBB) = {

    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val (index, indexStartBB, indexEndBB) = genExprBB(arrayLoc.index, tempGenie, symbolTable)
    val (checkStartBB, checkEndBB) = checkArrayBoundsBB(arrayLoc.name, index, tempGenie, symbolTable)
    indexEndBB.child = checkStartBB
    checkStartBB.parent = indexEndBB
    val tac = new TacArrayRight(tempGenie.generateTacNumber(), temp, arrayLoc.name, index)
    checkEndBB.instrs += tac
    return (temp, indexStartBB, checkEndBB)
  }

  def genIrSingleLocationBB(
                           singleLoc: IrSingleLocation,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, NormalBB, NormalBB) = {
    val slBB = new NormalBB(symbolTable)
    val temp: String = tempGenie.generateName()
    val locType = symbolTable.lookupID(singleLoc.name)
    locType match {
      case a: ArrayBaseDescriptor => {
        symbolTable.insert(temp, new IntArrayTypeDescriptor(a.length))
      }
      case p: PrimitiveBaseDescriptor => {
        symbolTable.insert(temp, new IntTypeDescriptor)
      }
    }

    val tac = new TacCopy(tempGenie.generateTacNumber(), temp, singleLoc.name)
    slBB.instrs += tac
    return (temp, slBB, slBB)
  }

  def genIrBooleanLiteralBB(
                           boolLit: IrBooleanLiteral,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, NormalBB, NormalBB) = {
    val boolLitBB = new NormalBB(symbolTable)
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    val tac = new TacCopyBoolean(tempGenie.generateTacNumber(), temp, boolLit.value)
    boolLitBB.instrs += tac
    return (temp, boolLitBB, boolLitBB)
  }

  def genIrIntLiteralBB(
                       intLit: IrIntLiteral,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (String, NormalBB, NormalBB) = {
    val intLitBB = new NormalBB(symbolTable)
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    if(!intLit.value.isDefined) {
      throw new CompilerProblem("Trying to assemble int literal copy, int literal has no value tho!", intLit.loc)
    }
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, intLit.value.get.toLong)
    intLitBB.instrs += tac
    return (temp, intLitBB, intLitBB)
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteralBB(
                        charLit: IrCharLiteral,
                        tempGenie: TempVariableGenie,
                        symbolTable: SymbolTable
                      ) : (String, NormalBB, NormalBB) = {
    val charLitBB = new NormalBB(symbolTable)
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, charLit.value.toLong)
    charLitBB.instrs += tac
    return (temp, charLitBB, charLitBB)
  }
}
