package compile.tac

import compile.Ir._
import compile.exceptionhandling._
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.descriptors._
import compile.tac.OpTypes._
import compile.tac.AsmGen._
import compile.tac.ThreeAddressCode._
import compile.util.Util.combineLinkedHashMaps
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}

object TacGen {

  def gen(
           program: IrProgram,
           tempGenie: TempVariableGenie,
           methodsTable : MethodsTable
         ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    for (method <- program.methodDecls) {
      val methodDesc = methodsTable.lookupID(method.name)
      val methodAsm = genMethodDecl(method, tempGenie, methodDesc)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, methodAsm)
    }

    // Rearrange string literals TACs to the top of the TAC list; form a .text section
    val stringLitLHM = LinkedHashMap.empty[Tac, List[String]]

    val firstTac = new TacProgramEnter(tempGenie.generateTacNumber())
    stringLitLHM(firstTac) = asmGen(firstTac, methodsTable.getGlobalFieldTable)

    val strStart = new TacStringLiteralStart(tempGenie.generateTacNumber())
    stringLitLHM(strStart) = asmGen(strStart, methodsTable.getGlobalFieldTable)

    val tacs = tacAsmMap.keySet
    for(tac <- tacs) {
      if(tac.isInstanceOf[TacStringLiteral]) {
        stringLitLHM(tac) = tacAsmMap(tac)
        tacAsmMap.remove(tac)
      }
    }

    val strEnd = new TacStringLiteralEnd(tempGenie.generateTacNumber())
    stringLitLHM(strEnd) = asmGen(strEnd, methodsTable.getGlobalFieldTable)

    return combineLinkedHashMaps(stringLitLHM, tacAsmMap)
  }

  def genMethodDecl(
                     methodDecl: IrMethodDecl,
                     tempGenie: TempVariableGenie,
                     methodDesc : MethodDescriptor
                   ) : LinkedHashMap[Tac, List[String]] = {

    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val methodParamTable = methodDesc.getParamTable

    val nopTac = new TacNop(tempGenie.generateTacNumber(), "")
    tacAsmMap(nopTac) = asmGen(nopTac, methodParamTable)

    // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
    if (methodDecl.name == "main") {
      val mainGloblTac = new TacGlobl(tempGenie.generateTacNumber(), methodDecl.name)
      tacAsmMap(mainGloblTac) = asmGen(mainGloblTac, methodParamTable)
    }
    
    val methodLabelTac = new TacLabel(tempGenie.generateTacNumber(), methodDecl.name)
    tacAsmMap(methodLabelTac) = asmGen(methodLabelTac, methodParamTable)

    val blockLHM = genBlock(methodDecl.bodyBlock, null, null, tempGenie, methodParamTable)

    val methodEnterTac = new TacMethodEnter(tempGenie.generateTacNumber(), methodDesc)
    tacAsmMap(methodEnterTac) = asmGen(methodEnterTac, methodParamTable)

    if(methodDesc.methodType.isInstanceOf[VoidTypeDescriptor]) {
      val voidExit = new TacReturn(tempGenie.generateTacNumber())
      blockLHM(voidExit) = asmGen(voidExit, methodParamTable)
    } else {
      val mainExit = new TacSystemExit(tempGenie.generateTacNumber(), -2)
      blockLHM(mainExit) = asmGen(mainExit, methodParamTable)
    }

    return combineLinkedHashMaps(tacAsmMap, blockLHM)
  }

  // == Expr gening ==

  //returns (temp_var, code) where temp_var is where the expression is allocated, and code is the list of TACs.
  def genExpr(
               expr: IrExpression,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable
             ) : (String, LinkedHashMap[Tac, List[String]]) = {
    expr match {
      case singleLoc: IrSingleLocation => {
        return genIrSingleLocation(singleLoc, tempGenie, symbolTable)
      }
      case arrayLoc: IrArrayLocation => {
        return genIrArrayLocation(arrayLoc, tempGenie, symbolTable)
      }
      case methodCall: IrMethodCallExpr => {
        return genIrMethodCallExpr(methodCall, tempGenie, symbolTable)
      }
      case intLit: IrIntLiteral => {
        return genIrIntLiteral(intLit, tempGenie, symbolTable)
      }
      case charLit: IrCharLiteral => {
        return genIrCharLiteral(charLit, tempGenie, symbolTable)
      }
      case boolLit: IrBooleanLiteral => {
        return genIrBooleanLiteral(boolLit, tempGenie, symbolTable)
      }
      case binOpExpr: IrBinOpExpr => {
        return genIrBinOpExpr(binOpExpr, tempGenie, symbolTable)
      }
      case unOpExpr: IrUnOpExpr => {
        return genIrUnOpExpr(unOpExpr, tempGenie, symbolTable)
      }
      case ternOpExpr: IrTernOpExpr => {
        return genIrTernOpExpr(ternOpExpr, tempGenie, symbolTable)
      }
      case _ => {
        return null
      }
    }
  }

  def genIrTernOpExpr(
                       ternOpExpr: IrTernOpExpr,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (String, LinkedHashMap[Tac, List[String]]) = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val elseLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()

    val (condTemp, condMap) = genExpr(ternOpExpr.cond, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, condMap) // generate code for evaluating the conditional expr


    val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    val endLabelGotoTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
    val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
    

    val tac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, elseLabel)
    tacAsmMap(tac) = asmGen(tac, symbolTable) // ifFalse condExpr, goto elseLabel

    val (leftTemp, leftMap) = genExpr(ternOpExpr.leftExpr, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, leftMap) // if block code
    val leftCopy = TacCopy(tempGenie.generateTacNumber(), temp, leftTemp)
    tacAsmMap(leftCopy) = asmGen(leftCopy, symbolTable)

    tacAsmMap(endLabelGotoTac) = asmGen(endLabelGotoTac, symbolTable) // done evaluating; goto endLabel

    tacAsmMap(elseLabelTac) = asmGen(elseLabelTac, symbolTable) // else label

    val (rightTemp, rightMap) = genExpr(ternOpExpr.rightExpr, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, rightMap) // else block code
    val rightCopy = TacCopy(tempGenie.generateTacNumber(), temp, rightTemp)
    tacAsmMap(rightCopy) = asmGen(rightCopy, symbolTable)

    tacAsmMap(endLabelTac) = asmGen(endLabelTac, symbolTable) // end label
    
    return (temp, tacAsmMap)
  }

  def genIrUnOpExpr(
                     unOpExpr: IrUnOpExpr,
                     tempGenie: TempVariableGenie,
                     symbolTable: SymbolTable
                   ) : (String, LinkedHashMap[Tac, List[String]]) = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()

    val (exprTemp, exprMap) = genExpr(unOpExpr.expr, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, exprMap) // generates code for the expr

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
    tacAsmMap(tac) = asmGen(tac, symbolTable) // generates code for applying the unary op

    return (temp, tacAsmMap)
  }

  def genIrBinOpExpr(
                      binOpExpr: IrBinOpExpr,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : (String, LinkedHashMap[Tac, List[String]]) = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val (leftTemp, leftMap) = genExpr(binOpExpr.leftExpr, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, leftMap)

    val endLabel = tempGenie.generateLabel()

    // If leftTemp is false, and Op is AND, copy leftTemp to temp, and skip over binOp evaluation and short circuit 2
    // If leftTemp is true, and Op is OR, copy leftTemp to temp, and skip over binOp evaluation

    if(binOpExpr.binOp.isInstanceOf[IrAndOp]) {
      val copyTac = new TacCopy(tempGenie.generateTacNumber(), temp, leftTemp)
      val ifTac = new TacIfFalse(tempGenie.generateTacNumber(), leftTemp, endLabel)
      tacAsmMap(copyTac) = asmGen(copyTac, symbolTable)
      tacAsmMap(ifTac) = asmGen(ifTac, symbolTable)
    } else if(binOpExpr.binOp.isInstanceOf[IrOrOp]) {
      val copyTac = new TacCopy(tempGenie.generateTacNumber(), temp, leftTemp)
      val ifTac = new TacIf(tempGenie.generateTacNumber(), leftTemp, endLabel)
      tacAsmMap(copyTac) = asmGen(copyTac, symbolTable)
      tacAsmMap(ifTac) = asmGen(ifTac, symbolTable)
    }

    val (rightTemp, rightMap) = genExpr(binOpExpr.rightExpr, tempGenie, symbolTable)
    
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

    val tac = new TacBinOp(tempGenie.generateTacNumber(), temp, leftTemp, op, rightTemp)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, rightMap)
    tacAsmMap(tac) = asmGen(tac, symbolTable)

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    tacAsmMap(endLabelTAC) = asmGen(endLabelTAC, symbolTable)

    return (temp, tacAsmMap)
  }

  def genIrLocation(
                     irLoc: IrLocation,
                     tempGenie: TempVariableGenie,
                     symbolTable: SymbolTable
                   ) : (String, LinkedHashMap[Tac, List[String]]) = {
    irLoc match {
      case sl: IrSingleLocation => {
        return genIrSingleLocation(sl, tempGenie, symbolTable)
      }
      case al: IrArrayLocation => {
        return genIrArrayLocation(al, tempGenie, symbolTable)
      }
    }
  }

  def genIrSingleLocation(
                           singleLoc: IrSingleLocation,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, LinkedHashMap[Tac, List[String]]) = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
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
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap) 
  }

  def genIrArrayLocation(
                          arrayLoc: IrArrayLocation,
                          tempGenie: TempVariableGenie,
                          symbolTable: SymbolTable
                        ) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    val (index, indexMap) = genExpr(arrayLoc.index, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, indexMap)

    tacAsmMap = combineLinkedHashMaps(tacAsmMap, checkArrayBounds(arrayLoc.name, index, tempGenie, symbolTable))
    val tac = new TacArrayRight(tempGenie.generateTacNumber(), temp, arrayLoc.name, index)
    
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap) 
  }

  def genIrMethodCallExpr(
                           methodExpr: IrMethodCallExpr,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, LinkedHashMap[Tac, List[String]]) =  {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)

    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]
    for (arg <- methodExpr.args) { 
      val argTemp: String = tempGenie.generateName()
      symbolTable.insert(argTemp, new IntTypeDescriptor)
      arg match {
        case IrCallExprArg(argExpr, _) => { 
          val (argTemp, argMap) = genExpr(argExpr, tempGenie, symbolTable)
          tacAsmMap = combineLinkedHashMaps(tacAsmMap, argMap)
          tempArgs += argTemp
        }
        case IrCallStringArg(strLit, _) => {
          val strLitLabel = tempGenie.generateLabel()
          val strLitTac = new TacStringLiteral(tempGenie.generateTacNumber(), strLitLabel, strLit.value)
          tacAsmMap(strLitTac) = asmGen(strLitTac, symbolTable)
          tempArgs += strLitLabel
        }
      }
    }
    
    val tac = new TacMethodCallExpr(tempGenie.generateTacNumber(), temp, methodExpr.name, tempArgs.toList)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  def genIrIntLiteral(
                       intLit: IrIntLiteral,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : (String, LinkedHashMap[Tac, List[String]]) = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    if(!intLit.value.isDefined) {
      throw new CompilerProblem("Trying to assemble int literal copy, int literal has no value tho!", intLit.loc)
    }
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, intLit.value.get.toLong)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteral(
                        charLit: IrCharLiteral,
                        tempGenie: TempVariableGenie,
                        symbolTable: SymbolTable
                      ) : (String, LinkedHashMap[Tac, List[String]]) = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, charLit.value.toLong)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  def genIrBooleanLiteral(
                           boolLit: IrBooleanLiteral,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : (String, LinkedHashMap[Tac, List[String]]) = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    symbolTable.insert(temp, new IntTypeDescriptor)
    val tac = new TacCopyBoolean(tempGenie.generateTacNumber(), temp, boolLit.value)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }
  
  // == Block macro ==

  def initializeLocalField(
                            name : String,
                            desc : BaseDescriptor,
                            tempGenie: TempVariableGenie,
                            symbolTable: SymbolTable
                          ): LinkedHashMap[Tac, List[String]] = {

    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    desc match {
      case itd: IntTypeDescriptor => {
        val copyIntTac = new TacCopyInt(tempGenie.generateTacNumber(), name, 0)
        tacAsmMap(copyIntTac) = asmGen(copyIntTac, symbolTable)
      }
      case btd: BoolTypeDescriptor => {
        val copyBoolTac = new TacCopyBoolean(tempGenie.generateTacNumber(), name, false)
        tacAsmMap(copyBoolTac) = asmGen(copyBoolTac, symbolTable)
      }
      case iad: IntArrayTypeDescriptor => {
        val indexTemp = tempGenie.generateName()
        symbolTable.insert(indexTemp, new IntTypeDescriptor)

        val constantZeroTemp = tempGenie.generateName()
        symbolTable.insert(constantZeroTemp, new IntTypeDescriptor)
        val copyConstantZero = new TacCopyInt(tempGenie.generateTacNumber(), constantZeroTemp, 0)
        tacAsmMap(copyConstantZero) = asmGen(copyConstantZero, symbolTable)

        for(i <- 0 until iad.size.toInt) {
          val copyIndex = new TacCopyInt(tempGenie.generateTacNumber(), indexTemp, i)
          tacAsmMap(copyIndex) = asmGen(copyIndex, symbolTable)

          val arrayAssign = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, constantZeroTemp)
          tacAsmMap(arrayAssign) = asmGen(arrayAssign, symbolTable)
        }
      }
      case bad: BoolArrayTypeDescriptor => {
        val indexTemp = tempGenie.generateName()
        symbolTable.insert(indexTemp, new IntTypeDescriptor)

        val constantFalseTemp = tempGenie.generateName()
        symbolTable.insert(constantFalseTemp, new BoolTypeDescriptor)
        val copyConstantFalse = new TacCopyBoolean(tempGenie.generateTacNumber(), constantFalseTemp, false)
        tacAsmMap(copyConstantFalse) = asmGen(copyConstantFalse, symbolTable)

        for(i <- 0 until bad.size.toInt) {
          val copyIndex = new TacCopyInt(tempGenie.generateTacNumber(), indexTemp, i)
          tacAsmMap(copyIndex) = asmGen(copyIndex, symbolTable)

          val arrayAssign = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, constantFalseTemp)
          tacAsmMap(arrayAssign) = asmGen(arrayAssign, symbolTable)
        }
      }
    }
    return tacAsmMap
  }
  
  def genBlock(
                block: IrBlock,
                parentStart: String,
                parentEnd: String,
                tempGenie: TempVariableGenie,
                symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    for((name, desc) <- symbolTable.symbolTableMap) {
      if(name.size < 2 || name.substring(0,2) != ".T") {
        tacAsmMap = combineLinkedHashMaps(tacAsmMap, initializeLocalField(name, desc, tempGenie, symbolTable))
      }
    }

    val childrenTables = symbolTable.getChildrenSymbolTables
    val expectedNumSubBlocks = childrenTables.size
    var subblockCount = 0

    for (stmt <- block.stmts) {
      if(stmt.isInstanceOf[IrIfStmt] && stmt.asInstanceOf[IrIfStmt].elseBlock.isDefined) {
        tacAsmMap = combineLinkedHashMaps(tacAsmMap, genStmt(stmt, parentStart, parentEnd, tempGenie, childrenTables(subblockCount), childrenTables(subblockCount+1)))
        subblockCount += 2
      } else if(stmt.isInstanceOf[IrForStmt] || stmt.isInstanceOf[IrWhileStmt] || stmt.isInstanceOf[IrIfStmt]) {
        tacAsmMap = combineLinkedHashMaps(tacAsmMap, genStmt(stmt, parentStart, parentEnd, tempGenie, childrenTables(subblockCount)))
        subblockCount += 1
      } else {
        tacAsmMap = combineLinkedHashMaps(tacAsmMap, genStmt(stmt, parentStart, parentEnd, tempGenie, symbolTable))
      }
    }

    if(expectedNumSubBlocks != subblockCount) {
      throw new ExpectedSubBlockCountNotActualException("Expected " + expectedNumSubBlocks.toString  + " but got " + subblockCount.toString)
    }

    return tacAsmMap
  } 

  //  == Statement Generating TAC ==

  def genStmt(
               stmt: IrStatement,
               parentStart: String,
               parentEnd: String,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable,
               symbolTable2: SymbolTable = null
             ) : LinkedHashMap[Tac, List[String]] = {
    stmt match {
      case s: IrAssignStmt => {
        return genIrAssignStmt(s, tempGenie, symbolTable)
      }
      case s: IrMethodCallStmt => {
        return genIrMethodCallStmt(s, tempGenie, symbolTable)
      }
      case s: IrIfStmt => {
        if(s.elseBlock.isDefined) {
          return genIrIfStmt(s, parentStart, parentEnd, tempGenie, symbolTable, symbolTable2)
        } else {
          return genIrIfStmt(s, parentStart, parentEnd, tempGenie, symbolTable)
        }
      }
      case s: IrForStmt => {
        return genIrForStmt(s, tempGenie, symbolTable)
      }
      case s: IrWhileStmt => {
        return genIrWhileStmt(s, tempGenie, symbolTable)
      }
      case s: IrReturnStmt => {
        return genIrReturnStmt(s, tempGenie, symbolTable)
      }
      case s: IrBreakStmt => {
        return genIrBreakStmt(s, parentEnd, tempGenie, symbolTable)
      }
      case s: IrContinueStmt => {
        return genIrContinueStmt(s, parentStart, tempGenie, symbolTable) 
      }
      case _ => {
        throw new NoMatchingStatementException("No matching statement", stmt.nodeLoc)
      }
    }
  }

  def checkArrayBounds(
                       arrayName : String,
                       indexTemp: String,
                       tempGenie : TempVariableGenie,
                       symbolTable : SymbolTable
                     ) : LinkedHashMap[Tac, List[String]] = {

    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    // Get the size of the array
    val sizeOfArray: String = tempGenie.generateName()
    symbolTable.insert(sizeOfArray, new IntTypeDescriptor())
    val tacSizeOfArray = new TacUnOp(tempGenie.generateTacNumber(), sizeOfArray, SIZE, arrayName)
    tacAsmMap(tacSizeOfArray) = asmGen(tacSizeOfArray, symbolTable)

    // Check whether the array index is too big
    val sizeCheckBool: String = tempGenie.generateName()
    symbolTable.insert(sizeCheckBool, new BoolTypeDescriptor())
    val sizeCondCmpTAC = new TacBinOp(tempGenie.generateTacNumber(), sizeCheckBool, indexTemp, LT, sizeOfArray)
    tacAsmMap(sizeCondCmpTAC) = asmGen(sizeCondCmpTAC, symbolTable)

    // Initialize constant zero for lower bound check
    val constantZeroTemp = tempGenie.generateName()
    symbolTable.insert(constantZeroTemp, new IntTypeDescriptor)
    val copyConstantZero = new TacCopyInt(tempGenie.generateTacNumber(), constantZeroTemp, 0)
    tacAsmMap(copyConstantZero) = asmGen(copyConstantZero, symbolTable)

    // Check whether the array index is too small
    val lowerSizeCheckBool: String = tempGenie.generateName()
    symbolTable.insert(lowerSizeCheckBool, new BoolTypeDescriptor())
    val lowerSizeCondTAC = new TacBinOp(tempGenie.generateTacNumber(), lowerSizeCheckBool, indexTemp, LT, constantZeroTemp)
    tacAsmMap(lowerSizeCondTAC) = asmGen(lowerSizeCondTAC, symbolTable)

    val accessIndexLabel: String = tempGenie.generateLabel()
    val errorOutLabel: String = tempGenie.generateLabel()

    // If index is not > 0, jump to the system exit (-1)
    val lowerboundsTAC = new TacIf(tempGenie.generateTacNumber(), lowerSizeCheckBool, errorOutLabel)
    tacAsmMap(lowerboundsTAC) = asmGen(lowerboundsTAC, symbolTable)

    // If index is within bounds, jump over the system exit (-1)
    val sizeIfTAC = new TacIf(tempGenie.generateTacNumber(), sizeCheckBool, accessIndexLabel)
    tacAsmMap(sizeIfTAC) = asmGen(sizeIfTAC, symbolTable)

    // Label right before error out
    val errorOutLabelTAC = new TacLabel(tempGenie.generateTacNumber(), errorOutLabel)
    tacAsmMap(errorOutLabelTAC) = asmGen(errorOutLabelTAC, symbolTable)

    val sysExitOne = new TacSystemExit(tempGenie.generateTacNumber(), -1)
    tacAsmMap(sysExitOne) = asmGen(sysExitOne, symbolTable)

    // Label for everything is all good
    val accessIndexLabelTAC = new TacLabel(tempGenie.generateTacNumber(), accessIndexLabel)
    tacAsmMap(accessIndexLabelTAC) = asmGen(accessIndexLabelTAC, symbolTable)

    return tacAsmMap
  }

  def genIrAssignStmt(
                       stmt: IrAssignStmt,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : LinkedHashMap[Tac, List[String]] = {

    val (exprTemp, exprMap) = genExpr(stmt.expr, tempGenie, symbolTable)
    var tacAsmMap = exprMap
    stmt match {
      case IrEqualsAssignStmt(irLoc, expr, _) => {
        irLoc match { 
          case IrSingleLocation(name, _) => { 
            val singleLocTac = new TacCopy(tempGenie.generateTacNumber(), name, exprTemp)
            tacAsmMap(singleLocTac) = asmGen(singleLocTac, symbolTable)
            return tacAsmMap
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexMap) = genExpr(index, tempGenie, symbolTable)
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, indexMap)

            // Copy RHS into array location
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, checkArrayBounds(name, indexTemp, tempGenie, symbolTable))
            val arrayLocTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, exprTemp)
            tacAsmMap(arrayLocTac) = asmGen(arrayLocTac, symbolTable)

            return tacAsmMap
          }
        }
      }

      case IrMinusAssignStmt(irLoc, expr, _) =>  {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val minusTac = new TacBinOp(tempGenie.generateTacNumber(), name, name, SUB, exprTemp)
            tacAsmMap(minusTac) = asmGen(minusTac, symbolTable)
            return tacAsmMap
          }

          case IrArrayLocation(name, index, _) => {
            // Evaluate expression in index
            val (indexTemp, indexMap) = genExpr(index, tempGenie, symbolTable)
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, indexMap)

            val temp: String = tempGenie.generateName()
            symbolTable.insert(temp, new IntTypeDescriptor())

            val arrayRightTac = new TacArrayRight(tempGenie.generateTacNumber(), temp, name, indexTemp)
            val arrayOpTac = new TacBinOp(tempGenie.generateTacNumber(), temp, temp, SUB, exprTemp)
            val arrayLeftTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, temp)
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, checkArrayBounds(name, indexTemp, tempGenie, symbolTable))
            tacAsmMap(arrayRightTac) = asmGen(arrayRightTac, symbolTable)
            tacAsmMap(arrayOpTac) = asmGen(arrayOpTac, symbolTable)
            tacAsmMap(arrayLeftTac) = asmGen(arrayLeftTac, symbolTable)

            return tacAsmMap
          }
        }
      }

      case IrPlusAssignStmt(irLoc, expr, _) => {
        irLoc match {
          case IrSingleLocation(name, _) => {
            val plusTac = new TacBinOp(tempGenie.generateTacNumber(), name, name, ADD, exprTemp)
            tacAsmMap(plusTac) = asmGen(plusTac, symbolTable)
            return tacAsmMap
          }

          case IrArrayLocation(name, index, _) => {
            val (indexTemp, indexMap) = genExpr(index, tempGenie, symbolTable)
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, indexMap)

            val temp: String = tempGenie.generateName()
            symbolTable.insert(temp, new IntTypeDescriptor())

            val arrayRightTac = new TacArrayRight(tempGenie.generateTacNumber(), temp, name, indexTemp)
            val arrayOpTac = new TacBinOp(tempGenie.generateTacNumber(), temp, name, ADD, exprTemp)
            val arrayLeftTac = new TacArrayLeft(tempGenie.generateTacNumber(), name, indexTemp, temp)
            tacAsmMap = combineLinkedHashMaps(tacAsmMap, checkArrayBounds(name, indexTemp, tempGenie, symbolTable))
            tacAsmMap(arrayRightTac) = asmGen(arrayRightTac, symbolTable)
            tacAsmMap(arrayOpTac) = asmGen(arrayOpTac, symbolTable)
            tacAsmMap(arrayLeftTac) = asmGen(arrayLeftTac, symbolTable)

            return tacAsmMap
          }
        }
      }
    }

    throw new NoMatchingStatementException("in genIrAssignStmt()", stmt.expr.nodeLoc)
  }

  def genIrMethodCallStmt(
                           stmt: IrMethodCallStmt,
                           tempGenie: TempVariableGenie,
                           symbolTable: SymbolTable
                         ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val callExpr: IrCallExpr = stmt.methCall
    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]

    callExpr match {
      case IrMethodCallExpr(name, args, _) => {
        for (arg <- args) {
          arg match {
            case IrCallExprArg(argExpr, _) => {
              val (argTemp, argTac) = genExpr(argExpr, tempGenie, symbolTable)
              tacAsmMap = combineLinkedHashMaps(tacAsmMap, argTac)
              tempArgs += argTemp
            }

            case IrCallStringArg(strLit, _) => {
              val strLitLabel = tempGenie.generateLabel()
              val strTac = new TacStringLiteral(tempGenie.generateTacNumber(), strLitLabel, strLit.value)
              tacAsmMap(strTac) = asmGen(strTac, symbolTable)
              tempArgs += strLitLabel
            }
          }
        } 
      val tac = new TacMethodCallStmt(tempGenie.generateTacNumber(), name, tempArgs.toList)
      tacAsmMap(tac) = asmGen(tac, symbolTable)
      }
    }
    return tacAsmMap
  }
  
  def genIrIfStmt(
                   stmt: IrIfStmt,
                   parentStart: String,
                   parentEnd: String,
                   tempGenie: TempVariableGenie,
                   symbolTable: SymbolTable,
                   symbolTable2: SymbolTable = null
                 ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val (condTemp, condTac) = genExpr(stmt.cond, tempGenie, symbolTable)
    val endLabel: String = tempGenie.generateLabel()
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, condTac)

    if (stmt.elseBlock.isDefined) {
      // Generate jmp-if-false and if-block
      val elseLabel: String = tempGenie.generateLabel()
      val ifFalseTac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, elseLabel) // jump to the else block
      tacAsmMap(ifFalseTac) = asmGen(ifFalseTac, symbolTable)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.ifBlock, parentStart, parentEnd, tempGenie, symbolTable))

      val gotoEndTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
      tacAsmMap(gotoEndTac) = asmGen(gotoEndTac, symbolTable)
      val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
      tacAsmMap(elseLabelTac) = asmGen(elseLabelTac, symbolTable)

      tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.elseBlock.get, parentStart, parentEnd, tempGenie, symbolTable2))

      val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)
      tacAsmMap(endLabelTac) = asmGen(endLabelTac, symbolTable)

    } else {
      val ifFalseTac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel) // jump to the end of the if
      tacAsmMap(ifFalseTac) = asmGen(ifFalseTac, symbolTable)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.ifBlock, parentStart, parentEnd, tempGenie, symbolTable))
      val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)
      tacAsmMap(endLabelTac) = asmGen(endLabelTac, symbolTable)
    }
  
    return tacAsmMap
  }

  def genIrForStmt(
                    stmt: IrForStmt,
                    tempGenie: TempVariableGenie,
                    symbolTable: SymbolTable
                  ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()

    val (initValTemp, initValTac) = genExpr(stmt.initVal, tempGenie, symbolTable)
    val (endValTemp, endValTac) = genExpr(stmt.endVal, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, initValTac)
    val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
    tacAsmMap(copyIntoVar) = asmGen(copyIntoVar, symbolTable)

    tacAsmMap = combineLinkedHashMaps(tacAsmMap, endValTac)

    val forBeginTAC = new TacLabel(tempGenie.generateTacNumber(), startLabel) // beginning of the for loop
    tacAsmMap(forBeginTAC) = asmGen(forBeginTAC, symbolTable)

    // lessThan is boolean representing if index < endVal
    val lessThan: String = tempGenie.generateName()
    symbolTable.insert(lessThan, new BoolTypeDescriptor())
    val forCondCmpTAC = new TacBinOp(tempGenie.generateTacNumber(), lessThan, initValTemp, LT, endValTemp)
    tacAsmMap(forCondCmpTAC) = asmGen(forCondCmpTAC, symbolTable)

    val forIfFalseTAC = new TacIfFalse(tempGenie.generateTacNumber(), lessThan, endLabel) //if index >= endVal, exit for loop
    tacAsmMap(forIfFalseTAC) = asmGen(forIfFalseTAC, symbolTable)

    tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie, symbolTable))

    if (stmt.inc.isDefined) { 
      val (incTemp, incTac) = genExpr(stmt.inc.get, tempGenie, symbolTable)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, incTac)

      val forIncTAC = new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by inc
      tacAsmMap(forIncTAC) = asmGen(forIncTAC, symbolTable)

      val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
      tacAsmMap(copyIntoVar) = asmGen(copyIntoVar, symbolTable)

    } else {
      val incTemp : String = tempGenie.generateName()
      symbolTable.insert(incTemp, new IntTypeDescriptor())
      val loadIncTAC = new TacCopyInt(tempGenie.generateTacNumber(), incTemp, 1)
      tacAsmMap(loadIncTAC) = asmGen(loadIncTAC, symbolTable)
      val incOpTAC = new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by 1
      tacAsmMap(incOpTAC) = asmGen(incOpTAC, symbolTable)

      val copyIntoVar = new TacCopy(tempGenie.generateTacNumber(), stmt.irLoc.name, initValTemp)
      tacAsmMap(copyIntoVar) = asmGen(copyIntoVar, symbolTable)
    }
    val loopTAC = new TacGoto(tempGenie.generateTacNumber(), startLabel) // continue looping
    tacAsmMap(loopTAC) = asmGen(loopTAC, symbolTable)

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    tacAsmMap(endLabelTAC) = asmGen(endLabelTAC, symbolTable)
    
    return tacAsmMap
  }

  def genIrWhileStmt(
                      stmt: IrWhileStmt,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()

    val startLabelTAC = new TacLabel(tempGenie.generateTacNumber(), startLabel)
    tacAsmMap(startLabelTAC) = asmGen(startLabelTAC, symbolTable)

    val (condTemp, condTac) = genExpr(stmt.boolExpr, tempGenie, symbolTable)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, condTac)

    val jmpIfFalseTAC = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel)
    tacAsmMap(jmpIfFalseTAC) = asmGen(jmpIfFalseTAC, symbolTable)

    tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie, symbolTable))

    val loopBackTAC = new TacGoto(tempGenie.generateTacNumber(), startLabel)
    tacAsmMap(loopBackTAC) = asmGen(loopBackTAC, symbolTable)

    val endLabelTAC = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    tacAsmMap(endLabelTAC) = asmGen(endLabelTAC, symbolTable)
    
    return tacAsmMap
  }

  def genIrReturnStmt(
                       stmt: IrReturnStmt,
                       tempGenie: TempVariableGenie,
                       symbolTable: SymbolTable
                     ) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    if (stmt.value.isDefined) {
      val (retTemp, retTac) = genExpr(stmt.value.get, tempGenie, symbolTable)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, retTac)
      val returnValueTac = new TacReturnValue(tempGenie.generateTacNumber(), retTemp)
      tacAsmMap(returnValueTac) = asmGen(returnValueTac, symbolTable)
    }

    val returnTac = new TacReturn(tempGenie.generateTacNumber())
    tacAsmMap(returnTac) = asmGen(returnTac, symbolTable)
    return tacAsmMap
  }

  def genIrBreakStmt(
                      stmt: IrBreakStmt,
                      parentEnd: String,
                      tempGenie: TempVariableGenie,
                      symbolTable: SymbolTable
                    ) : LinkedHashMap[Tac, List[String]] = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentEnd)
    tacAsmMap(gotoTac) = asmGen(gotoTac, symbolTable)
    return tacAsmMap
  }

  def genIrContinueStmt(
                         stmt: IrContinueStmt,
                         parentStart: String,
                         tempGenie: TempVariableGenie,
                         symbolTable: SymbolTable
                       ): LinkedHashMap[Tac, List[String]] = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentStart)
    tacAsmMap(gotoTac) = asmGen(gotoTac, symbolTable)
    return tacAsmMap
  }
}
