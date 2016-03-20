package compile.tac

import compile.Ir._
import compile.exceptionhandling._
import compile.symboltables.{ParametersTable, MethodsTable, SymbolTable}
import compile.descriptors.{IntTypeDescriptor, MethodDescriptor}
import compile.tac.OpTypes._
import compile.tac.AsmGen._
import compile.tac.ThreeAddressCode._
import compile.util.Util.combineLinkedHashMaps
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}

object TACGen {

  def gen(
           program: IrProgram,
           tempGenie: TempVariableGenie,
           methodsTable : MethodsTable
         ) : LinkedHashMap[Tac, List[String]] = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val asm: List[String] = List.empty[String]

    val firstTac = new TacProgramEnter(tempGenie.generateTacNumber())
    tacAsmMap(firstTac) = asmGen(firstTac, methodsTable.getGlobalFieldTable)

    for (method <- program.methodDecls) {
      val methodDesc = methodsTable.lookupID(method.name)
      val methodAsm = genMethodDecl(method, tempGenie, methodDesc)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, methodAsm)
    }

    return tacAsmMap
  }

  def genMethodDecl(
                     methodDecl: IrMethodDecl,
                     tempGenie: TempVariableGenie,
                     methodDesc : MethodDescriptor
                   ) : LinkedHashMap[Tac, List[String]] = {

    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val methodParamTable = methodDesc.getParamTable

    // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
    if (methodDecl.name == "main") {
      val mainGloblTac = new TacGlobl(tempGenie.generateTacNumber(), methodDecl.name)
      tacAsmMap(mainGloblTac) = asmGen(mainGloblTac, methodParamTable)
    }
    
    val methodLabelTac = new TacLabel(tempGenie.generateTacNumber(), methodDecl.name)
    tacAsmMap(methodLabelTac) = asmGen(methodLabelTac, methodParamTable)

    val methodEnterTac = new TacMethodEnter(tempGenie.generateTacNumber(), methodDesc)
    tacAsmMap(methodEnterTac) = asmGen(methodEnterTac, methodParamTable)

    val blockLHM = genBlock(methodDecl.bodyBlock, null, null, tempGenie, methodParamTable)
    return combineLinkedHashMaps(tacAsmMap, blockLHM)
  }

  // == Expr gening ==

  //returns (temp_var, code) where temp_var is where the expression is allocated, and code is the list of TACs.
  def genExpr(expr: IrExpression, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
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
    }
    null
  }

  def genIrTernOpExpr(ternOpExpr: IrTernOpExpr, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val elseLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    
    val (condTemp, condMap) = genExpr(ternOpExpr.cond, tempGenie, symbolTable)
    val (leftTemp, leftMap) = genExpr(ternOpExpr.leftExpr, tempGenie, symbolTable)
    val (rightTemp, rightMap) = genExpr(ternOpExpr.rightExpr, tempGenie, symbolTable)
    
    val tac = new TacIfFalse(tempGenie.generateTacNumber(), condTemp, elseLabel)
    val endLabelTac = new TacLabel(tempGenie.generateTacNumber(), endLabel)
    val endLabelGotoTac = new TacGoto(tempGenie.generateTacNumber(), endLabel)
    val elseLabelTac = new TacLabel(tempGenie.generateTacNumber(), elseLabel)
    
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, condMap) // generate code for evaluating the conditional expr
    tacAsmMap(tac) = asmGen(tac, symbolTable) // ifFalse condExpr, goto elseLabel
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, leftMap) // if block code
    tacAsmMap(endLabelGotoTac) = asmGen(endLabelGotoTac, symbolTable) // done evaluating; goto endLabel
    tacAsmMap(elseLabelTac) = asmGen(elseLabelTac, symbolTable) // else label
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, rightMap) // else block code
    tacAsmMap(endLabelTac) = asmGen(endLabelTac, symbolTable) // end label
    
    return (temp, tacAsmMap)
  }

  def genIrUnOpExpr(unOpExpr: IrUnOpExpr, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val (exprTemp, exprMap) = genExpr(unOpExpr.expr, tempGenie, symbolTable)
    var op: UnOpEnumVal = null
    unOpExpr.unop match {
      case minus: IrMinusOp => {
        op = MINUS
      }
      case not: IrNotOp => {
        op = NOT
      }
      case size: IrArraySizeOp => {
        op = SIZE
      }
    }

    val tac = new TacUnOp(tempGenie.generateTacNumber(), temp, op, exprTemp)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, exprMap) // generates code for the expr 
    tacAsmMap(tac) = asmGen(tac, symbolTable) // generates code for applying the unary op
    return (temp, tacAsmMap)
  }

  def genIrBinOpExpr(binOpExpr: IrBinOpExpr, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()

    val (leftTemp, leftMap) = genExpr(binOpExpr.leftExpr, tempGenie, symbolTable)
    val (rightTemp, rightMap) = genExpr(binOpExpr.rightExpr, tempGenie, symbolTable)
    
    var op : BinOpEnumVal = null
    binOpExpr.binOp match {
      case IrMulOp() => op = MULT
      case IrDivOp() => op = DIV
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
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, leftMap)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, rightMap)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  def genIrLocation(irLoc: IrLocation, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    irLoc match {
      case sl: IrSingleLocation => {
        return genIrSingleLocation(sl, tempGenie, symbolTable)
      }
      case al: IrArrayLocation => {
        return genIrArrayLocation(al, tempGenie, symbolTable)
      }
    }
  }

  def genIrSingleLocation(singleLoc: IrSingleLocation, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val tac = new TacCopy(tempGenie.generateTacNumber(), temp, singleLoc.name) //TODO This is a problem, temp should address if singleLoc.name is array
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap) 
  }

  def genIrArrayLocation(arrayLoc: IrArrayLocation, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val (index, indexMap) = genExpr(arrayLoc.index, tempGenie, symbolTable)
    val tac = new TacArrayRight(tempGenie.generateTacNumber(), temp, arrayLoc.name, index)
    
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, indexMap)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap) 
  }

  def genIrMethodCallExpr(methodExpr: IrMethodCallExpr, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) =  {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    
    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]
    for (arg <- methodExpr.args) { 
      val argTemp: String = tempGenie.generateName()
      arg match {
        case IrCallExprArg(argExpr, _) => { 
          val (argTemp, argMap) = genExpr(argExpr, tempGenie, symbolTable)
          tacAsmMap = combineLinkedHashMaps(tacAsmMap, argMap)
          tempArgs += argTemp
        }
        case IrCallStringArg(strLit, _) => {
          val strLitLabel = tempGenie.generateLabel()
          // TODO these need to be rearranged in the TACs to come before
          // A method declaration
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

  def genIrIntLiteral(intLit: IrIntLiteral, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, intLit.value.get.toInt) //TODO: check that this is actually safe, since it could be BigInt, etc.
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteral(charLit: IrCharLiteral, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val tac = new TacCopyInt(tempGenie.generateTacNumber(), temp, charLit.value.toInt)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }

  def genIrBooleanLiteral(boolLit: IrBooleanLiteral, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : (String, LinkedHashMap[Tac, List[String]]) = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val temp: String = tempGenie.generateName()
    val tac = new TacCopyBoolean(tempGenie.generateTacNumber(), temp, boolLit.value)
    tacAsmMap(tac) = asmGen(tac, symbolTable)
    return (temp, tacAsmMap)
  }
  
  // == Block macro == 
  
  def genBlock(
                block: IrBlock,
                parentStart: String,
                parentEnd: String,
                tempGenie: TempVariableGenie,
                symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {

    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]

    for (stmt <- block.stmts) {
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, genStmt(stmt, parentStart, parentEnd, tempGenie, symbolTable))
    }

    return tacAsmMap
  } 

  //  == Statement Generating TAC ==

  def genStmt(
               stmt: IrStatement,
               parentStart: String,
               parentEnd: String,
               tempGenie: TempVariableGenie,
               symbolTable: SymbolTable
             ) : LinkedHashMap[Tac, List[String]] = {
    stmt match {
      case s: IrAssignStmt => {
        return genIrAssignStmt(s, tempGenie, symbolTable)
      }
      case s: IrMethodCallStmt => {
        return genIrMethodCallStmt(s, tempGenie, symbolTable)
      }
      case s: IrIfStmt => {
        return genIrIfStmt(s, parentStart, parentEnd, tempGenie, symbolTable)
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
            tacAsmMap(arrayRightTac) = asmGen(arrayRightTac, symbolTable)
            tacAsmMap(arrayOpTac) = asmGen(arrayRightTac, symbolTable)
            tacAsmMap(arrayLeftTac) = asmGen(arrayRightTac, symbolTable)

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
                   symbolTable: SymbolTable
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

      tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.elseBlock.get, parentStart, parentEnd, tempGenie, symbolTable))

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

  def genIrForStmt(stmt: IrForStmt, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()
    val (initValTemp, initValTac) = genExpr(stmt.initVal, tempGenie, symbolTable)
    val (endValTemp, endValTac) = genExpr(stmt.endVal, tempGenie, symbolTable)
    val lessThan: String = tempGenie.generateName() 

    tacAsmMap = combineLinkedHashMaps(tacAsmMap, initValTac)
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, endValTac)
    buf += new TacBinOp(tempGenie.generateTacNumber(), lessThan, initValTemp, LT, endValTemp) // lessThan is boolean representing if index < endVal 
    buf += new TacLabel(tempGenie.generateTacNumber(), startLabel) // beginning of the for loop
    buf += new TacIfFalse(tempGenie.generateTacNumber(), lessThan, endLabel) //if index >= endVal, exit for loop 

    if (stmt.inc.isDefined) { 
      val (incTemp, incTac) = genExpr(stmt.inc.get, tempGenie, symbolTable)
      tacAsmMap = combineLinkedHashMaps(tacAsmMap, incTac)
      buf += new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by inc
    } else {
      val incTemp : String = tempGenie.generateName()
      buf += new TacCopyInt(tempGenie.generateTacNumber(), incTemp, 1)
      buf += new TacBinOp(tempGenie.generateTacNumber(), initValTemp, initValTemp, ADD, incTemp) // increment index by 1
    }
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie, symbolTable))
    buf += new TacGoto(tempGenie.generateTacNumber(), startLabel) // continue looping
    buf += new TacLabel(tempGenie.generateTacNumber(), endLabel)
    
    return tacAsmMap
  }

  def genIrWhileStmt(stmt: IrWhileStmt, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel() 
    val (condTemp, condTac) = genExpr(stmt.boolExpr, tempGenie, symbolTable)
     
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, condTac)
    buf += new TacLabel(tempGenie.generateTacNumber(), startLabel) 
    buf += new TacIfFalse(tempGenie.generateTacNumber(), condTemp, endLabel) 
    tacAsmMap = combineLinkedHashMaps(tacAsmMap, genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie, symbolTable)) 
    buf += new TacGoto(tempGenie.generateTacNumber(), startLabel)
    buf += new TacLabel(tempGenie.generateTacNumber(), endLabel)
    
    return tacAsmMap
  }

  def genIrReturnStmt(stmt: IrReturnStmt, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {
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

  def genIrBreakStmt(stmt: IrBreakStmt, parentEnd: String, tempGenie: TempVariableGenie, symbolTable: SymbolTable) : LinkedHashMap[Tac, List[String]] = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentEnd)
    tacAsmMap(gotoTac) = asmGen(gotoTac, symbolTable)
    return tacAsmMap
  }

  def genIrContinueStmt(stmt: IrContinueStmt, parentStart: String, tempGenie: TempVariableGenie, symbolTable: SymbolTable): LinkedHashMap[Tac, List[String]] = {
    var tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    val gotoTac = new TacGoto(tempGenie.generateTacNumber(), parentStart)
    tacAsmMap(gotoTac) = asmGen(gotoTac, symbolTable)
    return tacAsmMap
  }
}
