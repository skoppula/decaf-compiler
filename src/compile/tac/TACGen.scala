package compile.tac

import compile.Ir._
import compile.symboltables.{ParametersTable, MethodsTable}
import compile.descriptors.{MethodDescriptor}
import compile.tac.OpTypes._
import compile.tac.AsmGen._
import compile.tac.ThreeAddressCode._

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer, LinkedHashMap}

object TACGen {

  def combineLinkedHashMaps(lhm1 : LinkedHashMap[Tac, List[String]], lhm2 : LinkedHashMap[Tac, List[String]]) : LinkedHashMap[Tac, List[String]]= {
    val newLHM : LinkedHashMap[Tac, List[String]] = LinkedHashMap.empty[Tac, List[String]]
    for((tac, asm) <- lhm1) {
      newLHM(tac) = asm
    }

    for((tac, asm) <- lhm2) {
      newLHM(tac) = asm
    }

    return newLHM
  }

  def gen(program: IrProgram, tempGenie: TempVariableGenie, methodsTable : MethodsTable) : LinkedHashMap[Tac, List[String]] = {
    val tacAsmMap = LinkedHashMap.empty[Tac, List[String]]
    var tacs: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val asm: List[String] = List.empty[String]

    tacAsmMap(new TacProgramEnter()) = asmGen(tacs(0), methodsTable.getGlobalFieldTable)

    for (method <- program.methodDecls) {
      val methodName = method.name
      val methodParamTable = methodsTable.lookupID(methodName).getParamTable
      val methodDesc = methodsTable.lookupID(methodName)
      var (methodTacs, methodAsm) = genMethodDecl(method, tempGenie, methodParamTable, methodDesc)
      tacs ++= methodTacs
      asm :+ methodAsm
    }

    return tacAsmMap
  }

  def genMethodDecl(methodDecl: IrMethodDecl, tempGenie: TempVariableGenie, methodParamTable : ParametersTable, methodDesc : MethodDescriptor) : (ArrayBuffer[Tac], List[String]) = {
    var tacs: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val asm: List[String] = List.empty[String]

    // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
    if (methodDecl.name == "main") {
      tacs += new TacGlobl(methodDecl.name)
    }
    tacs += new TacLabel(methodDecl.name)
    // Austin:
    // TODO : You need to pass in the amount of space I need here to allocate on the stack
    // or give me access to the MethodDescriptor to call getTotalByteSize
    // For now I have opted to pass in the method descriptor but I don't know what is
    // best for the current framework
    tacs += new TacMethodEnter(methodDesc)

    asm :+ asmGen(tacs(0), methodParamTable)
    asm :+ asmGen(tacs(0), methodParamTable)

    // Why is this needed **
    // Austin: 
    // TODO : Yeah actually we should not generate a new temp
    // var for each arg, but instead the corresponding scope
    // table needs to map the param name to a stack offset
    // Commenting it out for now
    /*
    for (arg <- methodDecl.args) {
      tempGenie.generateName()    
    }
     */

    tacs ++= genBlock(methodDecl.bodyBlock, null, null, tempGenie)

    return (tacs, asm)
  }

  // == Expr gening ==

  //returns (temp_var, code) where temp_var is where the expression is allocated, and code is the list of TACs.
  def genExpr(expr: IrExpression, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    expr match {
      case singleLoc: IrSingleLocation => {
        return genIrSingleLocation(singleLoc, tempGenie)
      }
      case arrayLoc: IrArrayLocation => {
        return genIrArrayLocation(arrayLoc, tempGenie)
      }
      case methodCall: IrMethodCallExpr => {
        return genIrMethodCallExpr(methodCall, tempGenie)
      }
      case intLit: IrIntLiteral => {
        return genIrIntLiteral(intLit, tempGenie)
      }
      case charLit: IrCharLiteral => {
        return genIrCharLiteral(charLit, tempGenie)
      }
      case boolLit: IrBooleanLiteral => {
        return genIrBooleanLiteral(boolLit, tempGenie)
      }
      case binOpExpr: IrBinOpExpr => {
        return genIrBinOpExpr(binOpExpr, tempGenie)
      }
      case unOpExpr: IrUnOpExpr => {
        return genIrUnOpExpr(unOpExpr, tempGenie)
      }
      case ternOpExpr: IrTernOpExpr => {
        return genIrTernOpExpr(ternOpExpr, tempGenie)
      }
    }
    ("error", null)
  }

  def genIrTernOpExpr(ternOpExpr: IrTernOpExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    val elseLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    
    val (condTemp, condCode) = genExpr(ternOpExpr.cond, tempGenie)
    val (leftTemp, leftCode) = genExpr(ternOpExpr.leftExpr, tempGenie)
    val (rightTemp, rightCode) = genExpr(ternOpExpr.rightExpr, tempGenie)
    
    buf ++= condCode
    buf += new TacIfFalse(condTemp, elseLabel)
    buf ++= leftCode
    buf += new TacGoto(endLabel)
    buf += new TacLabel(elseLabel)
    buf ++= rightCode
    buf += new TacLabel(endLabel)
    
    return (temp, buf)
  }

  def genIrUnOpExpr(unOpExpr: IrUnOpExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val (exprTemp, exprCode) = genExpr(unOpExpr.expr, tempGenie)
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

    val tac = new TacUnOp(temp, op, exprTemp)
    buf ++= exprCode 
    buf += tac
    return (temp, buf)
  }

  def genIrBinOpExpr(binOpExpr: IrBinOpExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]

    val (leftTemp, leftCode) = genExpr(binOpExpr.leftExpr, tempGenie)
    val (rightTemp, rightCode) = genExpr(binOpExpr.rightExpr, tempGenie)
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
    val tac = new TacBinOp(temp, leftTemp, op, rightTemp)
    buf ++= leftCode
    buf ++= rightCode
    buf += tac
    return (temp, buf)
  }

  def genIrLocation(irLoc: IrLocation, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    irLoc match {
      case sl: IrSingleLocation => {
        return genIrSingleLocation(sl, tempGenie)
      }
      case al: IrArrayLocation => {
        return genIrArrayLocation(al, tempGenie)
      }
    }
  }

  def genIrSingleLocation(singleLoc: IrSingleLocation, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopy(temp, singleLoc.name) //This is problem, temp should adress if singleLoc.name is array
    buf += tac
    return (temp, buf)  
  }

  def genIrArrayLocation(arrayLoc: IrArrayLocation, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val (index, indexCode) = genExpr(arrayLoc.index, tempGenie)
    val tac = new TacArrayRight(temp, arrayLoc.name, index)
    buf ++= indexCode
    buf += tac
    return (temp, buf) 
  }

  def genIrMethodCallExpr(methodExpr: IrMethodCallExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) =  {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]
    for (arg <- methodExpr.args) { 
      val argTemp: String = tempGenie.generateName()
      arg match {
        case IrCallExprArg(argExpr, _) => { 
          val (argTemp, argTac) = genExpr(argExpr, tempGenie)
          buf ++= argTac
          tempArgs += argTemp
        }
        case IrCallStringArg(strLit, _) => {
          val strLitLabel = tempGenie.generateLabel()
          // TODO these need to be rearranged in the TACs to come before
          // A method declaration
          buf prepend new TacStringLiteral(strLitLabel, strLit.value)
          tempArgs += strLitLabel
        }
      }
    }
    
    val tac = new TacMethodCallExpr(temp, methodExpr.name, tempArgs.toList)
    buf += tac
    return (temp, buf)
  }

  def genIrIntLiteral(intLit: IrIntLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopyInt(temp, intLit.value.get.toInt) //TODO: check that this is actually safe, since it could be BigInt, etc.
    buf += tac
    return (temp, buf)
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteral(charLit: IrCharLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopyInt(temp, charLit.value.toInt)
    buf += tac
    return (temp, buf)
  }

  def genIrBooleanLiteral(boolLit: IrBooleanLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopyBoolean(temp, boolLit.value)
    buf += tac
    return (temp, buf)
  }
  
  // == Block macro == 
  
  def genBlock(block: IrBlock, parentStart: String, parentEnd: String, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    for (field <- block.fieldDecls) {
      tempGenie.generateName()
    }
    for (stmt <- block.stmts) {
      buf ++= genStmt(stmt, parentStart, parentEnd, tempGenie)
    }
    return buf
  } 

  //  == Statement Generating TAC ==

  def genStmt(stmt: IrStatement, parentStart: String, parentEnd: String, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {

    stmt match {
      case s: IrAssignStmt => {
        return genIrAssignStmt(s, tempGenie)
      }
      case s: IrMethodCallStmt => {
        return genIrMethodCallStmt(s, tempGenie)
      }
      case s: IrIfStmt => {
        return genIrIfStmt(s, parentStart, parentEnd, tempGenie)
      }
      case s: IrForStmt => {
        return genIrForStmt(s, tempGenie)
      }
      case s: IrWhileStmt => {
        return genIrWhileStmt(s, tempGenie)
      }
      case s: IrReturnStmt => {
        return genIrReturnStmt(s, tempGenie)
      }
      case s: IrBreakStmt => {
        return genIrBreakStmt(s, parentEnd)
      }
      case s: IrContinueStmt => {
        return genIrContinueStmt(s, parentStart) 
      }
    }
    return ArrayBuffer.empty[Tac]
  }

  def genIrAssignStmt(stmt: IrAssignStmt, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac] 
    val (exprTemp, exprTac) = genExpr(stmt.expr, tempGenie)
    buf ++= exprTac
    stmt match {
      case IrEqualsAssignStmt(irLoc, expr, _) => {
        irLoc match { 
          case IrSingleLocation(name, _) => { 
            buf += new TacCopy(name, exprTemp)
          }
          case IrArrayLocation(name, index, _) => { 
            val (indexTemp, indexTac) = genExpr(index, tempGenie)
            buf ++= indexTac
            buf += new TacArrayLeft(name, indexTemp, exprTemp)
          }
        }
      }

      case IrMinusAssignStmt(irLoc, expr, _) =>  {
        irLoc match {
          case IrSingleLocation(name, _) => {
            buf += new TacBinOp(name, name, SUB, exprTemp)
          }
          case IrArrayLocation(name, index, _) => {
            val temp: String = tempGenie.generateName()
            val (indexTemp, indexTac) = genExpr(index, tempGenie) 
            buf ++= indexTac
            buf += new TacArrayRight(temp, name, indexTemp)
            buf += new TacBinOp(temp, temp, SUB, exprTemp)
            buf += new TacArrayLeft(name, indexTemp, temp)
          }
        }
      }

      case IrPlusAssignStmt(irLoc, expr, _) => {
        irLoc match {
          case IrSingleLocation(name, _) => {
            buf += new TacBinOp(name, name, ADD, exprTemp)
          }
          case IrArrayLocation(name, index, _) => {
            val temp: String = tempGenie.generateName() 
            val (indexTemp, indexTac) = genExpr(index, tempGenie)
            buf ++= indexTac
            buf += new TacArrayRight(temp, name, indexTemp)
            buf += new TacBinOp(temp, name, ADD, exprTemp)
            buf += new TacArrayLeft(name, indexTemp, temp)
          }
        }
      }
      return buf
    }
  }

  def genIrMethodCallStmt(stmt: IrMethodCallStmt, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val callExpr: IrCallExpr = stmt.methCall
    var tempArgs: ListBuffer[String] = ListBuffer.empty[String]
    callExpr match {
      case IrMethodCallExpr(name, args, _) => {
        for (arg <- args) {
          arg match {
            case IrCallExprArg(argExpr, _) => {
              val (argTemp, argTac) = genExpr(argExpr, tempGenie)
              buf ++= argTac
              tempArgs += argTemp
            }
            case IrCallStringArg(strLit, _) => {  // should be unreachable...?
              val strLitLabel = tempGenie.generateLabel()
              buf prepend new TacStringLiteral(strLitLabel, strLit.value)
              tempArgs += strLitLabel
            }
          }
        } 
      val tac = new TacMethodCallStmt(name, tempArgs.toList)
      buf += tac 
      }
    }
    return buf
  }
  
  def genIrIfStmt(stmt: IrIfStmt, parentStart: String, parentEnd: String, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val (condTemp, condTac) = genExpr(stmt.cond, tempGenie)
    val endLabel: String = tempGenie.generateLabel()
    buf ++= condTac

    if (stmt.elseBlock.isDefined) { 
      val elseLabel: String = tempGenie.generateLabel()
      val tac = new TacIfFalse(condTemp, elseLabel) // jump to the else block
      buf += tac
      buf ++= genBlock(stmt.ifBlock, parentStart, parentEnd, tempGenie)
      buf += new TacGoto(endLabel)
      buf += new TacLabel(elseLabel)
      buf ++= genBlock(stmt.elseBlock.get, parentStart, parentEnd, tempGenie)
      buf += new TacLabel(endLabel)
    } else {
      val tac = new TacIfFalse(condTemp, endLabel) // jump to the end of the if
      buf += tac
      buf ++= genBlock(stmt.ifBlock, parentStart, parentEnd, tempGenie)
      buf += new TacLabel(endLabel)
    }
  
    return buf
  }

  def genIrForStmt(stmt: IrForStmt, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel()
    val (initValTemp, initValTac) = genExpr(stmt.initVal, tempGenie)
    val (endValTemp, endValTac) = genExpr(stmt.endVal, tempGenie)
    val lessThan: String = tempGenie.generateName() 

    buf ++= initValTac
    buf ++= endValTac
    buf += new TacBinOp(lessThan, initValTemp, LT, endValTemp) // lessThan is boolean representing if index < endVal 
    buf += new TacLabel(startLabel) // beginning of the for loop
    buf += new TacIfFalse(lessThan, endLabel) //if index >= endVal, exit for loop 

    if (stmt.inc.isDefined) { 
      val (incTemp, incTac) = genExpr(stmt.inc.get, tempGenie)
      buf ++= incTac
      buf += new TacBinOp(initValTemp, initValTemp, ADD, incTemp) // increment index by inc
    } else {
      val incTemp : String = tempGenie.generateName()
      buf += new TacCopyInt(incTemp, 1)
      buf += new TacBinOp(initValTemp, initValTemp, ADD, incTemp) // increment index by 1
    }
    buf ++= genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie)
    buf += new TacGoto(startLabel) // continue looping
    buf += new TacLabel(endLabel)
    
    return buf
  }

  def genIrWhileStmt(stmt: IrWhileStmt, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val startLabel: String = tempGenie.generateLabel()
    val endLabel: String = tempGenie.generateLabel() 
    val (condTemp, condTac) = genExpr(stmt.boolExpr, tempGenie)
     
    buf ++= condTac
    buf += new TacLabel(startLabel) 
    buf += new TacIfFalse(condTemp, endLabel) 
    buf ++= genBlock(stmt.bodyBlock, startLabel, endLabel, tempGenie) 
    buf += new TacGoto(startLabel)
    buf += new TacLabel(endLabel)
    
    return buf
  }

  def genIrReturnStmt(stmt: IrReturnStmt, tempGenie: TempVariableGenie) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    if (stmt.value.isDefined) {
      val (retTemp, retTac) = genExpr(stmt.value.get, tempGenie)
      buf ++= retTac
      buf += new TacReturnValue(retTemp)
    }    
    buf += new TacReturn()
    return buf
  }

  def genIrBreakStmt(stmt: IrBreakStmt, parentEnd: String) : ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    buf += new TacGoto(parentEnd)
    return buf
  }

  def genIrContinueStmt(stmt: IrContinueStmt, parentStart: String): ArrayBuffer[Tac] = {
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    buf += new TacGoto(parentStart)
    return buf
  }
}
