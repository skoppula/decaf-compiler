package compile

import compile.Ir._
import compile.Compiler._

import compile.tac.OpTypes._
import compile.tac._
import compile.tac.ThreeAddressCode._

import scala.collection.mutable.ArrayBuffer
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}
import compile.ScopeTypes._

object Gen {

  def gen(program: IrProgram, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    return ("", null)
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
    
    val tac = new TacIfFalse(condTemp, elseLabel)
    buf ++= condCode
    buf += tac
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
    var op: OpEnumVal = null
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

    val tac = new TacUnaryOp(temp, op, exprTemp)
    buf ++= exprCode 
    buf += tac
    return (temp, buf)
  }

  def genIrBinOpExpr(binOpExpr: IrBinOpExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]

    val (leftTemp, leftCode) = genExpr(binOpExpr.leftExpr, tempGenie)
    val (rightTemp, rightCode) = genExpr(binOpExpr.rightExpr, tempGenie)
    var op : OpEnumVal = null
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
    val tac = new TacCopy(temp, singleLoc.name)
    buf += tac
    return (temp, buf)  
  }

  def genIrArrayLocation(arrayLoc: IrArrayLocation, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val (index, indexCode) = genExpr(arrayLoc.index, tempGenie)
    val tac = new TacExprArray(temp, arrayLoc.name, index)
    buf ++= indexCode
    buf += tac
    return (temp, buf) 
  }

  def genIrMethodCallExpr(methodExpr: IrMethodCallExpr, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) =  {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacMethodCall(temp, methodExpr.name, methodExpr.args)
    buf += tac
    return (temp, buf)
  }

  def genIrIntLiteral(intLit: IrIntLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopy(temp, intLit.rep) //TODO: should this be the repr or value?
    buf += tac
    return (temp, buf)
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteral(charLit: IrCharLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopy(temp, charLit.value.toInt.toString) //hideous...
    buf += tac
    return (temp, buf)
  }

  def genIrBooleanLiteral(boolLit: IrBooleanLiteral, tempGenie: TempVariableGenie) : (String, ArrayBuffer[Tac]) = {
    val temp: String = tempGenie.generateName()
    var buf: ArrayBuffer[Tac] = ArrayBuffer.empty[Tac]
    val tac = new TacCopy(temp, boolLit.value.toString)
    buf += tac
    return (temp, buf)
  }
/*
  //  == Statement Checking ==

  def genStmt(stmt: IrStatement) : ArrayBuffer[Tac] = {

    stmt match {
      case s: IrAssignStmt => {
        return genIrAssignStmt(s)
      }
      case s: IrMethodCallStmt => {
        return genIrMethodCallStmt(s)
      }
      case s: IrIfStmt => {
        return genIrIfStmt(s)
      }
      case s: IrForStmt => {
        return genIrForStmt(s)
      }
      case s: IrWhileStmt => {
        return genIrWhileStmt(s)
      }
      case s: IrReturnStmt => {
        return genIrReturnStmt(s)
      }
      case s: IrBreakStmt => {
        return genIrBreakStmt(s)
      }
      case s: IrContinueStmt => {
        return genIrContinueStmt(s) 
      }
    }
    return false
  }

  def genIrAssignStmt(stmt: IrAssignStmt) : ArrayBuffer[Tac] = {
  }

  def genIrMethodCallStmt(stmt: IrMethodCallStmt) : ArrayBuffer[Tac] = {
  }

  def genIrIfStmt(stmt: IrIfStmt) : ArrayBuffer[Tac] = {
  }

  def genIrForStmt(stmt: IrForStmt) : ArrayBuffer[Tac] = {
  }

  def genIrWhileStmt(stmt: IrWhileStmt) : ArrayBuffer[Tac] = {
  }

  def genIrReturnStmt(stmt: IrReturnStmt) : ArrayBuffer[Tac] = {
  }

  def genIrBreakStmt(stmt: IrBreakStmt) : ArrayBuffer[Tac] = {}

  def genIrContinueStmt(stmt: IrContinueStmt): ArrayBuffer[Tac] = {}
*/
}
