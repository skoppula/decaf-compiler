package compile

import compile.Ir._
import compile.Compiler._
import scala.collection.mutable
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}
import compile.ScopeTypes._

object Gen {
  // == Expr gening ==

  def genExpr(expr: IrExpression) : ArrayBuffer[ThreeAddrCode] = {
    expr match {
      case singleLoc: IrSingleLocation => {
        return genIrSingleLocation(singleLoc)
      }
      case arrayLoc: IrArrayLocation => {
        return genIrArrayLocation(arrayLoc)
      }
      case methodCall: IrMethodCallExpr => {
        return genIrMethodCallExpr(methodCall)
      }
      case intLit: IrIntLiteral => {
        return genIrIntLiteral(intLit)
      }
      case charLit: IrCharLiteral => {
        return genIrCharLiteral(charLit)
      }
      case boolLit: IrBooleanLiteral => {
        return genIrBooleanLiteral(boolLit)
      }
      case binOpExpr: IrBinOpExpr => {
        return genIrBinOpExpr(binOpExpr)
      }
      case unOpExpr: IrUnOpExpr => {
        return genIrUnOpExpr(unOpExpr)
      }
      case ternOpExpr: IrTernOpExpr => {
        return genIrTernOpExpr(ternOpExpr)
      }
    }
    (false, null)
  }

  def genIrTernOpExpr(ternOpExpr: IrTernOpExpr) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrUnOpExpr(unOpExpr: IrUnOpExpr) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrBinOpExpr(binOpExpr: IrBinOpExpr) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrLocation(irLoc: IrLocation) : ArrayBuffer[ThreeAddrCode] = {
    irLoc match {
      case l: IrSingleLocation => {
        return genIrSingleLocation(l)
      }
      case l: IrArrayLocation => {
        return genIrArrayLocation(l)
      }
    }

  }

  def genIrSingleLocation(singleLoc: IrSingleLocation) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrArrayLocation(arrayLoc: IrArrayLocation) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrMethodCallExpr(methodExpr: IrMethodCallExpr) : ArrayBuffer[ThreeAddrCode] =  {
  }

  def genIrIntLiteral(intLit: IrIntLiteral) : ArrayBuffer[ThreeAddrCode] = {
  }

  // Character literals evaluate to their integer ASCII value
  def genIrCharLiteral(charLit: IrCharLiteral) : ArrayBuffer[ThreeAddrCode] = {
  }

  //  == Statement Checking ==

  def genStmt(stmt: IrStatement) : ArrayBuffer[ThreeAddrCode] = {

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

  def genIrAssignStmt(stmt: IrAssignStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrMethodCallStmt(stmt: IrMethodCallStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrIfStmt(stmt: IrIfStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrForStmt(stmt: IrForStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrWhileStmt(stmt: IrWhileStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrReturnStmt(stmt: IrReturnStmt) : ArrayBuffer[ThreeAddrCode] = {
  }

  def genIrBreakStmt(stmt: IrBreakStmt) : ArrayBuffer[ThreeAddrCode] = {}

  def genIrContinueStmt(stmt: IrContinueStmt): ArrayBuffer[ThreeAddrCode] = {}
}
