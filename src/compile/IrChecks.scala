package compile

import compile.Ir._
import compile.Compiler._
import scala.collection.mutable
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}
import compile.ScopeTypes._

object Check {
  // == Declaration checking ==

  // Checks if the size corresponding to an array declaration is valid
  def checkArrayDeclSize(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    arrayDecl: IrArrayFieldDecl,
    genie: ExceptionGenie
  ) : (Boolean) = {
    //case class         IrArrayFieldDecl(name: String, size: IrIntLiteral, loc: NodeLocation) extends IrFieldDeclArg(loc)
  //def checkIrIntLiteral(intLit: IrIntLiteral, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val size = arrayDecl.size
    val (sizeSuccess, sizeType) = checkIrIntLiteral(size, genie)
    if (sizeSuccess) {
     size.value match {
       case Some(v) => {
         if (!(v > 0)) {
           genie.insert(new InvalidArraySizeException("array size " + size.rep + " is not positive", size.nodeLoc))
           false
         } else {
           true
         }
       }
       case None => {false}
     }
    } else {
      false
    }
  }

  // == Expr checking ==

  // returns (valid, type), where valid is true if the expr was well-formed.
  // type is the type of the expression if well-formed
  def checkExpr(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    expr: IrExpression,
    genie: ExceptionGenie
  ) : (Boolean, BaseDescriptor) = {
    
    expr match {
      case singleLoc: IrSingleLocation => {
        checkIrSingleLocation(scopeStack, singleLoc, genie)
      }
      case arrayLoc: IrArrayLocation => {
        checkIrArrayLocation(methodsTable, scopeStack, arrayLoc, genie)
      }
      case methodCall: IrMethodCallExpr => {
        checkIrMethodCallExpr(methodsTable, scopeStack, methodCall, genie)
      }
      case intLit: IrIntLiteral => {
        checkIrIntLiteral(intLit, genie)
      }
      case charLit: IrCharLiteral => {
        checkIrCharLiteral(charLit)
      }
      case IrBooleanLiteral(value, loc) => {
        (true, new BoolTypeDescriptor)
      }
      case binOpExpr: IrBinOpExpr => {
        checkIrBinOpExpr(methodsTable, scopeStack, binOpExpr, genie)
      }
      case unOpExpr: IrUnOpExpr => {
        checkIrUnOpExpr(methodsTable, scopeStack, unOpExpr, genie)
      }
      case ternOpExpr: IrTernOpExpr => {
        checkIrTernOpExpr(methodsTable, scopeStack, ternOpExpr, genie)
      }
    }
    (false, null)
  }

  def checkIrTernOpExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], ternOpExpr: IrTernOpExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val (condSuccess, condType) = checkExpr(methodsTable, scopeStack, ternOpExpr.cond, genie)
    val (leftSuccess, leftType) = checkExpr(methodsTable, scopeStack, ternOpExpr.leftExpr, genie)
    val (rightSuccess, rightType) = checkExpr(methodsTable, scopeStack, ternOpExpr.rightExpr, genie)
    if (condSuccess && leftSuccess && rightSuccess) {
      if (condType.isInstanceOf[BoolTypeDescriptor]) {
        if (leftType == rightType) {
          (true, leftType)
        } else {
          genie.insert(new TernOpMatchTypeException("The ternary expressions: " + ternOpExpr.leftExpr + " and " + ternOpExpr.rightExpr + " must have the same type.", ternOpExpr.nodeLoc))
          (false, null)
        }
      } else {
        genie.insert(new TernOpCondTypeException("The condition: " + ternOpExpr.cond + " must be a Boolean", ternOpExpr.nodeLoc))
        (false, null)
      }

    } else {
      (false, null)
    }
  }


  def checkIrUnOpExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], unOpExpr: IrUnOpExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val (success, exprType) = checkExpr(methodsTable, scopeStack, unOpExpr.expr, genie)
    if (success) {
      val op = unOpExpr.unop
      op match {
        case IrMinusOp() => {
          if (exprType.isInstanceOf[IntTypeDescriptor]) {
            (true, new IntTypeDescriptor)
          } else {
            genie.insert(new MinusOpTypeException("Expression: " + unOpExpr.expr + " is not an integer.", unOpExpr.nodeLoc))
            (false, null)
          }
        }
        case IrNotOp() => {
          if (exprType.isInstanceOf[BoolTypeDescriptor]) {
            (true, new BoolTypeDescriptor)
          } else {
            genie.insert(new NotOpTypeException("Expression: " + unOpExpr.expr + " is not a Boolean.", unOpExpr.nodeLoc))
            (false, null)
          }
        }
        case IrArraySizeOp() => {
          if (exprType.isInstanceOf[ArrayBaseDescriptor]) {
            (true, new IntTypeDescriptor)
          } else {
            genie.insert(new LengthOpTypeException("Expression: " + unOpExpr.expr + " is not an array.", unOpExpr.nodeLoc))
            (false, null)
          }
        }
      }
    } else {
      (false, null)
    }
  }

  def checkIrBinOpExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], binOpExpr: IrBinOpExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val (leftSuccess, leftType) = checkExpr(methodsTable, scopeStack, binOpExpr.leftExpr, genie)
    val (rightSuccess, rightType) = checkExpr(methodsTable, scopeStack, binOpExpr.rightExpr, genie)
    val op = binOpExpr.binOp
    op match {
      case arith: IrArithOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType.isInstanceOf[IntTypeDescriptor]) {
            if (rightType.isInstanceOf[IntTypeDescriptor]) {
              arith match {
                case IrMulOp() => (true, new IntTypeDescriptor)
                case IrDivOp() => (true, new IntTypeDescriptor)
                case IrModOp() => (true, new IntTypeDescriptor)
                case IrAddOp() => (true, new IntTypeDescriptor)
                case IrSubOp() => (true, new IntTypeDescriptor)
              }
            } else {
              genie.insert(new ArithOpIntOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))
              (false, null)
            }
          } else {
            genie.insert(new ArithOpIntOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
            (false, null)
          }
        } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
          (false, null)
        }
      }
      case rel: IrRelOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType.isInstanceOf[IntTypeDescriptor]) {
            if (rightType.isInstanceOf[IntTypeDescriptor]) {
              rel match {
                case IrLtOp() => (true, new BoolTypeDescriptor)
                case IrLteOp() => (true, new BoolTypeDescriptor)
                case IrGtOp() => (true, new BoolTypeDescriptor)
                case IrGteOp() => (true, new BoolTypeDescriptor)
              }
            } else {
              genie.insert(new ArithOpIntOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))
              (false, null)
            }
          } else {
            genie.insert(new ArithOpIntOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
            (false, null)
          }
        } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
          (false, null)
        }
      }
      case eq: IrEqOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType == rightType) {
            (true, new BoolTypeDescriptor)
          } else {
            genie.insert(new EqOpTypeException("Left operand has type: " + leftType + " different from right operand of type " + rightType, binOpExpr.leftExpr.nodeLoc))
            (false, null)
          }
        }
        (false, null)
      }
      case cond: IrCondOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType.isInstanceOf[BoolTypeDescriptor]) {
            if (rightType.isInstanceOf[BoolTypeDescriptor]) {
              cond match {
                case IrAndOp() => (true, new BoolTypeDescriptor)
                case IrOrOp() => (true, new BoolTypeDescriptor)
              }
            } else {
              genie.insert(new CondOpBoolOperandException("The right operand does not evaluate to a boolean", binOpExpr.rightExpr.nodeLoc))
              (false, null)
            }
          } else {
            genie.insert(new CondOpBoolOperandException("The left operand does not evaluate to a boolean", binOpExpr.leftExpr.nodeLoc))
            (false, null)
          }
        } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
          (false, null)
        }
      }
    }
  }

  def checkIrLocation(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], irLoc: IrLocation, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    irLoc match {
      case l: IrSingleLocation => {
        checkIrSingleLocation(scopeStack, l, genie)
      }
      case l: IrArrayLocation => {
        checkIrArrayLocation(methodsTable, scopeStack, l, genie)
      }
    }

  }

  def checkIrSingleLocation(scopeStack: mutable.Stack[SymbolTable], singleLoc: IrSingleLocation, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val currScope = scopeStack.top
    val id = currScope.lookupID(singleLoc.name)
    if (id == null) {
      genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + singleLoc.name + singleLoc.loc + " not found."))
      (false, null)
    }
    (true, id)
  }

  def checkIrArrayLocation(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], arrayLoc: IrArrayLocation, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val currScope = scopeStack.top
    val id = currScope.lookupID(arrayLoc.name)
    id match {
      case null => {
        genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + arrayLoc.name + arrayLoc.loc + " not found."))
        (false, null)
      }
      case IntArrayTypeDescriptor(size) =>  {
        val (valid, typeOf) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
        if (valid) {
          if (typeOf.isInstanceOf[IntTypeDescriptor]) {
            (true, id)
          } else {
            genie.insert(new IdentifierIsArrayButIndexIsNotIntException("Identifier " + arrayLoc.name + " with Expr is not of type int", arrayLoc.loc))
            (false, null)
          }
        } else {
          (false, null)
        }
      }
      case BoolArrayTypeDescriptor(size) => {
        val (valid, typeOf) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
        if (valid) {
          if (typeOf.isInstanceOf[IntTypeDescriptor]) {
            (true, id)
          } else {
            genie.insert(new IdentifierIsArrayButIndexIsNotIntException("Identifier " + arrayLoc.name + " with Expr is not of type int", arrayLoc.loc))
            (false, null)
          }
        } else {
          (false, null)
        }
      }
      case _ => {
        genie.insert(new IdentifierIsNotArrayException("Identifier is not an array: " + arrayLoc.name, arrayLoc.loc))
        (false, null)
      }
    }
  }

  def checkIrMethodCallExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], methodExpr: IrMethodCallExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor) =  {
    val currScope = scopeStack.top
    val method = methodsTable.lookupID(methodExpr.name)
    if (method == null) {
      genie.insert(new MethodNotFoundException("Method " + methodExpr.name + " was not found"))
      (false, null)
    } else {
      val expectedTypes = method.getParamTable.values // param types expected by the method decl
      val providedTypes = new Array[BaseDescriptor](0)
      
      // BIG TODO: Check method name against callouts as well? If it's a callout, then we don't have to verify the args, and the return type             // is int.
      /*
       for (args <- methodExpr.args) {
       arg match { 
       case exprArg: IrCallExprArg {
       val (valid, typeOf, value) = checkExpr(methodsTable, scopeStack, exprArg, genie)
       if (valid) { 
       providedTypes +: typeOf
       } else {
       (false, null, null)
       }
       }
       } 
       }
       if (providedTypes == expectedTypes) {
       if (!method.returnType.isInstanceOf[VoidTypeDescriptor]) {
       (true, method.returnType, null) 
       } else { 
       genie.insert(new InvalidMethodCallReturnException("Method " + methodExpr.name + " does not return a value, methodExpr.nodeLoc))
       }
       } else {
       genie.insert(new InvalidMethodCallArgumentException("Method " + methodExpr.name + " has an incorrect argument", methodExpr.nodeLoc))
       }
       */
      (false, null)
    }
  }

  def checkIrIntLiteral(intLit: IrIntLiteral, genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    intLit.value match {
      case Some(v) => {
        if (v > Long.MaxValue) {
          genie.insert(new InvalidIntLiteralException("Integer literal greater than maximum 2^63 - 1", intLit.loc))
          (false, null)
        } else if (v < Long.MinValue) {
          genie.insert(new InvalidIntLiteralException("Integer literal lower than minimum 2^63", intLit.loc))
          (false, null)
        } else {
          (true, new IntTypeDescriptor)
        }
      }
      case None => {
        (false, null)
      }
    }
  }

  // Character literals evaluate to their integer ASCII value
  def checkIrCharLiteral(charLit: IrCharLiteral) : (Boolean, BaseDescriptor) = {
    (true, new IntTypeDescriptor)
  }



  //  == Statement Checking ==

  def checkStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrStatement,
    topMethodName: String,
    genie: ExceptionGenie
  ) : Boolean = {
    val loc = stmt.nodeLoc

    stmt match {
      case s: IrAssignStmt => {
        checkIrAssignStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrMethodCallStmt => {
        checkIrMethodCallStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrIfStmt => {
        checkIrIfStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrForStmt => {
        checkIrForStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrWhileStmt => {
        checkIrWhileStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrReturnStmt => {
        checkIrReturnStmt(methodsTable, scopeStack, s, topMethodName, genie)
      }
      case s: IrBreakStmt => {
        val scopeFilter = scopeStack.toList.filter((s:SymbolTable) => !(s.scopeType == FOR || s.scopeType == WHILE))
        if (scopeFilter.length > 0) {
          true
        } else {
          genie.insert(new BreakStmtInvalidScope("Break statement not contained in a FOR or WHILE block", loc))
          false
        }
      }
      case s: IrContinueStmt => {
        val scopeFilter = scopeStack.toList.filter((s:SymbolTable) => !(s.scopeType == FOR || s.scopeType == WHILE))
        if (scopeFilter.length > 0) {
          true
        } else {
          genie.insert(new BreakStmtInvalidScope("Break statement not contained in a FOR or WHILE block", loc))
          false
        }
      }
    }
    false
  }

  def checkIrAssignStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrAssignStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    val (irLoc, expr, loc) = (stmt.irLoc, stmt.expr, stmt.nodeLoc)
    val (irLocSuccess, irLocType) = checkIrLocation(methodsTable, scopeStack, irLoc, genie)
    val (exprSuccess, exprType) = checkExpr(methodsTable, scopeStack, expr, genie)

    if (irLocSuccess && exprSuccess) {

      stmt match {
        case IrEqualsAssignStmt(_,_,_) => {
          if (irLocType == exprType) {
            true
          } else {
            genie.insert(new AssignEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType +  " and expr " + expr + " with type " + exprType + "do not match" , loc))
            false
          }
        }
        case IrMinusAssignStmt(_,_,_) => {
          if (irLocType.isInstanceOf[IntTypeDescriptor] && exprType.isInstanceOf[IntTypeDescriptor]) {
            true
          } else {
            if (!irLocType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignMinusEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType + " is not of type int", irLoc.nodeLoc))
            }
            if (!exprType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignMinusEqStmtTypeMismatch("Expr " + expr + " with type " + irLocType + " is not of type int", expr.nodeLoc))
            }
            false
          }
        }
        case IrPlusAssignStmt(_,_,_) => {
          if (irLocType.isInstanceOf[IntTypeDescriptor] && exprType.isInstanceOf[IntTypeDescriptor]) {
            true
          } else {
            if (!irLocType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignPlusEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType + " is not of type int", irLoc.nodeLoc))
            }
            if (!exprType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignPlusEqStmtTypeMismatch("Expr " + expr + " with type " + irLocType + " is not of type int", expr.nodeLoc))
            }
            false
          }
        }
      }

    } else { // Error already generated upon checking location or expr or both
      false
    }

  }

  def checkIrMethodCallStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrMethodCallStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    // Big TODO - implementing method call stmt.
    // Note that the difference here is that it is okay if the method call is of type void
    // whereas method calls used in expr must be of type int or bool

    false
  }

  def checkIrIfStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrIfStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    val expr = stmt.cond
    val (exprSuccess, exprType) = checkExpr(methodsTable, scopeStack, expr, genie)
    if (exprSuccess) {
      if (exprType.isInstanceOf[BoolTypeDescriptor]) {
        true
      } else {
        genie.insert(new IfStmtCondTypeMismatch("If condition expr " + expr + " with type " + exprType + " is not of type bool", expr.nodeLoc))
        false
      }
    } else {
      false
    }
  }

  def checkIrForStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrForStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    val (irLoc, initExpr, endExpr, inc, loc) = (stmt.irLoc, stmt.initVal, stmt.endVal, stmt.inc, stmt.nodeLoc)
    var isError = false

    // check that irLoc has been previously declared and is of type integer
    val (irLocSuccess, irLocType) = checkIrLocation(methodsTable, scopeStack, irLoc, genie)
    if (irLocSuccess) {
      if (!irLocType.isInstanceOf[IntTypeDescriptor]) {
        genie.insert(new ForStmtIterLocTypeMismatch("location " + irLoc + " with type " + irLocType + " is not of type int", initExpr.nodeLoc))
        isError = true
      }
    }

    // checking initial expr (rule 21)
    val (initExprSuccess, initExprType) = checkExpr(methodsTable, scopeStack, initExpr, genie)
    if (initExprSuccess) {
      if (!initExprType.isInstanceOf[IntTypeDescriptor]) {
        genie.insert(new ForStmtInitExprTypeMismatch("inital value expr " + initExpr + " with type " + initExprType + " is not of type int", initExpr.nodeLoc))
        isError = true
      }
    }

    // checking ending expr (rule 21)
    val (endExprSuccess, endExprType) = checkExpr(methodsTable, scopeStack, endExpr, genie)
    if (endExprSuccess) {
      if (!endExprType.isInstanceOf[IntTypeDescriptor]) {
        genie.insert(new ForStmtEndExprTypeMismatch("end value expr " + endExpr + " with type " + endExprType + " is not of type int", endExpr.nodeLoc))
        isError = true
      }
    }

    // checking inc (rule 22)
    inc match {
      case Some(incVal) => {
        val (incValSuccess, incValType) = checkIrIntLiteral(incVal, genie)
        if (incValSuccess) {
          incVal.value match {
            case Some(v) => {
              if (!(v > 0)) {
                genie.insert(new ForStmtIncValMismatch("inc value " + incVal.rep + " is not positive", incVal.nodeLoc))
                isError = true
              }
            }
            case None => {isError = true}
          }
        } else {
          isError = true
        }
      }
      case None => {}
    }

    if (isError) {
      false
    } else {
      true
    }

  }

  def checkIrWhileStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrWhileStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    val expr = stmt.boolExpr
    val (exprSuccess, exprType) = checkExpr(methodsTable, scopeStack, expr, genie)
    if (exprSuccess) {
      if (exprType.isInstanceOf[BoolTypeDescriptor]) {
        true
      } else {
        genie.insert(new WhileStmtCondTypeMismatch("While condition expr " + expr + " with type " + exprType + " is not of type bool", expr.nodeLoc))
        false
      }
    } else {
      false
    }
  }

  def checkIrReturnStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrReturnStmt,
    topMethodName: String,
    genie: ExceptionGenie
  ) : Boolean = {
    try {
      val methodType = methodsTable.lookupID(topMethodName).methodType

      stmt.value match {
        case Some(expr) => {
          val (exprSuccess, exprType) = checkExpr(methodsTable, scopeStack, expr, genie)
          if (exprSuccess) {
            if (exprType == methodType) {
              true
            } else {
              genie.insert(new ReturnStmtTypeMismatch("Return stmt has expr " + expr + " with type " + exprType + "when method " + topMethodName + " has type " + methodType, stmt.nodeLoc))
              false
            }
          } else {
            false
          }
        }
        case None => {
          if (methodType.isInstanceOf[VoidTypeDescriptor]) {
            true
          } else {
            genie.insert(new ReturnStmtTypeMismatch("Return stmt has no return expr when method type is void", stmt.nodeLoc))
            false
          }
        }
      }

    } catch {
      // Should be impossible for this to trigger unless someone misused the method
      case mntfe : MethodNotFoundException => {false}
    }
  }

}