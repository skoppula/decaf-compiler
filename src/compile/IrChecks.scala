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
        return checkIrSingleLocation(scopeStack, singleLoc, genie)
      }
      case arrayLoc: IrArrayLocation => {
        return checkIrArrayLocation(methodsTable, scopeStack, arrayLoc, genie)
      }
      case methodCall: IrMethodCallExpr => {
        return checkIrMethodCallExpr(methodsTable, scopeStack, methodCall, genie)
      }
      case intLit: IrIntLiteral => {
        return checkIrIntLiteral(intLit, genie)
      }
      case charLit: IrCharLiteral => {
        return checkIrCharLiteral(charLit)
      }
      case IrBooleanLiteral(value, loc) => {
        return (true, new BoolTypeDescriptor)
      }
      case binOpExpr: IrBinOpExpr => {
        return checkIrBinOpExpr(methodsTable, scopeStack, binOpExpr, genie)
      }
      case unOpExpr: IrUnOpExpr => {
        return checkIrUnOpExpr(methodsTable, scopeStack, unOpExpr, genie)
      }
      case ternOpExpr: IrTernOpExpr => {
        return checkIrTernOpExpr(methodsTable, scopeStack, ternOpExpr, genie)
      }
    }
    (false, null)
  }

  def checkIrTernOpExpr(
                         methodsTable: MethodsTable,
                         scopeStack: mutable.Stack[SymbolTable],
                         ternOpExpr: IrTernOpExpr,
                         genie: ExceptionGenie
                       ) : (Boolean, BaseDescriptor) = {
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


  def checkIrUnOpExpr(
                      methodsTable: MethodsTable,
                      scopeStack: mutable.Stack[SymbolTable],
                      unOpExpr: IrUnOpExpr,
                      genie: ExceptionGenie
                     ) : (Boolean, BaseDescriptor) = {
    val (success, exprType) = checkExpr(methodsTable, scopeStack, unOpExpr.expr, genie)
    if (success) {
      val op = unOpExpr.unop
      op match {
        case IrMinusOp() => {
          if (exprType.isInstanceOf[IntTypeDescriptor]) {
            return (true, new IntTypeDescriptor)
          } else {
            genie.insert(new MinusOpTypeException("Expression: " + unOpExpr.expr + " is not an integer.", unOpExpr.nodeLoc))
            return (false, null)
          }
        }
        case IrNotOp() => {
          if (exprType.isInstanceOf[BoolTypeDescriptor]) {
            return (true, new BoolTypeDescriptor)
          } else {
            genie.insert(new NotOpTypeException("Expression: " + unOpExpr.expr + " is not a Boolean.", unOpExpr.nodeLoc))
            return (false, null)
          }
        }
        case IrArraySizeOp() => {
          if (exprType.isInstanceOf[ArrayBaseDescriptor]) {
            return (true, new IntTypeDescriptor)
          } else {
            genie.insert(new LengthOpTypeException("Expression: " + unOpExpr.expr + " is not an array.", unOpExpr.nodeLoc))
            return (false, null)
          }
        }
      }
    } else {
      (false, null)
    }
  }

  def checkIrBinOpExpr(
                        methodsTable: MethodsTable,
                        scopeStack: mutable.Stack[SymbolTable],
                        binOpExpr: IrBinOpExpr,
                        genie: ExceptionGenie
                      ) : (Boolean, BaseDescriptor) = {
    val (leftSuccess, leftType) = checkExpr(methodsTable, scopeStack, binOpExpr.leftExpr, genie)
    val (rightSuccess, rightType) = checkExpr(methodsTable, scopeStack, binOpExpr.rightExpr, genie)
    val op = binOpExpr.binOp
    op match {
      case arith: IrArithOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType.isInstanceOf[IntTypeDescriptor]) {
            if (rightType.isInstanceOf[IntTypeDescriptor]) {
              arith match {
                case IrMulOp() => return (true, new IntTypeDescriptor)
                case IrDivOp() => return (true, new IntTypeDescriptor)
                case IrModOp() => return (true, new IntTypeDescriptor)
                case IrAddOp() => return (true, new IntTypeDescriptor)
                case IrSubOp() => return (true, new IntTypeDescriptor)
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
                case IrLtOp() => return (true, new BoolTypeDescriptor)
                case IrLteOp() => return (true, new BoolTypeDescriptor)
                case IrGtOp() => return (true, new BoolTypeDescriptor)
                case IrGteOp() => return (true, new BoolTypeDescriptor)
              }
            } else {
              genie.insert(new ArithOpIntOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))
              return (false, null)
            }
          } else {
            genie.insert(new ArithOpIntOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
            return (false, null)
          }
        } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
          return (false, null)
        }
      }
      case eq: IrEqOp => {
        if (leftSuccess && rightSuccess) {
          if (leftType == rightType) {
            (true, new BoolTypeDescriptor)
          } else {
            genie.insert(new EqOpTypeException("Left operand has type: " + leftType + " different from right operand of type " + rightType, binOpExpr.leftExpr.nodeLoc))
            return (false, null)
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
              return (false, null)
            }
          } else {
            genie.insert(new CondOpBoolOperandException("The left operand does not evaluate to a boolean", binOpExpr.leftExpr.nodeLoc))
            return (false, null)
          }
        } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
          return (false, null)
        }
      }
    }
  }

  def checkIrLocation(
                       methodsTable: MethodsTable,
                       scopeStack: mutable.Stack[SymbolTable],
                       irLoc: IrLocation,
                       genie: ExceptionGenie
                     ) : (Boolean, BaseDescriptor) = {
    irLoc match {
      case l: IrSingleLocation => {
        return checkIrSingleLocation(scopeStack, l, genie)
      }
      case l: IrArrayLocation => {
        return checkIrArrayLocation(methodsTable, scopeStack, l, genie)
      }
    }

  }

  def checkIrSingleLocation(
                             scopeStack: mutable.Stack[SymbolTable],
                             singleLoc: IrSingleLocation,
                             genie: ExceptionGenie) : (Boolean, BaseDescriptor) = {
    val currScope = scopeStack.top
    val id = currScope.lookupID(singleLoc.name)
    if (id == null) {
      genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + singleLoc.name + singleLoc.loc + " not found."))
      (false, null)
    }
    (true, id)
  }

  def checkIrArrayLocation(
                            methodsTable: MethodsTable,
                            scopeStack: mutable.Stack[SymbolTable],
                            arrayLoc: IrArrayLocation,
                            genie: ExceptionGenie
                          ) : (Boolean, BaseDescriptor) = {
    val currScope = scopeStack.top
    val id = currScope.lookupID(arrayLoc.name)
    id match {
      case null => {
        genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + arrayLoc.name + arrayLoc.loc + " not found."))
        return (false, null)
      }
      case IntArrayTypeDescriptor(size) =>  {
        val (valid, typeOf) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
        if (valid) {
          if (typeOf.isInstanceOf[IntTypeDescriptor]) {
            return (true, new IntTypeDescriptor)
          } else {
            genie.insert(new IdentifierIsArrayButIndexIsNotIntException("Identifier " + arrayLoc.name + " with Expr is not of type int", arrayLoc.loc))
            return (false, null)
          }
        } else {
          return (false, null)
        }
      }
      case BoolArrayTypeDescriptor(size) => {
        val (valid, typeOf) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
        if (valid) {
          if (typeOf.isInstanceOf[IntTypeDescriptor]) {
            (true, new BoolTypeDescriptor)
          } else {
            genie.insert(new IdentifierIsArrayButIndexIsNotIntException("Identifier " + arrayLoc.name + " with Expr is not of type int", arrayLoc.loc))
            return (false, null)
          }
        } else {
          return (false, null)
        }
      }
      case _ => {
        genie.insert(new IdentifierIsNotArrayException("Identifier is not an array: " + arrayLoc.name, arrayLoc.loc))
        return (false, null)
      }
    }
  }

  def checkMethodHelper(
                         methodsTable: MethodsTable,
                         scopeStack: mutable.Stack[SymbolTable],
                         methodExpr: IrMethodCallExpr,
                         genie: ExceptionGenie,
                         checkVoid: Boolean
                       ) : (Boolean, BaseDescriptor) =  {

    if(scopeStack.top.lookupIDOnlyInLocalScope(methodExpr.name) != null) {
      genie.insert(new MethodCallIsShadowedByLocalVariable("Method call to " + methodExpr.name + " is shadowed by a variable of the same name", methodExpr.loc))
      return (false, null)
    }

    val method = methodsTable.lookupID(methodExpr.name)
    if (method == null) {
      if(methodsTable.isCallout(methodExpr.name)) {
        return (false, new IntTypeDescriptor)
      } else {
        genie.insert(new MethodNotFoundException("Method " + methodExpr.name + " was not found"))
        (false, null)
      }
    } else {
      val expectedTypes = method.getParamTable.values.toSeq // param types expected by the method decl

      if(expectedTypes.size != methodExpr.args.size) {
        genie.insert(new IncorrectNumberOfArgsException("Incorrect number of arguments in " + methodExpr.name, methodExpr.loc))
      }

      if(checkVoid) {
        if (method.methodType.isInstanceOf[VoidTypeDescriptor]) {
          genie.insert(new VoidCannotBeDeclarationTypeException("Method referenced returns void, but used in non-void setting", methodExpr.loc))
          return (false, null)
        }
      }

      var argCheck : Boolean = true
      var i : Integer = 0
      while (argCheck && i < expectedTypes.size) {
        val arg = methodExpr.args(i)
        val expectedArgType = expectedTypes(i)

        if(arg.isInstanceOf[IrCallStringArg]) {
          genie.insert(new StringArgInMethodCallException("String argument found in method call to " + methodExpr.name, methodExpr.loc))
        }

        val argExpr = arg.asInstanceOf[IrCallExprArg].arg
        val (argIsValid, argType) = checkExpr(methodsTable, scopeStack, argExpr, genie)

        if(!argIsValid) {
          return (false, null)
        }

        if(expectedArgType.isInstanceOf[IntTypeDescriptor]) {
          if(!argType.isInstanceOf[IntTypeDescriptor]) {
            argCheck = false
          }
        } else if (expectedArgType.isInstanceOf[BoolTypeDescriptor]) {
          if(!argType.isInstanceOf[BoolTypeDescriptor]) {
            argCheck = false
          }
        }

        i += 1
      }

      i -= 1
      if(argCheck) {
        return (true, method.methodType)
      } else {
        genie.insert(new InvalidMethodCallArgumentException("Method " + methodExpr.name + " has an incorrect argument", methodExpr.nodeLoc))
        return (false, null)
      }
    }

  }

  def checkIrMethodCallExpr(
                             methodsTable: MethodsTable,
                             scopeStack: mutable.Stack[SymbolTable],
                             methodExpr: IrMethodCallExpr,
                             genie: ExceptionGenie
                           ) : (Boolean, BaseDescriptor) =  {
    checkMethodHelper(methodsTable, scopeStack, methodExpr, genie, true)
  }

  def checkIrIntLiteral(
                         intLit: IrIntLiteral,
                         genie: ExceptionGenie
                       ) : (Boolean, BaseDescriptor) = {
    intLit.value match {
      case Some(v) => {
        if (v > Long.MaxValue) {
          genie.insert(new InvalidIntLiteralException("Integer literal greater than maximum 2^63 - 1", intLit.loc))
          return (false, null)
        } else if (v < Long.MinValue) {
          genie.insert(new InvalidIntLiteralException("Integer literal lower than minimum 2^63", intLit.loc))
          return (false, null)
        } else {
          return (true, new IntTypeDescriptor)
        }
      }
      case None => {
        return (false, null)
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
        return checkIrAssignStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrMethodCallStmt => {
        return checkIrMethodCallStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrIfStmt => {
        return checkIrIfStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrForStmt => {
        return checkIrForStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrWhileStmt => {
        return checkIrWhileStmt(methodsTable, scopeStack, s, genie)
      }
      case s: IrReturnStmt => {
        return checkIrReturnStmt(methodsTable, scopeStack, s, topMethodName, genie)
      }
      case s: IrBreakStmt => {
        val scopeFilter = scopeStack.toList.filter((s:SymbolTable) => (s.scopeType == FOR || s.scopeType == WHILE))
        if (scopeFilter.length > 0) {
          return true
        } else {
          genie.insert(new BreakStmtInvalidScope("Break statement not contained in a FOR or WHILE block", loc))
          return false
        }
      }
      case s: IrContinueStmt => {
        val scopeFilter = scopeStack.toList.filter((s:SymbolTable) => (s.scopeType == FOR || s.scopeType == WHILE))
        println(scopeFilter)
        if (scopeFilter.length > 0) {
          return true
        } else {
          genie.insert(new BreakStmtInvalidScope("Continue statement not contained in a FOR or WHILE block", loc))
          return false
        }
      }
    }
    return false
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
            return true
          } else {
            genie.insert(new AssignEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType +  " and expr " + expr + " with type " + exprType + "do not match" , loc))
            return false
          }
        }
        case IrMinusAssignStmt(_,_,_) => {
          if (irLocType.isInstanceOf[IntTypeDescriptor] && exprType.isInstanceOf[IntTypeDescriptor]) {
            return true
          } else {
            if (!irLocType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignMinusEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType + " is not of type int", irLoc.nodeLoc))
            }
            if (!exprType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignMinusEqStmtTypeMismatch("Expr " + expr + " with type " + irLocType + " is not of type int", expr.nodeLoc))
            }
            return false
          }
        }
        case IrPlusAssignStmt(_,_,_) => {
          if (irLocType.isInstanceOf[IntTypeDescriptor] && exprType.isInstanceOf[IntTypeDescriptor]) {
            return true
          } else {
            if (!irLocType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignPlusEqStmtTypeMismatch("The type of location " + irLoc.name + " with type " + irLocType + " is not of type int", irLoc.nodeLoc))
            }
            if (!exprType.isInstanceOf[IntTypeDescriptor]) {
              genie.insert(new AssignPlusEqStmtTypeMismatch("Expr " + expr + " with type " + irLocType + " is not of type int", expr.nodeLoc))
            }
            return false
          }
        }
      }

    } else { // Error already generated upon checking location or expr or both
      return false
    }

  }

  def checkIrMethodCallStmt(
    methodsTable: MethodsTable,
    scopeStack: mutable.Stack[SymbolTable],
    stmt: IrMethodCallStmt,
    genie: ExceptionGenie
  ) : Boolean = {
    val methodCallExpr : IrMethodCallExpr = stmt.methCall.asInstanceOf[IrMethodCallExpr]
    val (valid, _) = checkMethodHelper(methodsTable, scopeStack, methodCallExpr, genie, false)
    return valid
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
      return false
    } else {
      return true
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
      if(methodType == null) {
        throw new MethodNotFoundException("Method not found exception when compiler was checking a return statement")
      }

      stmt.value match {
        case Some(expr) => {
          val (exprSuccess, exprType) = checkExpr(methodsTable, scopeStack, expr, genie)
          if (exprSuccess) {
            if (exprType == methodType) {
              return true
            } else {
              genie.insert(new ReturnStmtTypeMismatch("Return stmt has expr " + expr + " with type " + exprType + "when method " + topMethodName + " has type " + methodType, stmt.nodeLoc))
              return false
            }
          } else {
            return false
          }
        }
        case None => {
          if (methodType.isInstanceOf[VoidTypeDescriptor]) {
            return true
          } else {
            genie.insert(new ReturnStmtTypeMismatch("Return stmt has no return expr when method type is void", stmt.nodeLoc))
            return false
          }
        }
      }

    } catch {
      // Should be impossible for this to trigger unless someone misused the method
      case mntfe : MethodNotFoundException => {false}
    }
  }

}
