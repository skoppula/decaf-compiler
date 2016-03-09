package compile

import compile.Ir._
import compile.Compiler._
import scala.collection.mutable
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}

object Check {
    // returns (valid, type, value), where valid is true if the expr was well-formed.
    // type is the type of the expression if well-formed
    // value is returned for cases like binary operation expressions, or int literals; null otherwise
    def checkExpr(
                 methodsTable: MethodsTable, 
                 scopeStack: mutable.Stack[SymbolTable], 
                 expr: IrExpression, 
                 genie: ExceptionGenie
                ) : (Boolean, BaseDescriptor, Any) = { 
        
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
                (true, new BoolTypeDescriptor, value)
            }
            case binOpExpr: IrBinOpExpr => {
                checkIrBinOpExpr(methodsTable, scopeStack, binOpExpr, genie)
            }
            case IrUnOpExpr(unop, expr, loc) => {
                (true, null, null)
            }
            case IrTernOpExpr(cond, leftExpr, rightExpr, loc) => {
                (true, null, null)
            }
        }   
        (false, null, null)
    }

    def checkIrBinOpExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], binOpExpr: IrBinOpExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor, Any) = { 
        val (leftSuccess, leftType, leftValue) = checkExpr(methodsTable, scopeStack, binOpExpr.leftExpr, genie)
        val (rightSuccess, rightType, rightValue) = checkExpr(methodsTable, scopeStack, binOpExpr.rightExpr, genie)
        val op = binOpExpr.binOp 
        op match {
            case arith: IrArithOp => {
                if (leftSuccess && rightSuccess) { 
                    if (leftType.isInstanceOf[IntTypeDescriptor]) {
                        if (rightType.isInstanceOf[IntTypeDescriptor]) {
                            val lV = leftValue.asInstanceOf[Long]
                            val rV = rightValue.asInstanceOf[Long]
                            arith match { 
                                    case IrMulOp() => (true, new BoolTypeDescriptor, lV * rV) //TODO: check this doesn't overflow
                                    case IrDivOp() => (true, new BoolTypeDescriptor, lV / rV) //TODO: divide by zero?
                                    case IrModOp() => (true, new BoolTypeDescriptor, lV % rV)
                                    case IrAddOp() => (true, new BoolTypeDescriptor, lV + rV) //TODO
                                    case IrSubOp() => (true, new IntTypeDescriptor, lV - rV) // TODO
                            }
                        } else {
                            genie.insert(new ArithOpIntOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))
                            (false, null, null)
                        }
                    } else {
                        genie.insert(new ArithOpIntOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
                        (false, null, null)
                    }
                } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
                    (false, null, null)
                }
            }
            case rel: IrRelOp => {
                if (leftSuccess && rightSuccess) {
                    if (leftType.isInstanceOf[IntTypeDescriptor]) {
                        if (rightType.isInstanceOf[IntTypeDescriptor]) {
                                val lV = leftValue.asInstanceOf[Long]
                                val rV = leftValue.asInstanceOf[Long]
                                rel match {
                                    case IrLtOp() => (true, new BoolTypeDescriptor, lV < rV)
                                    case IrLteOp() => (true, new BoolTypeDescriptor, lV <= rV)
                                    case IrGtOp() => (true, new BoolTypeDescriptor, lV > rV)
                                    case IrGteOp() => (true, new BoolTypeDescriptor, lV >= rV)
                                }
                        } else {
                            genie.insert(new ArithOpIntOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))
                            (false, null, null)
                        }
                    } else {
                        genie.insert(new ArithOpIntOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
                        (false, null, null)
                    }
                } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
                    (false, null, null)
                }
            }            
            case eq: IrEqOp => {
                if (leftSuccess && rightSuccess) { 
                    if (leftType == rightType) {
                        if (leftValue == rightValue) {
                            (true, new BoolTypeDescriptor, true)
                        } else {
                            (true, new BoolTypeDescriptor, false)
                        }
                    } else {
                        genie.insert(new EqOpTypeException("Left operand has type: " + leftType + " different from right operand of type " + rightType, binOpExpr.leftExpr.nodeLoc)) 
                        (false, null, null)
                    }
                }
                (false, null, null)        
            }
            case cond: IrCondOp => {
                if (leftSuccess && rightSuccess) {
                    if (leftType.isInstanceOf[BoolTypeDescriptor]) {
                        if (rightType.isInstanceOf[BoolTypeDescriptor]) {
                                val lV = leftValue.asInstanceOf[Boolean]
                                val rV = leftValue.asInstanceOf[Boolean]
                                cond match {
                                    case IrAndOp() => (true, new BoolTypeDescriptor, lV && rV)
                                    case IrOrOp() => (true, new BoolTypeDescriptor, lV || rV)
                                }   
                        } else {
                            genie.insert(new CondOpBoolOperandException("The right operand does not evaluate to integer", binOpExpr.rightExpr.nodeLoc))                           
                            (false, null, null)
                        }       
                    } else {
                        genie.insert(new CondOpBoolOperandException("The left operand does not evaluate to integer", binOpExpr.leftExpr.nodeLoc))
                        (false, null, null)
                    }       
                } else { // one or more of left/rightExpr didn't evaluate correctly. Already generated an error
                    (false, null, null)
                }       
            }
        }
    }

    def checkIrSingleLocation(scopeStack: mutable.Stack[SymbolTable], singleLoc: IrSingleLocation, genie: ExceptionGenie) : (Boolean, BaseDescriptor, Any) = {
        val currScope = scopeStack.top
        val id = currScope.lookupID(singleLoc.name)
        if (id == null) {
            genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + singleLoc.name + singleLoc.loc + " not found."))
            (false, null, null)
        }
        (true, id, null)
    }

    def checkIrArrayLocation(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], arrayLoc: IrArrayLocation, genie: ExceptionGenie) : (Boolean, BaseDescriptor, Any) = { 
        val currScope = scopeStack.top
        val id = currScope.lookupID(arrayLoc.name)
        id match {  
            case null => { 
                genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + arrayLoc.name + arrayLoc.loc + " not found."))
                (false, null, null)
            } 
            case IntArrayTypeDescriptor(size) =>  {
                val (valid, typeOf, value) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
                if (valid) {
                    val reqIndex = value.asInstanceOf[Long] 
                    if (size <= reqIndex || reqIndex < 0) {
                        genie.insert(new OutOfBoundsArrayAccessException("Array out of bounds access: " + arrayLoc.index, arrayLoc.loc))
                        (false, null, null)
                    } else {
                        (true, id, null)
                    }
                } else {
                    (false, null, null)
                }
            }
            case BoolArrayTypeDescriptor(size) => { 
                val (valid, typeOf, value) = checkExpr(methodsTable, scopeStack, arrayLoc.index, genie)
                if (valid) {
                    val reqIndex = value.asInstanceOf[Long] 
                    if (size <= reqIndex || reqIndex < 0) {
                        genie.insert(new OutOfBoundsArrayAccessException("Array out of bounds access: " + arrayLoc.index, arrayLoc.loc))
                        (false, null, null)
                    } else {
                        (true, id, null)
                    }   
                } else {
                    (false, null, null)
                }   
            }
            case _ => {
                genie.insert(new IdentifierIsNotArrayException("Identifier is not an array: " + arrayLoc.name, arrayLoc.loc)) 
                (false, null, null)
            } 
        }
    }

    def checkIrMethodCallExpr(methodsTable: MethodsTable, scopeStack: mutable.Stack[SymbolTable], methodExpr: IrMethodCallExpr, genie: ExceptionGenie) : (Boolean, BaseDescriptor, Any) =  {
        val currScope = scopeStack.top
        val method = methodsTable.lookupID(methodExpr.name)
        if (method == null) { 
            genie.insert(new MethodNotFoundException("Method " + methodExpr.name + " was not found"))
            (false, null, null)
        } else {
            val types = method.getParamTable.values // param types expected by the method decl 
            (false, null, null) // TODO: match type of given arguments vs decl, check callout args             
        }
    }
    
    // TODO: in theory, for every positive usage of int literal, we have to check elsewhere that it doesn't exceed 2^63 - 1.
    def checkIrIntLiteral(intLit: IrIntLiteral, genie: ExceptionGenie) : (Boolean, BaseDescriptor, Long) = {
        if (intLit.rep.size > "9223372036854775808".size) { 
            genie.insert(new InvalidArraySizeException("You specified an array size less than 1 or greater than 2^63", intLit.loc))
            (false, null, -1)
        } else if((intLit.rep.size == "9223372036854775808".size) && (intLit.rep > "9223372036854775808")) {
            genie.insert(new InvalidArraySizeException("You specified an array size less than 1 or greater than 2^63", intLit.loc))
            (false, null, -1)
        } else {
            (true, new IntTypeDescriptor, intLit.rep.toLong) 
        }
    } 

    // Character literals evaluate to their integer ASCII value
    def checkIrCharLiteral(charLit: IrCharLiteral) : (Boolean, BaseDescriptor, Int) = {
        (true, new IntTypeDescriptor, charLit.value.toInt)
    }
}
