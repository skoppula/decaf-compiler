package compile

import compile.Ir._
import compile.Compiler._
import scala.collection.mutable
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}

object Check {
    // returns (valid, value), where valid is true if the expr was well-formed.
    // value is returned for cases like binary operation expressions, or int literals; null otherwise
    def checkExpr(scopeStack: mutable.Stack[SymbolTable], expr: IrExpression, genie: ExceptionGenie) : (Boolean, Any) = {
        expr match {
            case IrSingleLocation(name, loc) => {
                
            }   
            case IrArrayLocation(name, index, loc) => {

            }        
            case IrMethodCallExpr(name, args, loc) => {

            }   
            case IrCalloutExpr(name, args, loc) => { 

            }
            case IrIntLiteral(name, repr, loc) => {

            }
            case IrCharLiteral(name, loc) => { 

            }
            case IrBooleanLiteral(value, loc) => { 

            }
            case IrBinOpExpr(binOp, leftExpr, rightExpr, loc) => {

            }
            case IrUnOpExpr(unop, expr, loc) => {

            }
            case IrTernOpExpr(cond, leftExpr, rightExpr, loc) => {

            }
        }   
        (false, null)
    }

    def checkIrSingleLocation(scopeStack: mutable.Stack[SymbolTable], singleLoc: IrSingleLocation, genie: ExceptionGenie) : Boolean = {
        val currScope = scopeStack.top
        val id = currScope.lookupID(singleLoc.name)
        if (id == null) {
            genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + singleLoc.name + singleLoc.loc + " not found."))
            false
        }
        true
    }

    def checkIrArrayLocation(scopeStack: mutable.Stack[SymbolTable], arrayLoc: IrArrayLocation, genie: ExceptionGenie) : Boolean = { 
        val currScope = scopeStack.top
        val id = currScope.lookupID(arrayLoc.name)
        id match {  
            case null => { 
            genie.insert(new IdentifierNotFoundException("Identifier with name and loc: " + arrayLoc.name + arrayLoc.loc + " not found."))
            false
            } 
            case IntArrayTypeDescriptor(size) =>  {
                val (valid, value) = checkExpr(scopeStack, arrayLoc.index, genie)
                if (valid) {
                    val reqIndex = value.asInstanceOf[Int] 
                    if (size <= reqIndex || reqIndex < 0) {
                        genie.insert(new OutOfBoundsArrayAccessException("Array out of bounds access: " + arrayLoc.index, arrayLoc.loc))
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            case BoolArrayTypeDescriptor(size) => { 
                val (valid, value) = checkExpr(scopeStack, arrayLoc.index, genie)
                if (valid) {
                    val reqIndex = value.asInstanceOf[Int] 
                    if (size <= reqIndex || reqIndex < 0) {
                        genie.insert(new OutOfBoundsArrayAccessException("Array out of bounds access: " + arrayLoc.index, arrayLoc.loc))
                        false
                    } else {
                        true
                    }   
                } else {
                    false
                }   
            }
            case _ => {
                genie.insert(new IdentifierIsNotArrayException("Identifier is not an array: " + arrayLoc.name, arrayLoc.loc)) 
                false
            } 
        }
    }
}
