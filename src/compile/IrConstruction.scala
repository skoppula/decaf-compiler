package compile

import edu.mit.compilers.grammar.{DecafParserTokenTypes => Token}
import antlr.collections.AST
import compile.Ir._

object IrConstruction {

  def constructIR(ast : TokenAST, exceptionGenie: ExceptionGenie) : IrProgram = {
    /** Expects TokenAST from ANTLR
      */
    val numChild = ast.getNumberOfChildren()
    var child = ast.getFirstChild()

    var calloutDecls : List[IrCalloutDecl] = List()
    var fieldDecls : List[IrFieldDecl] = List()
    var methodDecls : List[IrMethodDecl] = List()

    for ( _ <- 0 until numChild) {
      child.getType() match {
        case Token.CALLOUT => {
          calloutDecls :+= calloutNodeToIrCalloutDecl(child, exceptionGenie)
        }
        case Token.FIELD_DECL => {
          fieldDecls :+= fieldDeclNodeToIrFieldDecl(child, exceptionGenie)
        }
        case Token.METHOD_DECL => {
          methodDecls :+= methodDeclNodeToIrMethodDecl(child, exceptionGenie)
        }
      }
      child = child.getNextSibling()
    }

    return IrProgram(calloutDecls, fieldDecls, methodDecls)
  }

  // Expects a AST rooted at Token.CALLOUT node
  def calloutNodeToIrCalloutDecl(ast: AST, exceptionGenie: ExceptionGenie) : IrCalloutDecl = {
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())

    if (ast.getType() != Token.CALLOUT) {
      exceptionGenie.insert(new ExpectingCalloutTokenException("Error: Failed CALLOUT node conversion", nodeLoc))
    }

    // Process callout node
    val calloutId = ast.getFirstChild().getText()

    println("Callout Declaration: " + calloutId)

    return IrCalloutDecl(calloutId, nodeLoc)
  }

  // Expects a AST rooted at Token.FIELD_DECL node
  def fieldDeclNodeToIrFieldDecl(ast: AST, exceptionGenie: ExceptionGenie) : IrFieldDecl = {
    if (ast.getType() != Token.FIELD_DECL) {
      // Should not reach here unless someone misused this method
      exceptionGenie.insert(new ExpectingFieldDeclTokenException("Error: Failed FIELD_DECL node conversion", new NodeLocation(0,0)))
    }

    // Process field type node
    val fieldType = typeNodeToIrType(ast.getFirstChild(), exceptionGenie)
    val nodeLoc = fieldType.nodeLoc

    // Process field declaration argument ndoes
    var fieldArgs : List[IrFieldDeclArg] = List()
    val numFields = ast.getNumberOfChildren() - 1
    var fieldArgNode = ast.getFirstChild().getNextSibling() // FIELD_DECL_ARG
    for ( _ <- 0 until numFields) {
      val field = fieldArgNode.getFirstChild() // ID or array_id
      field.getType() match {
        case Token.ID => {
          val fieldName = field.getText()
          fieldArgs :+= IrSingleFieldDecl(fieldName, nodeLoc)
        }
        case Token.ARRAY_ID => {
          val arrayFieldName = field.getFirstChild().getText()
          val arraySizeStr = field.getFirstChild().getNextSibling().getText()
          var intVal = None: Option[BigInt];
          try {
            intVal = Some(getBigIntValue(arraySizeStr))
            if (intVal.get < 1) {
              throw new InvalidArraySizeException("You specified an array size less than 1", nodeLoc)
            }
          } catch {
            case nfa: NumberFormatException => {
              exceptionGenie.insert(new InvalidIntLiteralException("Cannot parse int literal", nodeLoc))
            }
          }
          val arrayFieldSize = IrIntLiteral(intVal, arraySizeStr, nodeLoc)
          fieldArgs :+= IrArrayFieldDecl(arrayFieldName, arrayFieldSize, nodeLoc)
        }
        case _ => {
          exceptionGenie.insert(new ExpectingIDInFieldDeclException("Error: Neither ID nor Array_ID in field declaration argument", nodeLoc))
        }
      }
      fieldArgNode = fieldArgNode.getNextSibling()
    }

    println("Field Declaration: type: " + fieldType + " args: " + fieldArgs)

    return IrFieldDecl(fieldType, fieldArgs, nodeLoc)
  }

  // Expects an AST rooted at Token.METHOD_DECL node
  def methodDeclNodeToIrMethodDecl(ast: AST, exceptionGenie: ExceptionGenie) : IrMethodDecl = {
    if (ast.getType() != Token.METHOD_DECL) {
      // Should not reach here unless someone misused this method
      exceptionGenie.insert(new ExpectingFieldDeclTokenException("Error: Failed METHOD_DECL node conversion", new NodeLocation(0,0)))
    }

    // Process method type node
    val methodTypeNode = ast.getFirstChild()
    val methodType = {
      ast.getFirstChild().getType() match {
        case Token.TYPE => typeNodeToIrType(methodTypeNode, exceptionGenie)
        case Token.VOID => IrVoidType(new NodeLocation(ast.getFirstChild().getLine(), ast.getFirstChild().getColumn()))
      }
    }

    val nodeLoc = methodType.nodeLoc


    // Process method name node
    val methodNameNode = methodTypeNode.getNextSibling()
    val methodName = methodNameNode.getText()

    // Process method argument nodes
    var methodArgs : List[IrMethodDeclArg] = List()
    val nonArgs = 3
    val numArgs = ast.getNumberOfChildren() - nonArgs
    var methodArgNode = methodNameNode.getNextSibling() // FIELD_DECL_ARG or BLOCK
    for ( _ <- 0 until numArgs) {
      val methodArgType = typeNodeToIrType(methodArgNode.getFirstChild(), exceptionGenie)
      val methodArgName = methodArgNode.getFirstChild().getNextSibling().getText()
      methodArgs :+= IrMethodDeclArg(methodArgType, methodArgName, nodeLoc)
      methodArgNode = methodArgNode.getNextSibling()
    }

    // Process block node
    val bodyBlockNode = methodArgNode
    val bodyBlock = blockNodeToIrBlock(bodyBlockNode, exceptionGenie)

    return IrMethodDecl(methodType, methodName, methodArgs, bodyBlock, nodeLoc)
  }

  // Expects an AST rooted at a Token.TYPE node
  def typeNodeToIrType(ast: AST, exceptionGenie: ExceptionGenie) : IrType = {
    val child = ast.getFirstChild()
    val nodeLoc = new NodeLocation(child.getLine(), child.getColumn())

    child.getType() match {
      case Token.INT => return IrIntType(nodeLoc)
      case Token.BOOLEAN => return IrBoolType(nodeLoc)
      case _ => {
        // Should not reach here unless someone misused this method
        exceptionGenie.insert(new ConvertingToIrTypeException("Error converting to IrType", nodeLoc))
        return null
      }
    }
  }

  // Expects an AST rooted at a Token.BLOCK node
  def blockNodeToIrBlock(ast: AST, exceptionGenie: ExceptionGenie) : IrBlock = {
    var fieldDecls : List[IrFieldDecl] = List()
    var stmts : List[IrStatement] = List()

    val numChildren = ast.getNumberOfChildren()
    var child = ast.getFirstChild()
    for ( _ <- 0 until numChildren) {
      child.getType() match {
        case Token.FIELD_DECL => fieldDecls :+= fieldDeclNodeToIrFieldDecl(child, exceptionGenie)
        case Token.STATEMENT => stmts :+= statementNodeToIrStatement(child, exceptionGenie)
        case _ => {
          // This should be impossible to reach
          exceptionGenie.insert(new ConvertingToBlockIrException("Error converting block child node", new NodeLocation(0,0)))
        }
      }
      child = child.getNextSibling()
    }

    return IrBlock(fieldDecls, stmts)
  }

  // Expects an AST rooted at a Token.STATEMENT node
  def statementNodeToIrStatement(ast: AST, exceptionGenie: ExceptionGenie) : IrStatement = {
    var stmt = ast.getFirstChild()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())

    stmt.getType() match {
      case Token.EQ => return assignmentNodeToIrStatement(stmt, exceptionGenie)
      case Token.PLUSEQ => return assignmentNodeToIrStatement(stmt, exceptionGenie)
      case Token.MINUSEQ => return assignmentNodeToIrStatement(stmt, exceptionGenie)
      case Token.METHOD_CALL => return IrMethodCallStmt(methodCallNodeToIrExpression(stmt, exceptionGenie), nodeLoc)
      case Token.IF => return ifNodeToIrStatement(stmt, exceptionGenie)
      case Token.FOR => return forNodeToIrStatement(stmt, exceptionGenie)
      case Token.WHILE => return whileNodeToIrStatement(stmt, exceptionGenie)
      case Token.RETURN => return returnNodeToIrStatement(stmt, exceptionGenie)
      case Token.BREAK => return IrBreakStmt(nodeLoc)
      case Token.CONTINUE => return IrContinueStmt(nodeLoc)
      case _ => {
        // Should not reach here unless someone misused this method
        exceptionGenie.insert(new ConvertingToStatementException("Error converting statement node", nodeLoc))
        return null
      }
    }
  }

  // Expects AST rooted at a Token.EQ, Token.PLUSEQ, or Token.MINUSEQ node
  def assignmentNodeToIrStatement(ast: AST, exceptionGenie: ExceptionGenie) : IrAssignStmt = {
    val locNode = ast.getFirstChild()
    val loc = locationNodeToIrLocation(locNode, exceptionGenie)
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())
    val exprNode = locNode.getNextSibling()
    val expr = exprNodeToIrExpression(exprNode, exceptionGenie)

    ast.getType() match {
      case Token.EQ => return IrEqualsAssignStmt(loc, expr, nodeLoc)
      case Token.PLUSEQ => return IrPlusAssignStmt(loc, expr, nodeLoc)
      case Token.MINUSEQ => return IrMinusAssignStmt(loc, expr, nodeLoc)
      case _ => {
        exceptionGenie.insert(new ConvertingToAssignmentException("Error converting assignment node", nodeLoc))
        return null
      }
    }
  }

  // Expects AST rooted at a Token.LOCATION node
  def locationNodeToIrLocation(ast: AST, exceptionGenie : ExceptionGenie) : IrLocation = {
    val child = ast.getFirstChild()
    child.getType() match {
      case Token.ID => {
        val nodeLoc = new NodeLocation(child.getLine(), child.getColumn())
        return IrSingleLocation(child.getText(), nodeLoc)
      }
      case Token.ARRAY_ACCESS => {
        val idNode = child.getFirstChild()
        val exprNode = idNode.getNextSibling()
        val nodeLoc = new NodeLocation(idNode.getLine(), idNode.getColumn())
        return IrArrayLocation(idNode.getText(), exprNodeToIrExpression(exprNode, exceptionGenie), nodeLoc)
      }
      case _ => {
        // It should be impossible to reach this
        exceptionGenie.insert(new ConvertingToVariableLocException("Error converting location node", new NodeLocation(0,0)));
        return null
      }
    }
  }

  // Expects AST rooted at a Token.METHOD_CALL node
  def methodCallNodeToIrExpression(ast: AST, exceptionGenie: ExceptionGenie) : IrCallExpr = {
    val numChild = ast.getNumberOfChildren()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())

    val methodNameNode = ast.getFirstChild()
    val methodName = methodNameNode.getFirstChild().getText()

    var args : List[IrCallArg] = List()

    val numArgs = numChild - 1

    // This initial conversion does not attempt to determine if we are performing
    // a method call or a callout call. This will need to be determined
    // on the semantic pass through. TODO
    var argNode = methodNameNode.getNextSibling()
    for ( _ <- 0 until numArgs) {
      argNode.getType() match {
        case Token.EXPR => args :+= IrCallExprArg(exprNodeToIrExpression(argNode, exceptionGenie), nodeLoc)
        case Token.STRING_LITERAL => {
          var text = argNode.getText()
          text = text.substring(1, text.length - 1) // trim off double quotes
          val strNodeLoc = new NodeLocation(argNode.getLine(), argNode.getColumn())
          args :+= IrCallStringArg(IrStringLiteral(text, strNodeLoc), nodeLoc)
        }
        case _ => println("Error converting method call argument node")
      }
      argNode = argNode.getNextSibling()
    }

    return IrMethodCallExpr(methodName, args, nodeLoc)
  }

  // Expects AST rooted at a Token.IF node
  def ifNodeToIrStatement(ast: AST, exceptionGenie: ExceptionGenie) : IrIfStmt = {
    val numChild = ast.getNumberOfChildren()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())

    val condNode = ast.getFirstChild()
    val condExpr = exprNodeToIrExpression(condNode, exceptionGenie)

    val ifBlockNode = condNode.getNextSibling()
    val ifBlock = blockNodeToIrBlock(ifBlockNode, exceptionGenie)

    var elseBlock : Option[IrBlock] = None

    if (numChild == 3) {
      val elseBlockNode = ifBlockNode.getNextSibling()
      elseBlock = Option(blockNodeToIrBlock(elseBlockNode, exceptionGenie))
    }

    return IrIfStmt(condExpr, ifBlock, elseBlock, nodeLoc)
  }

  // Expects AST rooted at a Token.FOR node
  def forNodeToIrStatement(ast: AST, exceptionGenie: ExceptionGenie) : IrForStmt = {
    val numChild = ast.getNumberOfChildren()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())

    val iterNode = ast.getFirstChild()
    val iterLoc = IrSingleLocation(iterNode.getText(), nodeLoc)

    val initValNode = iterNode.getNextSibling()
    val initValExpr = exprNodeToIrExpression(initValNode, exceptionGenie)

    val endValNode  = initValNode.getNextSibling()
    val endValExpr = exprNodeToIrExpression(endValNode, exceptionGenie)

    var incVal : Option[IrIntLiteral] = None

    var bodyBlock : IrBlock = IrBlock(List(), List())

    if (numChild == 5) {
      val incValNode = endValNode.getNextSibling()
      incVal = Option(IrIntLiteral(None, incValNode.getText(), nodeLoc))

      val bodyBlockNode = incValNode.getNextSibling()
      bodyBlock = blockNodeToIrBlock(bodyBlockNode, exceptionGenie)
    } else {
      val bodyBlockNode = endValNode.getNextSibling()
      bodyBlock = blockNodeToIrBlock(bodyBlockNode, exceptionGenie)
    }

    return IrForStmt(iterLoc, initValExpr, endValExpr, incVal, bodyBlock, nodeLoc)
  }

  // Expects AST rooted at a Token.WHILE node
  def whileNodeToIrStatement(ast: AST, exceptionGenie: ExceptionGenie) : IrWhileStmt = {
    val exprNode = ast.getFirstChild()
    val expr = exprNodeToIrExpression(exprNode, exceptionGenie)
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())
    val bodyBlockNode = exprNode.getNextSibling()
    val bodyBlock = blockNodeToIrBlock(bodyBlockNode, exceptionGenie )

    return IrWhileStmt(expr, bodyBlock, nodeLoc)
  }


  // Expects AST rooted at a Token.RETURN node
  def returnNodeToIrStatement(ast : AST, exceptionGenie: ExceptionGenie) : IrReturnStmt = {
    val numChild = ast.getNumberOfChildren()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())
    if (numChild == 0) {
      return IrReturnStmt(None, nodeLoc)
    } else if (numChild == 1) {
      return IrReturnStmt(Option(exprNodeToIrExpression(ast.getFirstChild(), exceptionGenie)), nodeLoc)
    } else {
      exceptionGenie.insert(new ConvertingReturnTokenException("Error converting return node", nodeLoc))
      return null
    }
  }

  // Expects AST rooted at a Token.EXPR
  def exprNodeToIrExpression(ast: AST, exceptionGenie: ExceptionGenie) : IrExpression = {
    val child = ast.getFirstChild()
    val nodeLoc = new NodeLocation(child.getLine(), child.getColumn())
    child.getType() match {
      case Token.QUESTION => {
        val condNode = child.getFirstChild()
        val condExpr = exprNodeToIrExpression(makeChildOfExprNode(condNode), exceptionGenie)
        val leftNode = condNode.getNextSibling()
        val leftExpr = exprNodeToIrExpression(leftNode, exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(rightNode, exceptionGenie)
        return IrTernOpExpr(condExpr, leftExpr, rightExpr, nodeLoc)
      }
      case Token.OR => {
      val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrOrOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.AND => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrAndOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.EQ_OP => {
        val eqOp = {
          child.getText() match {
            case "==" => IrEqualOp()
            case "!=" => IrNotEqualOp()
          }
        }
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(eqOp, leftExpr, rightExpr, nodeLoc)
      }
      case Token.REL_OP => {
        val relOp = {
          child.getText() match {
            case "<=" => IrLteOp()
            case ">=" => IrGteOp()
            case "<" => IrLtOp()
            case ">" => IrGtOp()
          }
        }
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(relOp, leftExpr, rightExpr, nodeLoc)
      }
      case Token.PLUS => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrAddOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.MINUS => {
        val numChild = child.getNumberOfChildren()
        numChild match {
          // Unary minus
          case 1 => {
            val node = child.getFirstChild()
            val expr = exprNodeToIrExpression(makeChildOfExprNode(node), exceptionGenie)

            expr match {
              case IrIntLiteral(v, r, l) => {
                v match {
                  case Some(i) => {
                    // Preserve all the unary minuses in representation
                    return IrIntLiteral(Some(-i), "-" + r, nodeLoc)
                  }
                  case None => {return IrUnOpExpr(IrMinusOp(), expr, nodeLoc)}
                }
              }
              case _ => {return IrUnOpExpr(IrMinusOp(), expr, nodeLoc)}
            }
          }
          // Binary minus
          case 2 => {
            val leftNode = child.getFirstChild()
            val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
            val rightNode = leftNode.getNextSibling()
            val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
            return IrBinOpExpr(IrSubOp(), leftExpr, rightExpr, nodeLoc)
          }
        }
      }
      case Token.MUL => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrMulOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.DIV => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrDivOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.MOD => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode), exceptionGenie)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode), exceptionGenie)
        return IrBinOpExpr(IrModOp(), leftExpr, rightExpr, nodeLoc)
      }
      case Token.NOT => {
        val node = child.getFirstChild()
        val expr = exprNodeToIrExpression(makeChildOfExprNode(node), exceptionGenie)
        return IrUnOpExpr(IrNotOp(), expr, nodeLoc)
      }
      case Token.AT => {
        val exprNode = child.getFirstChild()
        val expr = exprNodeToIrExpression(exprNode, exceptionGenie)
        return IrUnOpExpr(IrArraySizeOp(), expr, nodeLoc)
      }
      case Token.METHOD_CALL => return methodCallNodeToIrExpression(child, exceptionGenie)
      case Token.LOCATION => return locationNodeToIrLocation(child, exceptionGenie)
      case Token.LITERAL => return literalNodeToIrExpression(child, exceptionGenie)
      case Token.EXPR => return exprNodeToIrExpression(child, exceptionGenie)
      case _ => {
        // It should be impossible to reach this
        exceptionGenie.insert(new ConvertingExprTokenException("Error converting expr node", nodeLoc))
        return null
      }
    }
  }

  def getBigIntValue(s : String) : BigInt = {
      if (s.size > 2 && s.substring(0, 2) == "0x") {
        return BigInt(s.substring(2), 16)
      } else {
        return BigInt(s)
      }
  }

  // Expects AST rooted at a Token.LITERAL node
  def literalNodeToIrExpression(ast: AST, exceptionGenie: ExceptionGenie) : IrLiteral = {
    val child = ast.getFirstChild()
    val nodeLoc = new NodeLocation(child.getLine(), child.getColumn())
    child.getType() match {
      case Token.INT_LITERAL => {
        var intVal = None: Option[BigInt];
        try {
          intVal = Some(getBigIntValue(child.getText()))
        } catch {
          case nfa: NumberFormatException => {
            exceptionGenie.insert(new InvalidIntLiteralException("Cannot parse int literal", nodeLoc))
          }
        }
        return IrIntLiteral(intVal, child.getText(), nodeLoc)
      }
      case Token.CHAR_LITERAL => return charLiteralNodeToIrExpression(child, exceptionGenie)
      case Token.TRUE => return IrBooleanLiteral(true, nodeLoc)
      case Token.FALSE => return IrBooleanLiteral(false, nodeLoc)
      case _ => {
        exceptionGenie.insert(new ConvertingLiteralTokenException("Error converting literal node", nodeLoc))
        return null
      }
    }
  }

  // Expects AST rooted at a Token.CHAR_LITERAL node
  def charLiteralNodeToIrExpression(ast: AST, exceptionGenie: ExceptionGenie) : IrCharLiteral = {
    var text = ast.getText()
    val nodeLoc = new NodeLocation(ast.getLine(), ast.getColumn())
    text = text.substring(1, text.length - 1) // trim off single quotes
    text.length match {
      // Normal character
      case 1 => return IrCharLiteral(text.toCharArray()(0), nodeLoc)
      // Escaped character
      case 2 => {
        text match {
          case "\\\"" => return IrCharLiteral('"', nodeLoc)
          case "\\'" => return IrCharLiteral('\'', nodeLoc)
          case "\\\\" => return IrCharLiteral('\\', nodeLoc)
          case "\\t" => return IrCharLiteral('\t', nodeLoc)
          case "\\n" => return IrCharLiteral('\n', nodeLoc)
        }
      }
      case _ => {
        exceptionGenie.insert(new ConvertingCharLiteralTokenException("Error converting char literal node", nodeLoc))
        return null
      }
    }
  }

  class myAST(line: Int, col: Int) extends TokenAST {
    override def getLine = {line}
    override def getColumn = {col}
  }

  // This returns an expr node with the input ast as its only child
  def makeChildOfExprNode(ast: AST) : AST = {
    val parent = new myAST(ast.getLine(), ast.getColumn())
    parent.setType(Token.EXPR)
    parent.setText("expr")
    parent.addChild(ast)
    return parent
  }
}
