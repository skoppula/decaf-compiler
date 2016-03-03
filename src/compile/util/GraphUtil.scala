package compile.util

import antlr.CommonAST
import antlr.collections.AST
import antlr.debug.misc.ASTFrame
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes => Token}

import compile.Ir._

object GraphUtil {

  def lookupTokenType(i : Int): String = {
    return DecafParser._tokenNames(i)
  }

  def visualize(ast : CommonAST) {
    val frame : ASTFrame = new ASTFrame("AST JTree Example", ast);
    frame.setVisible(true);
    Thread.sleep(20000)
  }

  def walk(ast : CommonAST, funcPre : CommonAST => Any, funcPost : CommonAST => Any) : Any = {
    /**
      * DFS recursion via walk through the AST. Applies the function funcPreOrder on
      * ast, and then recursively calls walk on the child (if it exists). Then applies
      * the function funcPostOrder on ast and then on the next sibling (if it exists).
      */

    funcPre(ast)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:CommonAST) => walk(c, funcPre, funcPost)
      case None => {}
    }

    funcPost(ast)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:CommonAST) => {
        walk(s, funcPre, funcPost)
      }
      case None => {}
    }
  }

  def walkPreOrder(ast : CommonAST, funcPre : CommonAST => Any) : Any = {
    /**
      * DFS recursion via preorder traversal through the AST. Applies the
      * function funcPreOrder on ast, and then recursively calls walk on the child
      * (if it exists) and then on the next sibling (if it exists)
      */

    funcPre(ast)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:CommonAST) => walkPreOrder(c, funcPre)
      case None => {}
    }

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:CommonAST) => {
        walkPreOrder(s, funcPre)
      }
      case None => {}
    }
  }

  def walkPostOrder(ast : CommonAST, funcPost : CommonAST => Any) : Any = {
    /**
      * DFS recursion via postorder traversal through the AST. Recursively calls
      * walkPostOrder on the child (if it exists). Then applies funcPost to ast.
      * Then recursively calls walkPostOrder on the next sibling (if it exists).
      */

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:CommonAST) => walkPostOrder(c, funcPost)
      case None => {}
    }

    funcPost(ast)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:CommonAST) => {
        walkPostOrder(s, funcPost)
      }
      case None => {}
    }
  }

  def walkExperimental(ast : CommonAST, funcPre : (CommonAST, Any) => Any, funcPost : (CommonAST, Any) => Any, arg : Any) : Any = {
    /**
      * DFS recursion via walk through the AST. Takes preorder and postorder function
      * and and an argument to pass into those functions. Probably should not be used
      * for any actual walking.
      */

    val resultPre = funcPre(ast, arg)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:CommonAST) => walkExperimental(c, funcPre, funcPost, resultPre)
      case None => {}
    }

    val resultPost = funcPost(ast, resultPre)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:CommonAST) => {
        walkExperimental(s, funcPre, funcPost, arg)
      }
      case None => {}
    }

    return resultPost
  }


  def constructIR(ast : CommonAST) : IrProgram = {
    /** Expects CommonAST from ANTLR
      */

    val numChild = ast.getNumberOfChildren()
    var child = ast.getFirstChild()

    var calloutDecls : List[IrCalloutDecl] = List()
    var fieldDecls : List[IrFieldDecl] = List()
    var methodDecls : List[IrMethodDecl] = List()

    for ( _ <- 0 until numChild) {
      child.getType() match {
        case Token.CALLOUT => {
          calloutDecls :+= calloutNodeToIrCalloutDecl(child)
        }
        case Token.FIELD_DECL => {
          fieldDecls :+= fieldDeclNodeToIrFieldDecl(child)
        }
        case Token.METHOD_DECL => {
          methodDecls :+= methodDeclNodeToIrMethodDecl(child)
        }
      }
      child = child.getNextSibling()
    }

    return IrProgram(calloutDecls, fieldDecls, methodDecls)
  }

  // Expects a AST rooted at Token.CALLOUT node
  def calloutNodeToIrCalloutDecl(ast: AST) : IrCalloutDecl = {
    if (ast.getType() != Token.CALLOUT) {
      println("Error: Failed CALLOUT node conversion")
    }

    // Process callout node
    val calloutId = ast.getFirstChild().getText()
    println("Callout Declaration: " + calloutId)
    
    return IrCalloutDecl(calloutId)
  }

  // Expects a AST rooted at Token.FIELD_DECL node
  def fieldDeclNodeToIrFieldDecl(ast: AST) : IrFieldDecl = {
    if (ast.getType() != Token.FIELD_DECL) {
      println("Error: Failed FIELD_DECL node conversion")
    }

    // Process field type node
    val fieldType = typeNodeToIrType(ast.getFirstChild())

    // Process field declaration argument ndoes
    var fieldArgs : List[IrFieldDeclArg] = List()
    val numFields = ast.getNumberOfChildren() - 1
    var fieldArgNode = ast.getFirstChild().getNextSibling() // FIELD_DECL_ARG
    for ( _ <- 0 until numFields) {
      val field = fieldArgNode.getFirstChild() // ID or array_id
      field.getType() match {
        case Token.ID => {
          val fieldName = field.getText()
          fieldArgs :+= IrSingleFieldDecl(fieldName)
        }
        case Token.ARRAY_ID => {
          val arrayFieldName = field.getFirstChild().getText()
          // TODO: On the pass through of the completed IR, the representation of the int literal
          // Needs to be checked and if valid, populates the value field of IrIntLiteral
          val arrayFieldSize = IrIntLiteral(None, field.getFirstChild().getNextSibling().getText())
          fieldArgs :+= IrArrayFieldDecl(arrayFieldName, arrayFieldSize)
        }
        case _ => {
          println("Error: Neither ID nor Array_ID in field declaration argument")
        }
      }
      fieldArgNode = fieldArgNode.getNextSibling()
    }

    println("Field Declaration: type: " + fieldType + " args: " + fieldArgs)

    return IrFieldDecl(fieldType, fieldArgs)
  }

  // Expects an AST rooted atToken.METHOD_DECL node
  def methodDeclNodeToIrMethodDecl(ast: AST) : IrMethodDecl = {
    if (ast.getType() != Token.METHOD_DECL) {
      println("Error: Failed METHOD_DECL node conversion")
    }

    // Process method type node
    val methodTypeNode = ast.getFirstChild()
    val methodType = {
      ast.getFirstChild().getType() match {
        case Token.TYPE => typeNodeToIrType(methodTypeNode)
        case Token.VOID => IrVoidType()
      }
    }

    // Process method name node
    val methodNameNode = methodTypeNode.getNextSibling()
    val methodName = methodNameNode.getText()

    // Process method argument nodes
    var methodArgs : List[IrMethodDeclArg] = List()
    val nonArgs = 3
    val numArgs = ast.getNumberOfChildren() - nonArgs
    var methodArgNode = methodNameNode.getNextSibling() // FIELD_DECL_ARG or BLOCK
    for ( _ <- 0 until numArgs) {
      val methodArgType = typeNodeToIrType(methodArgNode.getFirstChild())
      val methodArgName = methodArgNode.getFirstChild().getNextSibling().getText()
      methodArgs :+= IrMethodDeclArg(methodArgType, methodArgName)
      methodArgNode = methodArgNode.getNextSibling()
    }

    // Process block node
    val bodyBlockNode = methodArgNode
    val bodyBlock = blockNodeToIrBlock(bodyBlockNode)

    return IrMethodDecl(methodType, methodName, methodArgs, bodyBlock)
  }

  // Expects an AST rooted at a Token.TYPE node
  def typeNodeToIrType(ast: AST) : IrType = {
    val child = ast.getFirstChild()
    child.getType() match {
    case Token.INT => return IrIntType()
    case Token.BOOLEAN => return IrBoolType()
    case _ => {println("Conversion to IrType error"); return null}
    }
  }

  // Expects an AST rooted at a Token.BLOCK node
  def blockNodeToIrBlock(ast: AST) : IrBlock = {
    var fieldDecls : List[IrFieldDecl] = List()
    var stmts : List[IrStatement] = List()

    val numChildren = ast.getNumberOfChildren()
    var child = ast.getFirstChild()
   for ( _ <- 0 until numChildren) {
     child.getType() match {
       case Token.FIELD_DECL => fieldDecls :+= fieldDeclNodeToIrFieldDecl(child)
       case Token.STATEMENT => stmts :+= statementNodeToIrStatement(child)
       case _ => println("Error converting block child node")
     }
     child = child.getNextSibling()
    }

    return IrBlock(fieldDecls, stmts)
  }

  // Expects an AST rooted at a Token.STATEMENT node
  def statementNodeToIrStatement(ast: AST) : IrStatement = {
    var stmt = ast.getFirstChild()
    stmt.getType() match {
       case Token.EQ => return assignmentNodeToIrStatement(stmt)
       case Token.PLUSEQ => return assignmentNodeToIrStatement(stmt)
       case Token.MINUSEQ => return assignmentNodeToIrStatement(stmt)
       case Token.METHOD_CALL => return IrMethodCallStmt(methodCallNodeToIrExpression(stmt))
       case Token.IF => return ifNodeToIrStatement(stmt)
       case Token.FOR => return forNodeToIrStatement(stmt)
       case Token.WHILE => return whileNodeToIrStatement(stmt)
       case Token.RETURN => return returnNodeToIrStatement(stmt)
       case Token.BREAK => return IrBreakStmt()
       case Token.CONTINUE => return IrContinueStmt()
       case _ => {println("Error converting statement node"); return null}
    }
  }

  // Expects AST rooted at a Token.EQ, Token.PLUSEQ, or Token.MINUSEQ node
  def assignmentNodeToIrStatement(ast: AST) : IrAssignStmt = {
    val locNode = ast.getFirstChild()
    val loc = locationNodeToIrLocation(locNode)

    val exprNode = locNode.getNextSibling()
    val expr = exprNodeToIrExpression(exprNode)

    ast.getType() match {
      case Token.EQ => return IrEqualsAssignStmt(loc, expr)
      case Token.PLUSEQ => return IrPlusAssignStmt(loc, expr)
      case Token.MINUSEQ => return IrMinusAssignStmt(loc, expr)
      case _ => {println("Error converting assignment node"); return null}
    }
  }

  // Expects AST rooted at a Token.LOCATION node
  def locationNodeToIrLocation(ast: AST) : IrLocation = {
    val child = ast.getFirstChild()
    child.getType() match {
      case Token.ID => return IrSingleLocation(child.getText())
      case Token.ARRAY_ACCESS => {
        val idNode = child.getFirstChild()
        val exprNode = idNode.getNextSibling()
        return IrArrayLocation(idNode.getText(), exprNodeToIrExpression(exprNode))
      }
      case _ => {println("Error converting location node"); return null}
    }
  }

  // Expects AST rooted at a Token.METHOD_CALL node
  def methodCallNodeToIrExpression(ast: AST) : IrCallExpr = {
    val numChild = ast.getNumberOfChildren()

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
        case Token.EXPR => args :+= IrCallExprArg(exprNodeToIrExpression(argNode))
        case Token.STRING_LITERAL => {
          var text = argNode.getText()
          text = text.substring(1, text.length - 1) // trim off double quotes
          args :+= IrCallStringArg(IrStringLiteral(text))
        }
        case _ => println("Error converting method call argument node")
      }
    argNode = argNode.getNextSibling()
    }

    return IrMethodCallExpr(methodName, args)
  }

  // Expects AST rooted at a Token.IF node
  def ifNodeToIrStatement(ast: AST) : IrIfStmt = {
    val numChild = ast.getNumberOfChildren()
    val condNode = ast.getFirstChild()
    val condExpr = exprNodeToIrExpression(condNode)

    val ifBlockNode = condNode.getNextSibling()
    val ifBlock = blockNodeToIrBlock(ifBlockNode)

    var elseBlock : Option[IrBlock] = None

    if (numChild == 3) {
      val elseBlockNode = ifBlockNode.getNextSibling()
      elseBlock = Option(blockNodeToIrBlock(elseBlockNode))
    }

    return IrIfStmt(condExpr, ifBlock, elseBlock)
  }

  // Expects AST rooted at a Token.FOR node
  def forNodeToIrStatement(ast: AST) : IrForStmt = {
    val numChild = ast.getNumberOfChildren()
    
    val iterNode = ast.getFirstChild()
    val iterLoc = IrSingleLocation(iterNode.getText())

    val initValNode = iterNode.getNextSibling()
    val initValExpr = exprNodeToIrExpression(initValNode)

    val endValNode  = initValNode.getNextSibling()
    val endValExpr = exprNodeToIrExpression(endValNode)

    var incVal : Option[IrIntLiteral] = None

    var bodyBlock : IrBlock = IrBlock(List(), List())

    if (numChild == 5) {
      val incValNode = endValNode.getNextSibling()
      // TODO: On the pass through of the completed IR, the representation of the int literal
      // Needs to be checked and if valid, populates the value field of IrIntLiteral
      incVal = Option(IrIntLiteral(None, incValNode.getText()))

      val bodyBlockNode = incValNode.getNextSibling()
      bodyBlock = blockNodeToIrBlock(bodyBlockNode)
    } else {
      val bodyBlockNode = endValNode.getNextSibling()
      bodyBlock = blockNodeToIrBlock(bodyBlockNode)
    }

    return IrForStmt(iterLoc, initValExpr, endValExpr, incVal, bodyBlock)
  }

  // Expects AST rooted at a Token.WHILE node
  def whileNodeToIrStatement(ast: AST) : IrWhileStmt = {
    val exprNode = ast.getFirstChild()
    val expr = exprNodeToIrExpression(exprNode)

    val bodyBlockNode = exprNode.getNextSibling()
    val bodyBlock = blockNodeToIrBlock(bodyBlockNode)

    return IrWhileStmt(expr, bodyBlock)
  }


  // Expects AST rooted at a Token.RETURN node
  def returnNodeToIrStatement(ast : AST) : IrReturnStmt = {
    val numChild = ast.getNumberOfChildren()
    if (numChild == 0) {
      return IrReturnStmt(None)
    } else if (numChild == 1) {
      return IrReturnStmt(Option(exprNodeToIrExpression(ast.getFirstChild())))
    } else {
      println("Error converting return node")
      return null
    }
  }

  // Expects AST rooted at a Token.EXPR
  def exprNodeToIrExpression(ast: AST) : IrExpression = {
    val child = ast.getFirstChild()
    child.getType() match {
      case Token.QUESTION => {
        val condNode = child.getFirstChild()
        val condExpr = exprNodeToIrExpression(makeChildOfExprNode(condNode))
        val leftNode = condNode.getNextSibling()
        val leftExpr = exprNodeToIrExpression(leftNode)
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(rightNode)
        return IrTernOpExpr(condExpr, leftExpr, rightExpr)
      }
      case Token.OR => { //case class          IrBinOpExpr(binOp: IrBinOp, leftExpr: IrExpression, rightExpr: IrExpression) extends IrExpression
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrOrOp(), leftExpr, rightExpr)
      }
      case Token.AND => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrAndOp(), leftExpr, rightExpr)
      }
      case Token.EQ_OP => {
        val eqOp = {
          child.getText() match {
            case "==" => IrEqualOp()
            case "!=" => IrNotEqualOp()
          }
        }
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(eqOp, leftExpr, rightExpr)
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
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(relOp, leftExpr, rightExpr)
      }
      case Token.PLUS => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrAddOp(), leftExpr, rightExpr)
      }
      case Token.MINUS => {
        val numChild = child.getNumberOfChildren()
        numChild match {
          // Unary minus
          case 1 => {
            val node = child.getFirstChild()
            val expr = exprNodeToIrExpression(makeChildOfExprNode(node))
            return IrUnOpExpr(IrMinusOp(), expr)
          }
          // Binary minus
          case 2 => {
            val leftNode = child.getFirstChild()
            val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
            val rightNode = leftNode.getNextSibling()
            val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
            return IrBinOpExpr(IrSubOp(), leftExpr, rightExpr)
            }
        }
      }
      case Token.MUL => {        
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrMulOp(), leftExpr, rightExpr)
      }
      case Token.DIV => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrDivOp(), leftExpr, rightExpr)
      }
      case Token.MOD => {
        val leftNode = child.getFirstChild()
        val leftExpr = exprNodeToIrExpression(makeChildOfExprNode(leftNode))
        val rightNode = leftNode.getNextSibling()
        val rightExpr = exprNodeToIrExpression(makeChildOfExprNode(rightNode))
        return IrBinOpExpr(IrModOp(), leftExpr, rightExpr)
      }
      case Token.NOT => {
        val node = child.getFirstChild()
        val expr = exprNodeToIrExpression(makeChildOfExprNode(node))
        return IrUnOpExpr(IrNotOp(), expr)
      }
      case Token.AT => {
        val exprNode = child.getFirstChild()
        val expr = exprNodeToIrExpression(exprNode)
        return IrUnOpExpr(IrArraySizeOp(), expr)
      }
      case Token.METHOD_CALL => return methodCallNodeToIrExpression(child)
      case Token.LOCATION => return locationNodeToIrLocation(child)
      case Token.LITERAL => return literalNodeToIrExpression(child)
      case Token.EXPR => return exprNodeToIrExpression(child)
      case _ => {println("Error converting expr node " + child.getText()); return null}
    }
  }

  // Expects AST rooted at a Token.LITERAL node
  def literalNodeToIrExpression(ast: AST) : IrLiteral = {
    val child = ast.getFirstChild()

    child.getType() match {
      // TODO: On the pass through of the completed IR, the representation of the int literal
      // Needs to be checked and if valid, populates the value field of IrIntLiteral
      case Token.INT_LITERAL => return IrIntLiteral(None, child.getText())
      case Token.CHAR_LITERAL => return charLiteralNodeToIrExpression(child)
      case Token.TRUE => return IrBooleanLiteral(true)
      case Token.FALSE => return IrBooleanLiteral(false)
      case _ => {println("Error converting literal node"); return null}
    }
  }

  // Expects AST rooted at a Token.CHAR_LITERAL node
  def charLiteralNodeToIrExpression(ast: AST) : IrCharLiteral = {
    var text = ast.getText()
    text = text.substring(1, text.length - 1) // trim off single quotes
    text.length match {
      // Normal character
      case 1 => return IrCharLiteral(text.toCharArray()(0))
      // Escaped character
      case 2 => {
        text match {
          case "\\\"" => return IrCharLiteral('"')
          case "\\'" => return IrCharLiteral('\'')
          case "\\\\" => return IrCharLiteral('\\')
          case "\\t" => return IrCharLiteral('\t')
          case "\\n" => return IrCharLiteral('\n')
        }
      }
      case _ => {println("Error converting char literal node"); return null}
    }
  }

  // This returns an expr node with the input ast as its only child
  def makeChildOfExprNode(ast: AST) : AST = {
    var parent = new CommonAST()
    parent.setType(Token.EXPR)
    parent.setText("expr")
    parent.addChild(ast)
    return parent
  }
}
