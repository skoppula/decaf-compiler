package compile.util

import compile.TokenAST
import antlr.debug.misc.ASTFrame
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes => Token}

object GraphUtil {

  def funcPrePrint(ast : TokenAST, a : Any) : Any = {
    // Below is unsafe, but we just want a quick print for now.
    val s : String = a.asInstanceOf[String]
    println(s + ">" + "Entered " + ast.getText() + " Line: " + ast.getLine() + " Column: " + ast.getColumn())
    return (s + ">")
  }

  def funcPostPrint(ast : TokenAST, a : Any) : Any = {
    val s : String = a.asInstanceOf[String]
    println(s + "Exited " + ast.getText())
    return s.substring(0, s.length - 1)
  }

  def lookupTokenType(i : Int): String = {
    return DecafParser._tokenNames(i)
  }

  def visualize(ast : TokenAST) {
    val frame : ASTFrame = new ASTFrame("AST JTree Example", ast);
    frame.setVisible(true);
    Thread.sleep(20000)
  }

  def walk(ast : TokenAST, funcPre : TokenAST => Any, funcPost : TokenAST => Any) : Any = {
    /**
      * DFS recursion via walk through the AST. Applies the function funcPreOrder on
      * ast, and then recursively calls walk on the child (if it exists). Then applies
      * the function funcPostOrder on ast and then on the next sibling (if it exists).
      */

    funcPre(ast)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:TokenAST) => walk(c, funcPre, funcPost)
      case None => {}
    }

    funcPost(ast)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:TokenAST) => {
        walk(s, funcPre, funcPost)
      }
      case None => {}
    }
  }

  def walkPreOrder(ast : TokenAST, funcPre : TokenAST => Any) : Any = {
    /**
      * DFS recursion via preorder traversal through the AST. Applies the
      * function funcPreOrder on ast, and then recursively calls walk on the child
      * (if it exists) and then on the next sibling (if it exists)
      */

    funcPre(ast)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:TokenAST) => walkPreOrder(c, funcPre)
      case None => {}
    }

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:TokenAST) => {
        walkPreOrder(s, funcPre)
      }
      case None => {}
    }
  }

  def walkPostOrder(ast : TokenAST, funcPost : TokenAST => Any) : Any = {
    /**
      * DFS recursion via postorder traversal through the AST. Recursively calls
      * walkPostOrder on the child (if it exists). Then applies funcPost to ast.
      * Then recursively calls walkPostOrder on the next sibling (if it exists).
      */

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:TokenAST) => walkPostOrder(c, funcPost)
      case None => {}
    }

    funcPost(ast)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:TokenAST) => {
        walkPostOrder(s, funcPost)
      }
      case None => {}
    }
  }

  def walkExperimental(ast : TokenAST, funcPre : (TokenAST, Any) => Any, funcPost : (TokenAST, Any) => Any, arg : Any) : Any = {
    /**
      * DFS recursion via walk through the AST. Takes preorder and postorder function
      * and and an argument to pass into those functions. Probably should not be used
      * for any actual walking.
      */

    val resultPre = funcPre(ast, arg)

    // Recursive call on child
    val child = Option(ast.getFirstChild)
    child match {
      case Some(c:TokenAST) => walkExperimental(c, funcPre, funcPost, resultPre)
      case None => {}
    }

    val resultPost = funcPost(ast, resultPre)

    // recursive call on the next sibling
    var sibling = Option(ast.getNextSibling)
    sibling match {
      case Some(s:TokenAST) => {
        walkExperimental(s, funcPre, funcPost, arg)
      }
      case None => {}
    }

    return resultPost
  }

}
