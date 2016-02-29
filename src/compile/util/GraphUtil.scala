package compile.util

import antlr.CommonAST
import antlr.debug.misc.ASTFrame
import edu.mit.compilers.grammar.DecafParser

object GraphUtil {

  def lookupTokenType(i : Int): String = {
    return DecafParser._tokenNames(i)
  }

  def visualize(ast : CommonAST) {
    val frame : ASTFrame = new ASTFrame("AST JTree Example", ast);
    frame.setVisible(true);
    Thread.sleep(5000)
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
}
