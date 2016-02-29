package compile

import antlr.CommonAST
import edu.mit.compilers.grammar.DecafParser
import antlr.debug.misc.ASTFrame

object GraphUtil {
  def lookupTokenType(i : Int): String = {
    return DecafParser._tokenNames(i)
  }

  def visualize(ast : CommonAST) {
    val frame : ASTFrame = new ASTFrame("AST JTree Example", ast);
    frame.setVisible(true);
    Thread.sleep(5000)
  }

  def traverse(graph: CommonAST) {

  }
}
