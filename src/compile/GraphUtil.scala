package compile

import antlr.CommonAST
import edu.mit.compilers.grammar.DecafParser
import org.antlr.runtime.tree.{CommonTree, DOTTreeGenerator}

object GraphUtil {
  def lookupTokenType(i : Int): String = {
    return DecafParser._tokenNames(i)
  }

  def visualize(graph : CommonAST) {
    var gen : DOTTreeGenerator = new DOTTreeGenerator
  }

  def traverse(graph: CommonAST) {

  }
}
