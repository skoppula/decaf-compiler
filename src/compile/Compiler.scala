package compile
import util.CLI
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.PagedSeq
import java.io._
import scala.io.Source
import scala.collection.mutable.{StringBuilder, ListBuffer}
import scala.Console

// Begin parser/scanner imports
import antlr.CommonAST
import antlr.collections.AST
import antlr.Token
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes, DecafScanner, DecafScannerTokenTypes }

object Compiler {
  val tokenMap = Map(DecafScannerTokenTypes.CHAR_LITERAL -> "CHARLITERAL", DecafScannerTokenTypes.INT_LITERAL -> "INTLITERAL", DecafScannerTokenTypes.TRUE -> "BOOLEANLITERAL", DecafScannerTokenTypes.FALSE -> "BOOLEANLITERAL", DecafScannerTokenTypes.STRING_LITERAL -> "STRINGLITERAL", DecafScannerTokenTypes.ID -> "IDENTIFIER")
  var outFile = if (CLI.outfile == null) Console.out else (new java.io.PrintStream(
    new java.io.FileOutputStream(CLI.outfile)))
  def main(args: Array[String]): Unit = {
    CLI.parse(args, Array[String]());
    if (CLI.target == CLI.Action.SCAN) {
      scan(CLI.infile)
      System.exit(0)
    } else if (CLI.target == CLI.Action.PARSE) {
        if(parse(CLI.infile) == null) {
          System.exit(1)
        }
        System.exit(0)
    } else if (CLI.target == CLI.Action.INTER) {
      if(inter(CLI.infile) == null) {
        System.exit(1)
      }
      System.exit(0)
    }
  }

  def scan(fileName: String) {
    try {
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      scanner.setTrace(CLI.debug)
      var done = false
      while (!done) {
        try {
          val head = scanner.nextToken()
          if (head.getType() == DecafScannerTokenTypes.EOF) {
            done = true
          } else {
            val tokenType = tokenMap.getOrElse(head.getType(), "")
            outFile.println(head.getLine() + (if (tokenType ==  "") "" else " ") + tokenType + " " + head.getText())
          }
        } catch {
          case ex: Exception => {
            Console.err.println(CLI.infile + " " + ex)
            scanner.consume();
          }
        }
      }
    } catch {
      case ex: Exception => Console.err.println(ex)
    }
  }

  def parse(fileName: String): CommonAST  = {
    /** 
    Parse the file specified by the filename. Eventually, this method
    may return a type specific to your compiler.
    */
    var inputStream : java.io.FileInputStream = null
    try {
      inputStream = new java.io.FileInputStream(fileName)
    } catch {
      case f: FileNotFoundException => { Console.err.println("File " + fileName + " does not exist"); return null }
    }
    try {
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      val parser = new DecafParser(scanner)

      parser.setTrace(CLI.debug)
      parser.program()
      val t = parser.getAST().asInstanceOf[CommonAST]
      if (parser.getError()) {
        print("[ERROR] Parse failed\n")
        return null
      } else if (CLI.debug){
        print(t.toStringList())
      }
      return t
    } catch {
      case e: Exception => Console.err.println(CLI.infile + " " + e)
      null
    } 
  }

  def inter(fileName: String): (CommonAST, SymbolTable) = {
    /**
      * Create the intermediate AST representation + symbol table structures
      * Where all the identifier and semantic checking magic will happen
      */
    //val ast: CommonAST = parse(fileName);
    //println("num children: " + ast.getNumberOfChildren());
    //println("root text: \"" +  ast.getText() + "\", type: " + ast.getType());


    //println("first child: " + ast.getFirstChild());
    //println("next sibling: " + ast.getNextSibling());
    //println("next next sibling: " + main.getNextSibling);
    //println("next sibling children: " + main.getFirstChild);
    //println(ast.toStringList());
    //println("")
    //println(ast.toStringTree());
    //println(classOf[DecafParserTokenTypes].getFields);
    //println(DecafParser._tokenNames.deep.mkString(","))


/* This is for the normal walk, but node nesting is not too visible with this
    def funcPre(ast : CommonAST) : Any = {
      println("Entered " + ast.getText())
    }

    def funcPost(ast : CommonAST) : Any = {
      println("Exited " + ast.getText())
    }

    val ast = Option(parse(fileName));

    ast match {
      case Some(a) => {
        walk(a, funcPre, funcPost);
        return (a, null);
      }
      case None => {}
    }

*/

    def funcPre(ast : CommonAST, a : Any) : Any = {
      // Below is unsafe, but we just want a quick print for now.
      val s : String = a.asInstanceOf[String]
      println(s + ">" + "Entered " + ast.getText())
      return (s + ">")
    }

    def funcPost(ast : CommonAST, a : Any) : Any = {
      val s : String = a.asInstanceOf[String]
      println(s + "Exited " + ast.getText())
      return s.substring(0, s.length - 1)
    }

    val ast = Option(parse(fileName));

    ast match {
      case Some(a) => {
        walkExperimental(a, funcPre, funcPost, "");
        return (a, null);
      }
      case None => {}
    }




    return (null, null)
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
