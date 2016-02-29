package compile

import _root_.util.CLI
import java.io._
import compile.symboltables.{MethodsTable, GlobalFieldTable, SymbolTable}

import scala.Console
import compile.util.GraphUtil.walkExperimental
import scala.collection.mutable.Set
import scala.collection.mutable.Stack

// Begin parser/scanner imports
import antlr.CommonAST
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
    *Parse the file specified by the filename. Eventually, this method
    *may return a type specific to your compiler.
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

  def addNewScope(scopeStack : Stack[SymbolTable]) {
    // TODO
  }

  def addNewMethod(methodName : String, scopeStack : Stack[SymbolTable], methodsTable: MethodsTable): Unit = {
    // TODO
  }

  def inter(fileName: String): (CommonAST, SymbolTable) = {
    /**
      * Create the intermediate AST representation + symbol table structures
      * Where all the identifier and semantic checking magic will happen
      */

    /**
      * Step 1: Traverse tree and create AST
      * Step 2: (catch errors on any one of these steps)
      *   a. Step through AST and note callouts
      *   b. When first field declaration starts and store global field declarations
      *   c. When first method declaration happens, start scope/local symbol table stack and method table
      *       - Check that array bounds 0 < x < size of array
      *       -
      *      Stop parsing AST when after processing main method
      * Step 3: Run any other validation checks
      */

    // Step 1

    // Step 2.a.
    var calloutSet : Set[String] = Set.empty[String]

    // Step 2.b.
    val globalFieldTable : GlobalFieldTable = new GlobalFieldTable("the global field table")

    // Step 2.c.
    val methodsTable : MethodsTable = new MethodsTable
    var scopeStack = Stack.empty[SymbolTable]

    // Step Three
    methodsTable.validate()
    globalFieldTable.validate()


    /*******************************************************************/

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



}
