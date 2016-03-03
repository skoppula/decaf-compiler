package compile

import _root_.util.CLI
import java.io._
import compile.descriptors.{MethodDescriptor, BaseDescriptor}
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}

import scala.Console
import compile.util.GraphUtil.{walkExperimental, visualize, constructIR}
import scala.collection.mutable

// Begin parser/scanner imports
import antlr.CommonAST
import edu.mit.compilers.grammar.{ DecafParser, DecafScanner, DecafScannerTokenTypes }

object Compiler {

  val tokenMap = Map(DecafScannerTokenTypes.CHAR_LITERAL -> "CHARLITERAL", DecafScannerTokenTypes.INT_LITERAL -> "INTLITERAL", DecafScannerTokenTypes.TRUE -> "BOOLEANLITERAL", DecafScannerTokenTypes.FALSE -> "BOOLEANLITERAL", DecafScannerTokenTypes.STRING_LITERAL -> "STRINGLITERAL", DecafScannerTokenTypes.ID -> "IDENTIFIER")
  var outFile = if (CLI.outfile == null) Console.out else (new java.io.PrintStream(
    new java.io.FileOutputStream(CLI.outfile)))
  def main(args: Array[String]): Unit = {
    CLI.parse(args, Array[String]())
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
            scanner.consume()
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

  def addNewMethod(
                    scopeStack : mutable.Stack[SymbolTable],
                    methodsTable : MethodsTable,
                    globalFieldTable : GlobalFieldTable,
                    currMethodDescriptor : MethodDescriptor,

                    methodName : String,
                    parameters : mutable.LinkedHashMap[String, BaseDescriptor],
                    returnType : BaseDescriptor,
                    methodInfo : String) {

    val parametersTable : ParametersTable = new ParametersTable(globalFieldTable, methodInfo + ": parameter table", parameters)
    val methodDescriptor = new MethodDescriptor(parametersTable, methodInfo, returnType)
    methodsTable.insert(methodName, methodDescriptor)
    scopeStack.push(parametersTable)
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
      *       (check 1) callout reptition -> CalloutManager handles this
      *
      *   b. When first field declaration starts and store global field declarations
      *
      *   c. When first method declaration happens, start scope/local symbol table stack and method table
      *       - (check 4) decl array size > 0 -> when inserting into symbol table, can check this
      *       - (check 1 and 2) No identifier twice in scope/before declared -> symbol tables/scope stack handle this
      *       - (check 5) signature = arguments -> when descriptor is returned, can check this
      *       - (check 6) if method used as expression, returns result -> when descriptor is returned, can check this
      *       - (check 7) arrays + strings not used as arguments -> we can check our parameter types when creating parameters
      *                                                               LinkedHashMap, args won't match when we
      *                                                               check args==signature (check 5)
      *       - (check 8) return [value] can't be in non-return type -> check this with currMethodDescriptor
      *       - (check 9) return type must match signature -> check this with currMethodDescriptor
      *       - (check 10) var that's array location must exist -> during symbol table retrieval we check this
      *       - (check 11) array must have int location type -> parse whether it's int from AST
      *       - (check 12) @var, var must be array -> retrieve from symbol table, check if array descriptor
      *       - (check 13, 14) if/while/? condition = bool -> parse whether it's bool from AST
      *       - (check 15) ternary result exprs must have same type -> parse whether it's bool from AST
      *       - (check 16, 17, 18, 19, 20) operands of arith_ops/bool_ops/eq_ops/assign have correct type -> parse from AST
      *       - (check 21, 22) for loops contain int in conditions-> parse/typecheck from AST
      *       - (check 23) break and continue are in for/while -> check from SymbolTable at top of scope stack
      *
      * Step 3: Run any other validation checks
      *       - (check 3) 1 main() -> validate() in MethodsTable
      */

    // Step 1
    var ir = constructIR(parse(fileName))
    // Step 2.a.
    // Toy example using callout manager
    val calloutManager : CalloutManager = new CalloutManager
    try {
      calloutManager.addCallout("printf")
    } catch {
      case cae: CalloutAlreadyExistsException => {
        println("Line 2: Callout already exists (oh no)")
        sys.exit(1)
      }
      case iva: InvalidCalloutException => {
        println("Line 2: welp callout in wrong place")
      }
    }


    // Step 2.b.
    calloutManager.closeCallouts
    val globalFieldTable : GlobalFieldTable = new GlobalFieldTable("the global field table")

    // Step 2.c.
    val methodsTable : MethodsTable = new MethodsTable
    var scopeStack = mutable.Stack.empty[SymbolTable]
    var currMethodDescriptor : MethodDescriptor = null;

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
        println(ir)
        return (a, null);
      }
      case None => {}
    }
    return (null, null)
  }



}
