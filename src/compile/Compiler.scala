package compile

import _root_.util.CLI
import java.io._
import compile.Ir._
import sext._
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}

import scala.Console
import compile.util.GraphUtil.{walkExperimental, visualize}
import scala.collection.mutable

// Begin parser/scanner imports
import antlr.ASTFactory
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

  def parse(fileName: String): TokenAST  = {
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
      parser.setASTNodeClass("compile.TokenAST")
      parser.program()
      val t = parser.getAST().asInstanceOf[TokenAST]

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

  def inter(fileName: String): (TokenAST, SymbolTable) = {
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

    val exceptionGenie : ExceptionGenie = new ExceptionGenie

    // Step 1
    val ir = IrConstruction.constructIR(parse(fileName), exceptionGenie)
    println("\nIR decomposition:"); println(ir.treeString); println()

    // Step 2.a.
    // Toy example using callout manager
    val calloutManager : CalloutManager = new CalloutManager(exceptionGenie)
    for(callout <- ir.calloutDecls) {
      try {
        calloutManager.addCallout(callout.name)
      } catch {
        case cae: CalloutAlreadyExistsException => {
          exceptionGenie.insert(new CalloutAlreadyExistsWithLocException("Callout already exists", callout.nodeLoc))
        }
        case iva: InvalidCalloutException => {
          exceptionGenie.insert(new InvalidCalloutWithLocException("Callout in wrong place", callout.nodeLoc))
        }
      }
    }

    println(); println(calloutManager); println();

    // Step 2.b.
    calloutManager.closeCallouts
    val globalFieldTable : GlobalFieldTable = new GlobalFieldTable("the global field table")

    // Iterates over each field declaration statement
    for(fieldDecl <- ir.fieldDecls) {
      var isInt: Boolean = false;
      if (fieldDecl.fieldType.isInstanceOf[IrVoidType])
        exceptionGenie.insert(new VoidCannotBeDeclarationTypeException("void can't be declaration type", fieldDecl.loc))
      else {
        isInt = fieldDecl.fieldType.isInstanceOf[IrIntType]
      }

      // Iterates over each variable in the declaration line
      for (field <- fieldDecl.fields) {
        try {
          if (field.isInstanceOf[IrSingleFieldDecl]) {
            if (isInt)
              globalFieldTable.insert(field.asInstanceOf[IrSingleFieldDecl].name, new IntTypeDescriptor())
            else
              globalFieldTable.insert(field.asInstanceOf[IrSingleFieldDecl].name, new BoolTypeDescriptor())
          } else {
            if (isInt) {
              val intArr = field.asInstanceOf[IrArrayFieldDecl]
              val arrSize = intArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved :(", intArr.loc))
              globalFieldTable.insert(intArr.name, new IntArrayTypeDescriptor(arrSize))
            } else {
              val boolArr = field.asInstanceOf[IrArrayFieldDecl]
              val arrSize = boolArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved :(", boolArr.loc))
              globalFieldTable.insert(boolArr.name, new BoolArrayTypeDescriptor(arrSize))
            }
          }

        } catch {
          case iae: IdentifierAlreadyExistsException => {
            exceptionGenie.insert(new IdentifierAlreadyExistsWithLocException("Global field already exists", field.nodeLoc))
          }
        }
      }
    }

    // Step 2.c.
    val methodsTable : MethodsTable = new MethodsTable
    var scopeStack = mutable.Stack.empty[SymbolTable]
    var currMethodDescriptor : MethodDescriptor = null;

    // Step Three
    methodsTable.validate()
    globalFieldTable.validate()

    /*******************************************************************/


    val ast = Option(parse(fileName));

    ast match {
      case Some(a) => {
        // walkExperimental(a, funcPre, funcPost, "");
        return (a, null);
      }
      case None => {}
    }
    return (null, null)
  }



}
