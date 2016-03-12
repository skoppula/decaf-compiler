package compile

import _root_.util.CLI
import java.io._
import compile.Ir._
import sext._
import compile.descriptors._
import compile.symboltables.{ParametersTable, MethodsTable, GlobalFieldTable, SymbolTable}
import compile.Check._

import scala.Console
import scala.collection.mutable

// Begin parser/scanner imports
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
    } else if (CLI.target == CLI.Action.CONSTRUCT) {
      if(inter(CLI.infile) == null) {
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

  def inter(fileName: String): (IrProgram, SymbolTable, MethodsTable) = {
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
    if(CLI.irdebug) {
      println("\nIR decomposition:")
      println(ir.treeString)
      println()
    }

    // Step 2.a.
    // Insert callouts into the callout manager
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

    if(CLI.irdebug) {
      println()
      println(calloutManager)
    }

    // Step 2.b.
    // Adds field declaration statements to the global field table
    calloutManager.closeCallouts
    val globalFieldTable : GlobalFieldTable = new GlobalFieldTable
    insertFieldDecls(ir.fieldDecls, globalFieldTable, exceptionGenie)
    if(CLI.irdebug) {
      println()
      println(globalFieldTable)
      println()
    }

    // Step 2.c.
    // Process all the defined methods
    val methodsTable : MethodsTable = new MethodsTable(calloutManager)
    var scopeStack = new mutable.Stack[SymbolTable]

    for(methodDecl <- ir.methodDecls) {
      walkMethodIRNode(calloutManager, globalFieldTable, scopeStack, methodsTable, methodDecl, exceptionGenie)
    }

    if(CLI.irdebug) {
      println(methodsTable)
      println()
    }

    // Step Three
    // Any remaining semantic checks/validation:
    //  - Checks if main() method with correct signature is present
    val mainMethodDescriptor = methodsTable.lookupID("main")
    if(mainMethodDescriptor == null) {
      exceptionGenie.insert(new NoMainMethodException("You don't have a main() method! >:("))
    }

    if(mainMethodDescriptor.getParamTable.size != 0) {
      exceptionGenie.insert(new MainMethodHasParametersException("Your main() method has parameters! Not allowed! >:("))
    }

    return (ir, globalFieldTable, methodsTable)
  }

  def insertFieldDecls(fieldDecls: List[IrFieldDecl], symbolTable : SymbolTable, exceptionGenie: ExceptionGenie)= {
    for(fieldDecl <- fieldDecls) {
      var isInt: Boolean = false;
      if (fieldDecl.fieldType.isInstanceOf[IrVoidType])
        exceptionGenie.insert(new VoidCannotBeDeclarationTypeException("void can't be declaration type", fieldDecl.loc))
      else {
        isInt = fieldDecl.fieldType.isInstanceOf[IrIntType]
      }

      // Iterates over each variable in the declaration line
      var fieldName : String = null;
      for (field <- fieldDecl.fields) {
        try {
          if (field.isInstanceOf[IrSingleFieldDecl]) {
            fieldName = field.asInstanceOf[IrSingleFieldDecl].name;
            if (isInt)
              symbolTable.insert(fieldName, new IntTypeDescriptor())
            else
              symbolTable.insert(fieldName, new BoolTypeDescriptor())
          } else {
            if (isInt) {
              val intArr = field.asInstanceOf[IrArrayFieldDecl]
              fieldName = intArr.name
              val arrSize = intArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved :(", intArr.loc))
              symbolTable.insert(fieldName, new IntArrayTypeDescriptor(arrSize))
            } else {
              val boolArr = field.asInstanceOf[IrArrayFieldDecl]
              fieldName = boolArr.name
              val arrSize = boolArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved :(", boolArr.loc))
              symbolTable.insert(fieldName, new BoolArrayTypeDescriptor(arrSize))
            }
          }

        } catch {
          case iae: IdentifierAlreadyExistsException => {
            exceptionGenie.insert(new IdentifierAlreadyExistsWithLocException("Field already exists: " + fieldName, field.nodeLoc))
          }
        }
      }
    }
  }

  def walkMethodIRNode(
                        calloutManager: CalloutManager,
                        globalFieldTable : GlobalFieldTable,
                        scopeStack : mutable.Stack[SymbolTable],
                        methodsTable: MethodsTable,
                        methodDecl: IrMethodDecl,
                        exceptionGenie : ExceptionGenie
                      ) {
    // Check return type
    var returnType : PrimitiveBaseDescriptor = null;
    if(methodDecl.methodType.isInstanceOf[IrIntType]) {
      returnType = new IntTypeDescriptor;
    } else if(methodDecl.methodType.isInstanceOf[IrBoolType]) {
      returnType = new BoolTypeDescriptor;
    } else if(methodDecl.methodType.isInstanceOf[IrVoidType]) {
      returnType = new VoidTypeDescriptor;
    }

    // Get method name
    val methodName = methodDecl.name

    // Get method location
    val methodLoc = methodDecl.loc

    // Get the method args
    val parametersMap = new mutable.LinkedHashMap[String, PrimitiveBaseDescriptor]
    for(arg <- methodDecl.args) {
      if(arg.argType.isInstanceOf[IrVoidType]) {
        exceptionGenie.insert(new MethodParameterCannotBeVoidException("Method parameter cannot be void", methodLoc))
      } else {
        if(parametersMap.contains(arg.name)) {
          exceptionGenie.insert(new DuplicateParameterNameException("Duplicate parameter name " + arg.name, methodLoc))
        }
        if(arg.argType.isInstanceOf[IrIntType]) {
          parametersMap.put(arg.name, new IntTypeDescriptor)
        } else {
          parametersMap.put(arg.name, new BoolTypeDescriptor)
        }
      }
    }

    // Set up ParametersTable and MethodDescriptor
    val parametersTable = new ParametersTable(globalFieldTable, parametersMap)
    val currMethodDescriptor : MethodDescriptor = new MethodDescriptor(parametersTable, methodName, returnType);

    if(CLI.irdebug) {
      println(methodName + ": " + parametersTable);
    }

    // Add to methods table
    try {
      methodsTable.insert(methodName, currMethodDescriptor)
    } catch {
      case mae: MethodAlreadyExistsException => {
        exceptionGenie.insert(new MethodAlreadyExistsException("Method with name already exists: " + methodName))
      }
    }

    // Start analyzing method body
    scopeStack.push(parametersTable)
    enterBlock(methodsTable, scopeStack, methodDecl.bodyBlock, methodName, exceptionGenie)
  }

  // TODO
  //    negative integer literals
  //    ensure that method has return statement
  //    array locations are not valid for loop index variables
  //      declare var inside for loop?
  //    convert char to int?
  //    pretty print data structure
  //    documentation
  //
  //    hidden tests: illegal 4, 24, 53, method_shadowing2
  def enterBlock(
                methodsTable: MethodsTable,
                scopeStack : mutable.Stack[SymbolTable],
                block : IrBlock,
                topMethodName: String,
                exceptionGenie: ExceptionGenie
                ) {

    insertFieldDecls(block.fieldDecls, scopeStack.top, exceptionGenie)

    val currScope = scopeStack.top

    for(stmt <- block.stmts) {
      checkStmt(methodsTable, scopeStack, stmt, topMethodName, exceptionGenie)
      stmt match {
        case s: IrIfStmt => {
          val newScope : SymbolTable = new SymbolTable(currScope, ScopeTypes.IF)
          scopeStack.push(newScope)
          enterBlock(methodsTable, scopeStack, s.ifBlock, topMethodName, exceptionGenie)
          scopeStack.pop()
          if(s.elseBlock.isDefined) {
            val newScope : SymbolTable = new SymbolTable(currScope, ScopeTypes.IF)
            scopeStack.push(newScope)
            enterBlock(methodsTable, scopeStack, s.elseBlock.get, topMethodName, exceptionGenie)
            scopeStack.pop()
          }
        }
        case s: IrForStmt => {
          val newScope : SymbolTable = new SymbolTable(currScope, ScopeTypes.FOR)
          scopeStack.push(newScope)
          enterBlock(methodsTable, scopeStack, s.bodyBlock, topMethodName, exceptionGenie)
          scopeStack.pop()
        }
        case s: IrWhileStmt => {
          val newScope : SymbolTable = new SymbolTable(currScope, ScopeTypes.WHILE)
          scopeStack.push(newScope)
          enterBlock(methodsTable, scopeStack, s.bodyBlock, topMethodName, exceptionGenie)
          scopeStack.pop()
        }

        case _ => ;
      }
    }
  }
}
