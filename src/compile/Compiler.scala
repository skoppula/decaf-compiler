package compile

import _root_.util.CLI
import java.io._
import compile.cfg.{CFGUtil, CFGGen, NormalBB}
import compile.exceptionhandling._
import sext._

import compile.Ir._
import compile.descriptors._
import compile.symboltables._
import compile.IrChecks._

import compile.tac._
import compile.tac.ThreeAddressCode._
import TacGen._

import compile.util.Util.dprint
import compile.util.Util.dprintln

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
    } else if (CLI.target == CLI.Action.INTER) {
      val (program, y) = inter(CLI.infile) 
      if(program == null) {
        System.exit(1)
      } else {
        System.exit(0)
      }
    } else if(CLI.target == CLI.Action.ASSEMBLY) {
      val (program, symboltables) = inter(CLI.infile)
      assembly(program, symboltables)
    }
  }


  def assembly(program : IrProgram, methodsTable : MethodsTable): Unit = {
    val tempGenie : TempVariableGenie = new TempVariableGenie

    var asmStr : String = ""

    var dot : List[String] = List()

    if(CLI.tacgen) {
      val tacAsmMap = gen(program, tempGenie, methodsTable)
      for (list <- tacAsmMap.values) {
        asmStr += list mkString "" + "\n"
      }

      if(CLI.debug) {
        // SymbolTableUtil.printSymbolTableStructure(methodsTable)
        for(tac <- tacAsmMap.keys) {
          dprintln(tac + "\n")
        }
      }
    } else {
      dprintln("Using the new CFG method!")
      dprintln("Generating CFG...")
      val (programStartBB, methodsBBMap) = CFGGen.genCFG(program, tempGenie, methodsTable)

      dprintln("Converting CFG to a TAC list...")
      var tacs : List[(Tac, SymbolTable)] = CFGUtil.cfgToTacs(programStartBB, List())
      for((methodStartBB, methodEndBB) <- methodsBBMap.valuesIterator) {
        tacs = tacs ::: CFGUtil.cfgToTacs(methodStartBB, List())
      }

      dprintln("Finished creation of TAC list. TACs:")
      for((tac,st) <- tacs) {
        dprintln(tac + "\n")
      }
      dprintln("Generating assembly...")

      asmStr += CFGUtil.tacsToAsm(tacs) mkString ""

      // === Dot file generation end ===
      var map : Map[String,Set[String]] = CFGUtil.cfgToMap(programStartBB, List())
      for((methodStartBB, methodEndBB) <- methodsBBMap.valuesIterator) {
        map = CFGUtil.mergeMaps(map,CFGUtil.cfgToMap(methodStartBB, List()))
      }
      dot = CFGUtil.mapToDot(map)
      // === Dot file generation end ===
      // Should compile dot file into png using
      // dot -Tpng ~/graph.dot -o ~/graph.png
    }
    if (CLI.outfile == null || CLI.outfile.isEmpty) {
      if (CLI.dot) {
        outFile.println(dot.mkString)
      } else {
        outFile.println(asmStr)
      }
    } else {
      val pw = new PrintWriter(new File(CLI.outfile))
      if (CLI.dot) {
        pw.write(dot.mkString)
      } else {
        pw.write(asmStr)
      }
      pw.close
    }


  }

  def scan(fileName: String) {
    try {
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      scanner.setTrace(CLI.debug && CLI.target == CLI.Action.SCAN)
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

      parser.setTrace(CLI.debug && CLI.target == CLI.Action.PARSE)
      parser.setASTNodeClass("compile.TokenAST")
      parser.program()
      val t = parser.getAST().asInstanceOf[TokenAST]

      if (parser.getError) {
        print("[ERROR] Parse failed\n")
        return null
      } else if (CLI.debug && CLI.target == CLI.Action.PARSE){
        print(t.toStringList)
      }
      return t
    } catch {
      case e: Exception => Console.err.println(CLI.infile + " " + e)
      null
    } 
  }

  def inter(fileName: String): (IrProgram, MethodsTable) = {
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
      *
      * Step 3: Run any other validation checks
      *       - (check 3) 1 main() -> validate() in MethodsTable
      */

    val exceptionGenie : ExceptionGenie = new ExceptionGenie

    // Step 1
    val ir = IrConstruction.constructIR(parse(fileName), exceptionGenie)
    if(CLI.irdebug && CLI.target == CLI.Action.INTER) {
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

    // Step 2.b.
    // Adds field declaration statements to the global field table
    calloutManager.closeCallouts
    val globalFieldTable : GlobalFieldTable = new GlobalFieldTable
    insertFieldDecls(ir.fieldDecls, globalFieldTable, exceptionGenie)

    // Step 2.c.
    // Process all the defined methods
    val methodsTable : MethodsTable = new MethodsTable(calloutManager, globalFieldTable)
    val scopeStack = new mutable.Stack[SymbolTable]

    for(methodDecl <- ir.methodDecls) {
      walkMethodIRNode(calloutManager, globalFieldTable, scopeStack, methodsTable, methodDecl, exceptionGenie)
    }

    // Step Three
    // Any remaining semantic checks/validation:
    //  - Checks if main() method with correct signature is present
    val mainMethodDescriptor = methodsTable.lookupID("main")
    if(mainMethodDescriptor == null) {
      exceptionGenie.insert(new NoMainMethodException("The program contains no main() method"))
    }

    if(mainMethodDescriptor.getParamMap.nonEmpty) {
      exceptionGenie.insert(new MainMethodHasParametersException("The main() method cannot take arguments"))
    }
    
    if(CLI.irdebug && CLI.target == CLI.Action.INTER) {
      SymbolTableUtil.printSymbolTableStructure(methodsTable)
    }
    return (ir, methodsTable)
  }
  
  def insertFieldDecls(fieldDecls: List[IrFieldDecl], symbolTable : SymbolTable, exceptionGenie: ExceptionGenie)= {
    for(fieldDecl <- fieldDecls) {
      var isInt: Boolean = false;
      if (fieldDecl.fieldType.isInstanceOf[IrVoidType])
        exceptionGenie.insert(new VoidCannotBeDeclarationTypeException("A variable cannot be declared as void type", fieldDecl.loc))
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
              val arrSize = intArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved", intArr.loc))
              symbolTable.insert(fieldName, new IntArrayTypeDescriptor(arrSize))
            } else {
              val boolArr = field.asInstanceOf[IrArrayFieldDecl]
              fieldName = boolArr.name
              val arrSize = boolArr.size.value.getOrElse(throw new InvalidIntLiteralException("int literal had no value saved", boolArr.loc))
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
        exceptionGenie.insert(new MethodParameterCannotBeVoidException("Method parameter cannot be of void type", methodLoc))
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

    // Add to methods table
    try {
      methodsTable.insert(methodName, currMethodDescriptor)
    } catch {
      case mae: MethodAlreadyExistsException => {
        exceptionGenie.insert(mae)
      }
      case iae: IdentifierAlreadyExistsException => {
        exceptionGenie.insert(iae)
      }
      case cae: CalloutAlreadyExistsException => {
        exceptionGenie.insert(cae)
      }
    }

    // Start analyzing method body
    scopeStack.push(parametersTable)
    enterBlock(methodsTable, scopeStack, methodDecl.bodyBlock, methodName, exceptionGenie)
  }

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
