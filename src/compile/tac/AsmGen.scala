package compile.tac

import compile.Ir._
import compile.exceptionhandling.{CompilerProblemNoLocation, CompilerProblem}
import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._
import compile.symboltables.{ParametersTable, GlobalFieldTable, SymbolTable}
import scala.collection.mutable
import compile.descriptors._

import scala.collection.mutable.ListBuffer

// From the 6.035 x86-64 architecture guide
// For the code generation phase of the project you will not be performing register allocation.  You should use %r10 and %r11 for temporary values that you load from the stack.
// For consistency, for any asm instruction, format the string as 
// "\t%s\t%s, %s\n".format(op, arg1, arg2)
// or "\t%s\t%s\n".format(op, arg)
// I chose this format because that's how gcc spits out the instructions.

object AsmGen{
  def asmGen(tac: Tac, table: SymbolTable) : List[String] = {
    tac match {
      case t:TacProgramEnter => { 
        return programEnterToAsm(t, table.asInstanceOf[GlobalFieldTable])
      }
      case t:TacBinOp => { 
        return binOpToAsm(t, table)
      }
      case t:TacUnOp => { 
        return unaryOpToAsm(t, table)
      }
      case t:TacIf => { 
        return ifToAsm(t, table)
      }
      case t:TacIfFalse => { 
        return ifFalseToAsm(t, table)
      }
      case t:TacGoto => { 
        return gotoToAsm(t, table)
      }
      case t:TacGlobl => { 
        return globlToAsm(t, table)
      }
      case t:TacLabel => { 
        return labelToAsm(t, table)
      }
      case t:TacCopy => { 
        return copyToAsm(t, table)
      }
      case t:TacCopyInt => { 
        return copyIntToAsm(t, table)
      }
      case t:TacCopyBoolean => { 
        return copyBooleanToAsm(t, table)
      }
      case t:TacMethodEnter => {
        return methodEnterToAsm(t, table)
      }
      case t:TacMethodCallExpr => { 
        return methodCallExprToAsm(t, table)
      }
      case t:TacMethodCallStmt => { 
        return methodCallStmtToAsm(t, table)
      }
      case t:TacStringLiteralStart => {
        return List("\t.section\t.rodata\n")
      }
      case t:TacStringLiteral => { 
        return stringLiteralToAsm(t, table)
      }
      case t:TacStringLiteralEnd => {
        return List("\t.text\n")
      }
      case t:TacReturnValue => { 
        return returnValueToAsm(t, table)
      }
      case t:TacReturn => { 
        return returnToAsm(t, table)
      }
      case t:TacSystemExit => { 
        return systemExitToAsm(t, table)
      }
      case t:TacArrayLeft => { 
        return arrayLeftToAsm(t, table)
      }
      case t:TacArrayRight => { 
        return arrayRightToAsm(t, table)
      }
      case t:TacNop => {
        if(t.comment.isEmpty) {
          return List("\n")
        } else {
          return List("; " + t.comment + "\n")
        }
      }
    }
  }

  def programEnterToAsm(t: TacProgramEnter, table: GlobalFieldTable) : List[String] = {
    // This is where allocation for global variables takes place
    
    var instrs : List[String] = List()

    for((name, descriptor) <- table.symbolTableMap) {
      val sizeBytes = descToSizeBytes(descriptor)
      descriptor match {
        case d:PrimitiveBaseDescriptor => {
          // gcc uses alignment 8 for longs
          instrs :+= "\t.comm\t%s,%d,%d\n".format(name,sizeBytes,8)
        }
        case d:ArrayBaseDescriptor => {
          // gcc uses alignment 32 for global arrays
          instrs :+= "\t.comm\t%s,%d,%d\n".format(name,sizeBytes,32)
        }
      }
    }
    return instrs
  }

  def binOpToAsm(t: TacBinOp, table: SymbolTable) : List[String] = {
    val (addr1, addr2, op, addr3) = (t.addr1, t.addr2, t.op, t.addr3)
    // addr1 = addr2 op addr3
    // The general template of these are going to be something as follows:
    // 1. Lookup rbp offsets for addr1, addr2, addr3.
    // 2. movq [offset2](%rbp) %r10
    // 3. movq [offset3](%rbp) %r11
    // 4. asm_op %r10 %r11
    // 5. movq %r11 [offset1](%rbp)
    // You'll likely have to use a few more instructions for the boolean ops
    op match {
      // Match arith ops
      case ADD => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("addq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case SUB => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("subq", "%r11", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r10", addr1asm)

        asmCommands.toList
      }
      case MULT => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("imulq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case DIV => {
        // Take care here
        // idiv divisor <-> Divide rdx:rax by divisor. Store quotient in rax and store remainder in rdx.

        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%rax")
        asmCommands += "\t%s\n".format("cqo")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s\n".format("idivq","%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%rax", addr1asm)

        asmCommands.toList
      }
      case MOD => {
        // Take care here
        // idiv divisor <-> Divide rdx:rax by divisor. Store quotient in rax and store remainder in rdx.

        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%rax")
        asmCommands += "\t%s\n".format("cqo")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s\n".format("idivq","%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%rdx", addr1asm)

        asmCommands.toList
      }
      // Match cond ops 
      case AND => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("andq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case OR => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("orq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      // Match rel ops

      case LT => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r11", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmovlq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case LTE => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r11", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmovleq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case GT => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r11", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmovgq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case GTE => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r11", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmovgeq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      // Match eq ops
      case EQ => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmoveq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case NEQ => {
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("cmpq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$1", "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "$0", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("cmovneq", "%r10", "%r11")

        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
    }
  }

  def unaryOpToAsm(t: TacUnOp, table: SymbolTable) : List[String] = {
    // addr1 = op addr2
    var instrs : List[String] = List()
    val (addr1, op, addr2) = (t.addr1, t.op, t.addr2)
    val dest = addrToAsm(addr1, table)
    val src = addrToAsm(addr2, table)
    val reg = "%r10"

    op match {
      case SIZE => { 
        // 1. Get ArrayTypeDescriptor from lookupID(addr2)
        // 2. Get size = ArrayTypeDescriptor.length
        // 3. movq $size, %r10
        // 4. movq %r10, dest
        
        val descriptor = table.lookupID(addr2)

        descriptor match {
          case d:ArrayBaseDescriptor => {
            val size = "$%d".format(d.length.longValue())
            instrs :+= "\t%s\t%s, %s\n".format("movq", size, reg)
            instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)
            return instrs
          }
          case _ => {
            // Should not reach here
            return null
          }
        }
      }
      case MINUS => { 
        // 1. Lookup rbp offsets for addr1, addr2
        // 2. movq [offset2](%rbp) %r10
        // 3. negq %r10
        // 4. movq %r10 [offset1](%rbp)
        
        instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
        instrs :+= "\t%s\t%s\n".format("negq", reg)
        instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)

        return instrs
      }
      case NOT => { 
        // 1. Lookup rbp offset for addr1, addr2
        // 2. movq [offset2](%rbp) %r10
        // 3. notq %r10
        // 4. andq $1 %r10
        // 5. movq %r10 [offset1](%rbp)
        
        instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
        instrs :+= "\t%s\t%s\n".format("notq", reg)
        instrs :+= "\t%s\t%s, %s\n".format("andq", "$1", reg)
        instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)

        return instrs
      }
    }
  }

  def ifToAsm(t: TacIf, table: SymbolTable) : List[String] = {
    // if addr1 goto label
    // 1. Get  offset of addr1
    // 2. cmp $1 [offset1](%rbp)
    // 3. je label
    
    var instrs : List[String] = List()
    val (addr1, label) = (t.addr1, t.label)
    val dest = addrToAsm(addr1, table)

    instrs :+= "\t%s\t%s, %s\n".format("cmpq", "$1", dest)
    instrs :+= "\t%s\t%s\n".format("je", label)

    return instrs
  }

  def ifFalseToAsm(t: TacIfFalse, table: SymbolTable) : List[String] = {
    // ifFalse addr1 goto label
    // 1. Get offset of addr1
    // 2. cmp $1 [offset1](%rbp)
    // 3. jne label
    
    var instrs : List[String] = List()
    val (addr1, label) = (t.addr1, t.label)
    val dest = addrToAsm(addr1, table)

    instrs :+= "\t%s\t%s, %s\n".format("cmpq", "$1", dest)
    instrs :+= "\t%s\t%s\n".format("jne", label)

    return instrs
  }

  def gotoToAsm(t: TacGoto, table: SymbolTable) : List[String] = {
    // GOTO label
    // 1. jmp label
    var instrs : List[String] = List()

    instrs :+= "\t%s\t%s\n".format("jmp", t.label)

    return instrs
  }

  def globlToAsm(t: TacGlobl, table: SymbolTable) : List[String] = {
    // This is required for main
    // method declaration looks like
    // <tab> .globl main
    // main:
    // <code>
    var instrs : List[String] = List()
    val name = t.name

    instrs :+= "\t.globl %s\n".format(name)

    return instrs
  }

  def labelToAsm(t: TacLabel, table: SymbolTable) : List[String] = {
    // label:
    
    var instrs : List[String] = List()

    instrs :+= "%s:\n".format(t.label)

    return instrs
  }

  def copyToAsm(t: TacCopy, table: SymbolTable) : List[String] = {
    // x = y
    // 1. get %rbp offset of addr 1 and addr 2
    // 2. load from addr2 into some register
    // 3. movq into addr1
    //
    // HOWEVER, if addr2 is actually the name of an array variable
    // We need to instead store the address corresponding to the base
    // of the array.
    // addrToAsm should now handle string literal labels as well
    
    var instrs : List[String] = List()
    val (addr1, addr2) = (t.addr1, t.addr2)
    val dest = addrToAsm(addr1, table)
    val src = addrToAsm(addr2, table)
    val reg = "%r10"

    if (!table.isGlobal(addr2) && table.lookupID(addr2).isInstanceOf[ArrayBaseDescriptor]) {
      // Array pointer handler
      
      instrs :+= "\t%s\t%s, %s\n".format("leaq", src, reg)
    } else {
      instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
    }
    instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)

    return instrs
  }

  def copyIntToAsm(t: TacCopyInt, table: SymbolTable) : List[String] = {
    // x = 5
    // 1. get %rbp offset of addr1
    // 2. movq $int offset(%rbp)
    
    var instrs : List[String] = List()
    val (addr1, int) = (t.addr1, t.int)
    val dest = addrToAsm(addr1, table)
    val src = "$%d".format(int)
    val reg = "%r10"

    instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
    instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)

    return instrs
  }

  def copyBooleanToAsm(t: TacCopyBoolean, table: SymbolTable) : List[String] = {
    // x = true
    // 1. get %rbp offset of addr1
    // 2. if true, src = "$1" else "$0"
    // 3. movq src offset(%rbp)
    
    var instrs : List[String] = List()
    val (addr1, bool) = (t.addr1, t.bool)
    val dest = addrToAsm(addr1, table)
    val src = if (bool) "$1" else "$0" 

    instrs :+= "\t%s\t%s, %s\n".format("movq", src, dest)

    return instrs
  }


  def methodEnterToAsm(t: TacMethodEnter, table: SymbolTable) : List[String] = {

    if(!table.isInstanceOf[ParametersTable]) {
      throw new CompilerProblemNoLocation("Expected parameter table, but only got symbol table (while creating method enter ASM)")
    }

    var methodParamTable = table.asInstanceOf[ParametersTable]
    var instrs : List[String] = List()
    val methodDesc = t.methodDesc
    val reg = "%r10"

    // Allocate space on the stack
    instrs :+= "\tenter\t$(%d), $0\n".format(methodDesc.getTotalByteSize)

    // Put every passed argument into a corresponding local var on stack
    val params : List[String] = methodParamTable.getParamMap.keys.toList
    params.zipWithIndex.reverse foreach {
      case (addr, index) => {
        val src = pullArgNumToAsm(index+1)
        val dest = addrToAsm(addr, table)
        instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
        instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)
      }
    }

    return instrs
  }


  def methodCallExprToAsm(t: TacMethodCallExpr, table: SymbolTable) : List[String] = {
    // x = foo(args*)
    // 1. Use methodCallStmtToAsm to generate all the initial method call related instructions
    // 2. movq %rax [location of addr1](%rbp)
    
    var instrs : List[String] = List()
    val (addr1, method, args) = (t.addr1, t.method, t.args)
    val dest = addrToAsm(addr1, table)

    instrs = methodCallStmtToAsm(new TacMethodCallStmt(-1, method, args), table) //TODO: check that this -1 doesn't break anything
    instrs :+= "\t%s\t%s, %s\n".format("movq", "%rax", dest)

    return instrs
  }

  def methodCallStmtToAsm(t: TacMethodCallStmt, table: SymbolTable) : List[String] = {
    // foo(args*)
    // 1. Get rbp offset of addr1
    // 2. For each arg in args (iterating from the end to the beginning),
    //    get rbp offset of arg
    //    if arg # < 7:
    //       movq (offset)rbp into corresponding register
    //    else:
    //       movq (offsetrbp) into temp register
    //       pushq temp register or movq temp_register stack_location
    // 3. If method has variable number of args (callouts only?)
    //       movq $0 %rax
    // 4. call method
    // Same as methodCallExprToAsm excepting steps 1 and 5
    
    var instrs : List[String] = List()
    val (method, args) = (t.method, t.args)
    val reg = "%r10"

    val primitiveTypeSize = 8
    if (args.length > 6) { // Allocate space on stack for args
      instrs :+= "\t%s\t$%d, %s\n".format("subq", (args.length - 6)*primitiveTypeSize, "%rsp")
    }

    args.zipWithIndex.reverse foreach {
      case (addr, index) => {
        val src = addrToAsm(addr, table)
        val dest = pushArgNumToAsm(index+1)
        instrs :+= "\t%s\t%s, %s\n".format("movq", src, reg)
        instrs :+= "\t%s\t%s, %s\n".format("movq", reg, dest)
      }
    }

    instrs :+= "\t%s\t%s, %s\n".format("movq", "$0", "%rax") // Disable args through SSE regs
    instrs :+= "\t%s\t%s\n".format("call", method)

    if (args.length > 6) { // Deallocate space on stack for args
      instrs :+= "\t%s\t$%d, %s\n".format("addq", (args.length - 6)*primitiveTypeSize, "%rsp")
    }


    return instrs
  }

  def stringLiteralToAsm(t: TacStringLiteral, table: SymbolTable) : List[String] = {
    var instrs : List[String] = List()
    val (label, value) = (t.label, t.value)

    instrs :+= "%s:\n".format(label)
    instrs :+= "\t.string \"%s\"\n".format(value)

    return instrs
  }

  def returnValueToAsm(t: TacReturnValue, table: SymbolTable) : List[String] = {
    // addr1 is the variable where the return value is stored
    
    var instrs : List[String] = List()
    val addr1 = t.addr1
    val src = addrToAsm(addr1, table)
    val dest = "%rax"

    instrs :+= "\t%s\t%s, %s\n".format("movq", src, dest)

    return instrs
  }

  def returnToAsm(t: TacReturn, table: SymbolTable) : List[String] = {
    // indicator to leave, ret the method call
    
    var instrs : List[String] = List()

    instrs :+= "\tleave\n"
    instrs :+= "\tret\n"

    return instrs
  }

  def systemExitToAsm(t: TacSystemExit, table:SymbolTable) : List[String] = {
    var instrs : List[String] = List()
    val signal = t.signal;

    instrs :+= "\t%s\t$%s, %s\n".format("movq", signal, "%rdi")
    instrs :+= "\t%s\t%s\n".format("call", "exit")

    return instrs
  }

  def arrayRightToAsm(t: TacArrayRight, table: SymbolTable) : List[String] = {
    // x = y[index] (index is a temp variable as well)
    
    var instrs : List[String] = List()
    val (addr1, addr2, index) = (t.addr1, t.addr2, t.index)
    val dest = addrToAsm(addr1, table)
    val indexReg = "%r10"
    val tempReg = "%r11"
    val memAddrIndex = addrToAsm(index, table)
    val src = arrayAddrToAsm(addr2, indexReg, table)

    instrs :+= "\t%s\t%s, %s\n".format("movq", memAddrIndex, indexReg)
    instrs :+= "\t%s\t%s, %s\n".format("movq", src, tempReg)
    instrs :+= "\t%s\t%s, %s\n".format("movq", tempReg, dest)

    return instrs
  }

  def arrayLeftToAsm(t: TacArrayLeft, table: SymbolTable) : List[String] = {
    // x[index] = y (index is a temp variable as well)
    
    var instrs : List[String] = List()
    val (addr1, index, addr2) = (t.addr1, t.index, t.addr2)
    val src = addrToAsm(addr2, table)
    val indexReg = "%r10"
    val tempReg = "%r11"
    val memAddrIndex = addrToAsm(index, table)
    val dest = arrayAddrToAsm(addr1, indexReg, table)

    instrs :+= "\t%s\t%s, %s\n".format("movq", src, tempReg)
    instrs :+= "\t%s\t%s, %s\n".format("movq", memAddrIndex, indexReg)
    instrs :+= "\t%s\t%s, %s\n".format("movq", tempReg, dest)

    return instrs
  }

  def addrToAsm(name: String, table: SymbolTable) : String = {
    /* A global variable reference will be of the form: name(%rip)
     * A local variable reference will be of the form: offset(%rbp)
     * A string literal label with prefix .L will be of the form: $.L___
     */

    if (name.length >= 2 && name.substring(0,2) == ".L") {
      // String literal handler
      return "$%s".format(name)
    } else {
      if (table.isGlobal(name)) {
        table.lookupID(name) match {
          case d:PrimitiveBaseDescriptor => {
            return "%s(%%rip)".format(name)
          }
          case d:ArrayBaseDescriptor => {
            return "$%s".format(name)
          }
        }
      } else { // This handles array args fine, but need to use leaq instruction
        val offset = table.lookupID(name).offsetBytes
        return "%d(%%rbp)".format(offset)
      }
    }
  }


  def arrayAddrToAsm(name: String, indexReg: String, table: SymbolTable) : String = {
    /* A global array variable reference will be of the form: name(,indexReg,8)
     * A local array variable reference will be of the form: offset(%rbp,indexReg,8)
     */
    
    val primitiveTypeSize = 8
    if (table.isGlobal(name)) {
      return "%s(,%s,%d)".format(name, indexReg, primitiveTypeSize)
    } else {
      val offset = table.lookupID(name).offsetBytes
      return "%d(%%rbp,%s,%d)".format(offset, indexReg, primitiveTypeSize)
    }
  }

  def pushArgNumToAsm(argNum: Int) : String = {
    // argNum must be positive
    val primitiveTypeSize = 8
    argNum match {
      case 1 => {
        return "%rdi"
      }
      case 2 => {
        return "%rsi"
      }
      case 3 => {
        return "%rdx"
      }
      case 4 => {
        return "%rcx"
      }
      case 5 => {
        return "%r8"
      }
      case 6 => {
        return "%r9"
      }
      case 7 => {
        return "(%rsp)"
      }
      case _ => {
        return "%d(%%rsp)".format((argNum-7)*primitiveTypeSize)
      }
    }
  }

  def pullArgNumToAsm(argNum: Int) : String = {
    // argNum must be positive
    val primitiveTypeSize = 8
    argNum match {
      case 1 => {
        return "%rdi"
      }
      case 2 => {
        return "%rsi"
      }
      case 3 => {
        return "%rdx"
      }
      case 4 => {
        return "%rcx"
      }
      case 5 => {
        return "%r8"
      }
      case 6 => {
        return "%r9"
      }
      case _ => {
        return "%d(%%rbp)".format(16 + (argNum-7)*primitiveTypeSize)
      }
    }
  }

  def descToSizeBytes(desc: BaseDescriptor) : Int = {
    desc match {
      case d:IntTypeDescriptor => {
        return d.sizeBytes
      }
      case d:BoolTypeDescriptor => {
        return d.sizeBytes
      }
      case d:VoidTypeDescriptor => {
        return 0 // Should not reach
      }
      case d:IntArrayTypeDescriptor => {
        return d.sizeBytes
      }
      case d:BoolArrayTypeDescriptor => {
        return d.sizeBytes
      }
    }

  }

}
