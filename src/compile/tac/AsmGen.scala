package compile.tac

import compile.Ir._
import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._
import compile.symboltables.{SymbolTable}

import scala.collection.mutable


// Austin Note:
// From the 6.035 x86-64 architecture guide
// For the code generation phase of the project you will not be performing register allocation.  You should use %r10 and %r11 for temporary values that you load from the stack.
// For consistency, for any asm instruction, format the string as 
// "\t%s\t%s, %s\n".format(op, arg1, arg2)
// or "\t%s\t%s\n".format(op, arg)
// I chose this format because that's how gcc spits out the instructions.

class AsmGen{
  def asmGen(tac: Tac, table: SymbolTable) : List[String] = {
    tac match {
      case t:TacBinOp => { // TODO
        return binOpToAsm(t, table)
      }
      case t:TacUnOp => { // TODO
        return unaryOpToAsm(t, table)
      }
      case t:TacIf => { // TODO
        return ifToAsm(t, table)
      }
      case t:TacIfFalse => { // TODO
        return ifFalseToAsm(t, table)
      }
      case t:TacGoto => { 
        return gotoToAsm(t, table)
      }
      case t:TacLabel => {
        return labelToAsm(t, table)
      }
      case t:TacCopy => { // TODO
        return copyToAsm(t, table)
      }
      case t:TacMethodCallExpr => { // TODO
        return methodCallToAsm(t, table)
      }
      case t:TacArrayRight => { // TODO
        return exprArrayToAsm(t, table)
      }
    }
  }

  def binOpToAsm(t: TacBinOp, table: SymbolTable) : List[String] = { // TODO
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
      case SUB => { // TODO
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("subq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("movq", "%r11", addr1asm)

        asmCommands.toList
      }
      case MULT => { // TODO
        val addr1asm = addrToAsm(addr1,table)
        val addr2asm = addrToAsm(addr2,table)
        val addr3asm = addrToAsm(addr3,table)

        var asmCommands = new mutable.ListBuffer[String]()

        asmCommands += "\t%s\t%s, %s\n".format("movq", addr2asm, "%r10")
        asmCommands += "\t%s\t%s, %s\n".format("movq", addr3asm, "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("addq", "%r10", "%r11")
        asmCommands += "\t%s\t%s, %s\n".format("imulq", "%r11", addr1asm)

        asmCommands.toList
      }
      case DIV => { // TODO
        // Take care here
        // idiv divisor <-> Divide rdx:rax by divisor. Store quotient in rax and store remainder in rdx.
        return List()
      }
      // Match cond ops 
      case AND => { // TODO
        return List()
      }
      case OR => { // TODO
        return List()
      }
      // Match rel ops
      case LT => { // TODO
        return List()
      }
      case LTE => { // TODO
        return List()
      }
      case GT => { // TODO
        return List()
      }
      case GTE => { // TODO
        return List()
      }
      // Match eq ops
      case EQ => { // TODO
        return List()
      }
      case NEQ => { // TODO
        return List()
      }
    }
  }

  def unaryOpToAsm(t: TacUnOp, table: SymbolTable) : List[String] = { // TODO
    val (addr1, op, addr2) = (t.addr1, t.op, t.addr2)
    // addr1 = op addr2
    op match {
      // Match arith ops
      case SIZE => { // TODO
        // 1. Get ArrayTypeDescriptor from lookupID(addr2)
        // 2. Get x = ArrayTypeDescriptor.sizeBytes / TypeDescriptor.sizeBytes
        // 3. Get offset of addr1
        // 3. movq $(x.toString) [offset1](%rbp)
        return List()
      }
      case MINUS => { // TODO
        // 1. Lookup rbp offsets for addr1, addr2
        // 2. movq [offset2](%rbp) %r10
        // 3. movq $0 %r11
        // 4. subq %r10 %r11
        // alternatively, negq %r10
        // 5. movq %r11 [offset1](%rbp)
        return List()
      }
      case NOT => { // TODO
        // 1. Lookup rbp offset for addr1, addr2
        // 2. movq [offset2](%rbp) %r10
        // 3. notq %r10
        // 4. andq $1 %r10
        // 5. movq %r10 [offset1](%rbp)
        return List()
      }
    }
  }

  def ifToAsm(t: TacIf, table: SymbolTable) : List[String] = { // TODO
    // if addr1 goto label
    val (addr1, label) = (t.addr1, t.label)

    return List()
  }

  def ifFalseToAsm(t: TacIfFalse, table: SymbolTable) : List[String] = { // TODO
   // ifFalse addr1 goto label
    val (addr1, label) = (t.addr1, t.label)

    return List()
  }

  def gotoToAsm(t: TacGoto, table: SymbolTable) : List[String] = {
    return List("\t%s\t%s\n".format("jmp", t.label))
  }

  def labelToAsm(t: TacLabel, table: SymbolTable) : List[String] = {
    return List(t.label + ":\n")
  }

//  //ret doesn't change, but we may want this for consistency
//  def retToAsm(t: TacRet, table: SymbolTable) : List[String] = {
//    return List("ret")
//  }

  def copyToAsm(t: TacCopy, table: SymbolTable) : List[String] = { //TODO
    val (addr1, addr2) = (t.addr1, t.addr2)
    // 1. get %rbp offset of addr 1 and addr 2
    // 2. load from addr2 into some register
    // 3. movq into addr1

    return List()
  }

  def methodCallToAsm(t: TacMethodCallExpr, table: SymbolTable) : List[String] = { // TODO
    val (addr1, method, args) = (t.addr1, t.method, t.args)
    // 1. Get rbp offset of addr1
    // 2. For each arg in args (iterating from the end to the beginning),
    //    get rbp offset of arg
    //    if arg # < 7:
    //       movq (offset)rbp into corresponding register
    //    else:
    //       movq (offsetrbp) into temp register
    //       pushq temp register
    // 3. If method has variable number of args (callouts only?)
    //       movq $0 %rax
    // 4. call method
    // 5. movq %rax [location of addr1](%rbp)
    return List()
  }

  def exprArrayToAsm(t: TacArrayRight, table: SymbolTable) : List[String] = { // TODO
    val (addr1, addr2, index) = (t.addr1, t.addr2, t.index)
    // x = y[index] (index is a temp variable as well)
    // 1. Get rbp offset of addr1
    // 2. Get rbp offset of addr2
    // 3. Get rbp offset of index
    // 4. movq [index_offset](%rbp) index_temp_register
    // 5. imul $8 index_temp_register
    // 3. Calculate rbp offset of array element, new_offset = addr2_offset - index_temp_register
    //    (Runtime errors)?
    // 4. movq new_offset(%rbp) temp_register
    // 5. movq temp_register addr1_offset(%rbp)

    return List()
  }

  def addrToAsm(name: String, table: SymbolTable) : String = {
    // TODO
    // This will take care of whether or not we're trying to reference
    // A global variable or a local variable.
    return ""
  }

}
