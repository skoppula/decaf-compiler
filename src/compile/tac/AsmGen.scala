package compile.tac

import compile.Ir._
import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._

class AsmGen{
  def asmGen(t: Tac) : List[String] = {
    // Notes from Austin:
    t match {
      case tac:TacBinOp => {
        return binOpToAsm(tac)
      }
      case tac:TacUnaryOp => {
        return unOpToAsm(tac)
      }
      case tac:TacIfFalse => {
        return List()
      }
      case tac:TacLabel => {
        return List()
      }
      case tac:TacCopy => {
        return List()
      }
      case tac:TacMethodCall => {
        return List()
      }
      case tac:TacExprArray => {
        return List()
      }
    }
  }

  def binOpToAsm(t: TacBinOp) : List[String] = { // TODO
    val (addr1, addr2, op, addr3) = (t.addr1, t.addr2, t.op, t.addr3)

    op match {
      // Match arith ops
      case ADD => { // TODO
        return List()
      }
      case SUB => { // TODO
        return List()
      }
      case MULT => { // TODO
        return List()
      }
      case DIV => { // TODO
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

  def unaryOpToAsm(t: TacUnaryOp) : List[String] = { // TODO
    val (addr1, op, addr2) = (t.addr1, t.op, t.addr2)

    op match {
      // Match arith ops
      case SIZE => { // TODO
        return List()
      }
      case MINUS => { // TODO
        return List()
      }
      case NOT => { // TODO
        return List()
      }
    }
  }

  def labelToAsm(t: TacLabel) : List[String] = {
    return List(t.label + ":\n")
  }

  def copyToAsm(t: TacCopy) : List[String] = { //TODO
    val (addr1, addr2) = (t.addr1, t.addr2)
    // 1. get %rbp offset of addr 1 and addr 2
    // 2. load from addr2 into some register
    // 3. movq into addr1

    return List()
  }
//TacMethodCall(addr1: String, method: String, args: List[IrCallArg])
  def methodCallToAsm(t: TacMethodCall) : List[String] = { // TODO
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

  def exprArrayToAsm(t: TacExprArray) : List[String] = { // TODO
    val (addr1, addr2, index) = (t.addr1, t.addr2, t.index)
    // 1. Get rbp offset of addr1
    // 2. Get rbp offset of addr2
    // 3. Calculate rbp offset of array element, new_offset = addr2_offset - index*8
    //    (Runtime errors)?
    // 4. movq new_offset(%rbp) temp_register
    // 5. movq temp_register addr1_offset(%rbp)

    return List()
  }

}

/*
  case object ADD extends OpEnumVal // x = y + z, etc.
  case object SUB extends OpEnumVal
  case object MULT extends OpEnumVal
  case object DIV extends OpEnumVal

  case object SIZE extends OpEnumVal // x = @y
  case object MINUS extends OpEnumVal // x = -y  (Note this is unary minus, not subtraction)
  case object NOT extends OpEnumVal // x = !y
  
  case object AND extends OpEnumVal // x = y && z, etc.
  case object OR extends OpEnumVal

  case object LT extends OpEnumVal // x = y < z, etc.
  case object LTE extends OpEnumVal
  case object GT extends OpEnumVal
  case object GTE extends OpEnumVal

  case object EQ extends OpEnumVal // x = y == z, etc.
  case object NEQ extends OpEnumVal
 */


/*
object ThreeAddressCode {
  abstract class Tac{}
  case class TacBinOp(addr1: String, addr2: String, op: OpTypes.OpEnumVal, addr3: String) extends Tac{} // addr1 = addr2 op addr3
  case class TacUnaryOp(addr1: String, op: OpTypes.OpEnumVal, addr2: String) extends Tac{} // addr1 = op addr2
  case class TacIfFalse(addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label
  case class TacLabel(label: String) extends Tac{} // foo:
  case class TacCopy(addr1: String, addr2: String) extends Tac{} // x = y
  case class TacMethodCall(addr1: String, method: String, args: List[IrCallArg]) extends Tac{} // x = foo(args*)
  case class TacExprArray(addr1: String, addr2: String, index: String) extends Tac{} // x = y[index] (index is a temp variable as well)
}
 */
