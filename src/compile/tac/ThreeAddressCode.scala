package compile.tac

import compile.Ir._

abstract class ThreeAddressCode{}

object ThreeAddressCode {
  abstract class Tac{}
 
  case class TacProgramEnter() extends Tac{} // indicates the beginning of a program to cue allocating globals

  case class TacBinOp(addr1: String, addr2: String, op: OpTypes.BinOpEnumVal, addr3: String) extends Tac{} // addr1 = addr2 op addr3
  case class TacUnOp(addr1: String, op: OpTypes.UnOpEnumVal, addr2: String) extends Tac{} // addr1 = op addr2
  
  case class TacIf(addr1: String, label: String) extends Tac{} // if addr1 goto label
  case class TacIfFalse(addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label

  case class TacGoto(label: String) extends Tac{} // goto label
  case class TacLabel(label: String) extends Tac{} // foo:

  case class TacCopy(addr1: String, addr2: String) extends Tac{} // x = y
  case class TacCopyInt(addr1: String, int: Int) extends Tac{} // x = 5
  case class TacCopyBoolean(addr1: String, bool: Boolean) extends Tac{} // x = true

  case class TacMethodEnter() extends Tac{} // indicates beginning of method stack

  case class TacMethodCallExpr(addr1: String, method: String, args: List[IrCallArg]) extends Tac{} // x = foo(args*)
  case class TacMethodCallStmt(method: String, args: List[IrCallArg]) extends Tac{} // foo(args*) (void return type)

  case class TacReturnValue(addr1: String) extends Tac{} // addr1 is the temp variable where the return value is stored
  case class TacReturn() extends Tac{} // indicator to leave, ret the method call

  case class TacArrayLeft(addr1: String, addr2: String, index: String) extends Tac{} // x = y[index] (index is a temp variable as well)
  case class TacArrayRight(addr1: String, index: String, addr2: String) extends Tac{} // x[index] = y 
}
