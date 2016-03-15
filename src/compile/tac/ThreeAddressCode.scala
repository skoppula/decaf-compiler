package compile.tac

import compile.Ir._

abstract class ThreeAddressCode{}

object ThreeAddressCode {
  abstract class Tac{}
  case class TacBinOp(addr1: String, addr2: String, op: OpTypes.BinOpEnumVal, addr3: String) extends Tac{} // addr1 = addr2 op addr3
  case class TacUnaryOp(addr1: String, op: OpTypes.UnOpEnumVal, addr2: String) extends Tac{} // addr1 = op addr2
  case class TacIf(addr1: String, label: String) extends Tac{} // if
  case class TacIfFalse(addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label
  case class TacGoto(label: String) extends Tac{} // goto label
  case class TacLabel(label: String) extends Tac{} // foo:
  case class TacCopy(addr1: String, addr2: String) extends Tac{} // x = y
  case class TacMethodCall(addr1: String, method: String, args: List[IrCallArg]) extends Tac{} // x = foo(args*)
  case class TacExprArray(addr1: String, addr2: String, index: String) extends Tac{} // x = y[index] (index is a temp variable as well)
}
