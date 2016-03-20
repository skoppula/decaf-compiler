package compile.tac

import compile.Ir._
import compile.descriptors.{MethodDescriptor}

object ThreeAddressCode {
  abstract class Tac{}
 
  case class TacProgramEnter(id: Int) extends Tac{} // indicates the beginning of a program to cue allocating globals

  case class TacBinOp(id: Int, addr1: String, addr2: String, op: OpTypes.BinOpEnumVal, addr3: String) extends Tac{} // addr1 = addr2 op addr3
  case class TacUnOp(id: Int, addr1: String, op: OpTypes.UnOpEnumVal, addr2: String) extends Tac{} // addr1 = op addr2
  
  case class TacIf(id: Int, addr1: String, label: String) extends Tac{} // if addr1 goto label
  case class TacIfFalse(id: Int, addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label

  case class TacGoto(id: Int, label: String) extends Tac{} // goto label
  case class TacGlobl(id: Int, name: String) extends Tac{} // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
  case class TacLabel(id: Int, label: String) extends Tac{} // foo:

  case class TacCopy(id: Int, addr1: String, addr2: String) extends Tac{} // x = y
  case class TacCopyInt(id: Int, addr1: String, int: Int) extends Tac{} // x = 5
  case class TacCopyBoolean(id: Int, addr1: String, bool: Boolean) extends Tac{} // x = true

  case class TacMethodEnter(id: Int, methodDesc: MethodDescriptor) extends Tac{} // indicates beginning of method stack

  case class TacMethodCallExpr(id: Int, addr1: String, method: String, args: List[String]) extends Tac{} // x = foo(args*)
  case class TacMethodCallStmt(id: Int, method: String, args: List[String]) extends Tac{} // foo(args*) (statement)

  case class TacStringLiteral(id: Int, label: String, value: String) extends Tac{} // printf("asdf") --> .L1: .string "asdf"

  case class TacReturnValue(id: Int, addr1: String) extends Tac{} // addr1 is the temp variable where the return value is stored
  case class TacReturn(id: Int) extends Tac{} // indicator to leave, ret the method call

  case class TacArrayLeft(id: Int, addr1: String, addr2: String, index: String) extends Tac{} // x = y[index] (index is a temp variable as well)
  case class TacArrayRight(id: Int, addr1: String, index: String, addr2: String) extends Tac{} // x[index] = y 
}
