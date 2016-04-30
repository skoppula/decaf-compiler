package compile.tac

import compile.descriptors.{MethodDescriptor}

object ThreeAddressCode {
  abstract class Tac{
    val isAssign : Boolean = false
    val lhs : String = ""
  }
 
  case class TacProgramEnter(id: Int) extends Tac{} // indicates the beginning of a program to cue allocating globals

  case class TacNop(id: Int, comment: String) extends Tac{} // for nop operation, and adds a comment

  case class TacBinOp(id: Int, addr1: String, addr2: String, op: OpTypes.BinOpEnumVal, addr3: String) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // addr1 = addr2 op addr3

  case class TacUnOp(id: Int, addr1: String, op: OpTypes.UnOpEnumVal, addr2: String) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // addr1 = op addr2
  
  case class TacIf(id: Int, addr1: String, label: String) extends Tac{} // if addr1 goto label
  case class TacIfFalse(id: Int, addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label

  case class TacGoto(id: Int, label: String) extends Tac{} // goto label
  case class TacGlobl(id: Int, name: String) extends Tac{} // This is required for main; it exposes the label to the linker, which is needed by gcc to link main to the standard C runtime library
  case class TacLabel(id: Int, label: String) extends Tac{} // foo:

  case class TacCopy(id: Int, addr1: String, addr2: String) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // x = y

  case class TacCopyInt(id: Int, addr1: String, int: Long) extends Tac{} // x = 5
  case class TacCopyBoolean(id: Int, addr1: String, bool: Boolean) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // x = true

  case class TacMethodEnter(id: Int, methodDesc: MethodDescriptor) extends Tac{} // indicates beginning of method stack

  case class TacMethodCallExpr(id: Int, addr1: String, method: String, args: List[String]) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // x = foo(args*)

  case class TacMethodCallStmt(id: Int, method: String, args: List[String]) extends Tac{} // foo(args*) (statement)

  case class TacStringLiteralStart(id: Int) extends Tac{}
  case class TacStringLiteral(id: Int, label: String, value: String) extends Tac{} // printf("asdf") --> .L1: .string "asdf"
  case class TacStringLiteralEnd(id: Int) extends Tac{}

  case class TacReturnValue(id: Int, addr1: String) extends Tac{} // addr1 is the temp variable where the return value is stored
  case class TacReturn(id: Int) extends Tac{} // indicator to leave, ret the method call
  case class TacSystemExit(id: Int, signal: Int) extends Tac{} // indicated exit with signal

  case class TacArrayLeft(id: Int, addr1: String, index: String, addr2: String) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // x[index] = y (index is a temp variable as well)

  case class TacArrayRight(id: Int, addr1: String, addr2: String, index: String) extends Tac{
    override val isAssign = true
    override val lhs = addr1
  } // x = y[index] (index is a temp variable as well)
}
