package compile.tac

class Tac{}

object Tac {
  case class TacBinOp(addr1: String, addr2: String, op: OpTypes.OpEnumVal, addr3: String) extends Tac{} // addr1 = addr2 op addr3
  case class TacUnaryOp(addr1: String, op: OpTypes.OpEnumVal, addr2: String) extends Tac{} // addr1 = op addr2
  case class TacIfFalse(addr1: String, label: String) extends Tac{} // ifFalse addr1 goto label
  case class TacLabel(label: String) extends Tac{} // foo:
  case class TacCopy(addr1: String, addr2: String) extends Tac{} // x = y
}
