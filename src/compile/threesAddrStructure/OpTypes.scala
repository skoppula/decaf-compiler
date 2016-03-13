package compile.threesAddrStructure

object OpTypes {
  sealed trait EnumVal
  case object JMP extends EnumVal

  case object ADD extends EnumVal
  case object SUB extends EnumVal
  case object MULT extends EnumVal
  case object DIV extends EnumVal

  case object NOT extends EnumVal
  case object AND extends EnumVal
  case object OR extends EnumVal

  case object LT extends EnumVal
  case object LTE extends EnumVal
  case object GT extends EnumVal
  case object GTE extends EnumVal

  case object EQ extends EnumVal
  case object NEQ extends EnumVal
}
