package compile.tac

// refers to the op in three-address code, generally of the form x = y op z
object OpTypes {
  sealed trait OpEnumVal
  
  abstract class BinOpEnumVal extends OpEnumVal
  case object ADD extends OpEnumVal // x = y + z, etc.
  case object SUB extends OpEnumVal
  case object MULT extends OpEnumVal
  case object DIV extends OpEnumVal
  
  case object AND extends OpEnumVal // x = y && z, etc.
  case object OR extends OpEnumVal

  case object LT extends OpEnumVal // x = y < z, etc.
  case object LTE extends OpEnumVal
  case object GT extends OpEnumVal
  case object GTE extends OpEnumVal

  case object EQ extends OpEnumVal // x = y == z, etc.
  case object NEQ extends OpEnumVal

  abstract class UnOpEnumVal extends OpEnumVal

  case object SIZE extends OpEnumVal // x = @y
  case object MINUS extends OpEnumVal // x = -y  (Note this is unary minus, not subtraction)
  case object NOT extends OpEnumVal // x = !y
}
