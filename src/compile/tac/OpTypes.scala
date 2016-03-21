package compile.tac

// refers to the op in three-address code, generally of the form x = y op z
object OpTypes {
  sealed trait OpEnumVal
  
  sealed trait BinOpEnumVal extends OpEnumVal
  case object ADD extends BinOpEnumVal // x = y + z, etc.
  case object SUB extends BinOpEnumVal
  case object MULT extends BinOpEnumVal
  case object DIV extends BinOpEnumVal
  case object MOD extends BinOpEnumVal
  
  case object AND extends BinOpEnumVal // x = y && z, etc.
  case object OR extends BinOpEnumVal

  case object LT extends BinOpEnumVal // x = y < z, etc.
  case object LTE extends BinOpEnumVal
  case object GT extends BinOpEnumVal
  case object GTE extends BinOpEnumVal

  case object EQ extends BinOpEnumVal // x = y == z, etc.
  case object NEQ extends BinOpEnumVal

  sealed trait UnOpEnumVal extends OpEnumVal

  case object SIZE extends UnOpEnumVal // x = @y
  case object MINUS extends UnOpEnumVal // x = -y  (Note this is unary minus, not subtraction)
  case object NOT extends UnOpEnumVal // x = !y
}
