package compile.tac

// refers to the op in three-address code, generally of the form x = y op z
object OpTypes {
  sealed trait OpEnumVal
  
  sealed trait BinOpEnumVal extends OpEnumVal
  case object ADD extends BinOpEnumVal {// x = y + z, etc.
    override def toString : String = "ADD"
  }
  case object SUB extends BinOpEnumVal {
    override def toString : String = "SUB"
  }
  case object MULT extends BinOpEnumVal {
    override def toString : String = "MULT"
  }
  case object DIV extends BinOpEnumVal {
    override def toString : String = "DIV"
  }
  case object MOD extends BinOpEnumVal {
    override def toString : String = "MOD"
  }
  
  case object AND extends BinOpEnumVal {// x = y && z, etc.
    override def toString : String = "AND"
  }
  case object OR extends BinOpEnumVal {
    override def toString : String = "OR"
  }

  case object LT extends BinOpEnumVal {// x = y < z, etc.
    override def toString : String = "LT"
  }
  case object LTE extends BinOpEnumVal {
    override def toString : String = "LTE"
  }
  case object GT extends BinOpEnumVal {
    override def toString : String = "GT"
  }
  case object GTE extends BinOpEnumVal {
    override def toString : String = "GTE"
  }

  case object EQ extends BinOpEnumVal {// x = y == z, etc.
    override def toString : String = "EQ"
  }
  case object NEQ extends BinOpEnumVal {
    override def toString : String = "NEQ"
  }

  sealed trait UnOpEnumVal extends OpEnumVal

  case object SIZE extends UnOpEnumVal {// x = @y
    override def toString : String = "SIZE"
  }
  case object MINUS extends UnOpEnumVal { // x = -y  (Note this is unary minus, not subtraction)
    override def toString : String = "MINUS"
  }
  case object NOT extends UnOpEnumVal {// x = !y
    override def toString : String = "NOT"
  }
}
