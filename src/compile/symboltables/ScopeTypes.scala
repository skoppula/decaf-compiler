package compile.symboltables

object ScopeTypes {
  sealed trait EnumVal
  case object FOR extends EnumVal
  case object WHILE extends EnumVal
  case object IF extends EnumVal
  case object METHOD extends EnumVal
}
