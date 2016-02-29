package compile

object ScopeTypes {
  sealed trait EnumVal
  case object FOR extends EnumVal
  case object WHILE extends EnumVal
  case object IF extends EnumVal
}
