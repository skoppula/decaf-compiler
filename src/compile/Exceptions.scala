package compile

case class IdentifierNotFoundException(message: String) extends Exception(message)

case class IdentifierAlreadyExistsException(message: String) extends Exception(message)

case class MethodNotFoundException(message: String) extends Exception(message)

case class MethodAlreadyExistsException(message: String) extends Exception(message)

case class CalloutAlreadyExistsException(message: String) extends Exception(message)
