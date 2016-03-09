package compile

case class IdentifierNotFoundException(message: String) extends Exception(message)

case class IdentifierAlreadyExistsException(message: String) extends Exception(message)

case class IdentifierAlreadyExistsWithLocException(message: String, loc : NodeLocation) extends ExceptionWithLocation(message, loc)

case class MethodNotFoundException(message: String) extends Exception(message)

case class MethodAlreadyExistsException(message: String) extends Exception(message)

case class CalloutAlreadyExistsException(message: String) extends Exception(message)

case class CalloutAlreadyExistsWithLocException(message: String, loc : NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidCalloutException(message: String) extends Exception(message)

case class InvalidCalloutWithLocException(message: String, loc : NodeLocation) extends ExceptionWithLocation(message, loc)

abstract class ExceptionWithLocation(message: String, loc: NodeLocation) extends Exception(message)

case class ExpectingCalloutTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ExpectingFieldDeclTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ExpectingIDInFieldDeclException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingToIrTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingToBlockIrException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingToStatementException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingToAssignmentException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingToVariableLocException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingReturnTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingExprTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingLiteralTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ConvertingCharLiteralTokenException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class VoidCannotBeDeclarationTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidIntLiteralException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class IdentifierIsNotArrayException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class OutOfBoundsArrayAccessException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidArraySizeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class MethodParameterCannotBeVoidException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class DuplicateParameterNameException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ArithOpIntOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class RelOpIntOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class EqOpTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class CondOpBoolOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)






