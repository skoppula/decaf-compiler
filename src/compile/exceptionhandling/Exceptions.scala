package compile.exceptionhandling

import compile.NodeLocation

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

case class IdentifierIsArrayButIndexIsNotIntException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class OutOfBoundsArrayAccessException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidArraySizeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class MethodParameterCannotBeVoidException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class DuplicateParameterNameException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ArithOpIntOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class RelOpIntOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class EqOpTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class CondOpBoolOperandException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class NotOpTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class LengthOpTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class NegateOverflowException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class MinusOpTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class TernOpMatchTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class TernOpCondTypeException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidMethodCallReturnException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class InvalidMethodCallArgumentException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

// Statement exceptions

case class AssignEqStmtTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class AssignPlusEqStmtTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class AssignMinusEqStmtTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class IfStmtCondTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ForStmtIterLocTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class ForStmtInitExprTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class ForStmtEndExprTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class ForStmtIncValMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class WhileStmtCondTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class ReturnStmtTypeMismatch(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class BreakStmtInvalidScope(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class StringArgInMethodCallException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class IncorrectNumberOfArgsException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class CompilerProblem(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)
case class CompilerProblemNoLocation(message: String) extends Exception(message)

case class ExpectedSubBlockCountNotActualException(message: String) extends Exception(message)
case class StmtAfterContinueBreakReturnException(message: String) extends Exception(message)
case class NullElseBlockException(message: String) extends Exception(message)
case class NotForIfWhileStmtException(message: String) extends Exception(message)

case class NoMatchingStatementException(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class MethodCallIsShadowedByLocalVariable(message: String, loc: NodeLocation) extends ExceptionWithLocation(message, loc)

case class NoMainMethodException(message: String) extends Exception(message)

case class MethodDoesNotExistException(message: String) extends Exception(message)

case class MainMethodHasParametersException(message: String) extends Exception(message)

case class InvalidGlobalFieldTableMethodException(message: String) extends Exception(message)
