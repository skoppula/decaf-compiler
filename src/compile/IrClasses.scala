// class definitions for the intermediate representation 

abstract class Ir
abstract class IrExpression

abstract class IrLiteral extends IrExpression
class IrIntLiteral extends IrLiteral
class IrBooleanLiteral extends IrLiteral
class IrCharLiteral extends IrLiteral

class IrLocation(array: IrId, index: IrExpression) extends IrExpression

abstract class IrCallExpr  extends IrExpression

class IrMethodCallExpr(methCall: IrMethodCall) extends IrCallExpr
class IrCalloutExpr(calloutCall: IrCalloutCall) extends IrCallExpr // calloutCall is a 10/10 variable name
class IrBinopExpr(leftExpr: IrExpression, binop: IrBinOp, rightExpr:  extends IrCallExpr


abstract class   IrStatement
case class               IrAssignStmt extends IrStatement
case class                   IrEqualsAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class                   IrMinusAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class		             IrPlusAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt

class               IrMethodCallStmt(methCall: IrMethodCall) extends IrStatement

class               IrIfStmt(cond: IrExpression, bodyBlock: IrBlock, elseBlock: Option[IrBlock]) extends IrStatement
class               IrBreakStmt extends IrStatement
class               IrContinueStmt extends IrStatement
class               IrForStmt (???, bodyBlock: IrBlock) extends IrStatement
class               IrReturnStmt (value: Option[IrExpression]) extends IrStatement
class               IrBreakStmt extends IrStatement
class               IrContinue extends IrStatement

abstract class IrType
class IrIntType extends IrType
class IrBoolType extends IrType

