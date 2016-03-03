package compile

// class definitions for the intermediate representation
abstract class Ir
object Ir {

case class IrProgram(calloutDecls : List[IrCalloutDecl], fieldDecls : List[IrFieldDecl], methodDecls : List[IrMethodDecl]) extends Ir

// == Declarations ==
abstract class IrMemberDecl extends Ir
case class         IrCalloutDecl(name: String) extends IrMemberDecl
case class         IrFieldDecl(fieldType: IrType, fields: List[IrFieldDeclArg]) extends IrMemberDecl
case class         IrMethodDecl(methodType: IrType, name: String, args: List[IrMethodDeclArg], bodyBlock: IrBlock) extends IrMemberDecl

abstract class IrFieldDeclArg
case class         IrSingleFieldDecl(name: String) extends IrFieldDeclArg
case class         IrArrayFieldDecl(name: String, size: IrIntLiteral) extends IrFieldDeclArg

case class IrMethodDeclArg(argType: IrType, name: String)

// == Block ==
case class IrBlock(fieldDecls: List[IrFieldDecl], stmts: List[IrStatement]) extends Ir

// == Types ==
abstract class IrType extends Ir
case class         IrIntType() extends IrType
case class         IrBoolType() extends IrType
case class         IrVoidType() extends IrType

// == Statements ==
abstract class IrStatement extends Ir
abstract class IrAssignStmt extends IrStatement
case class         IrEqualsAssignStmt (loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class         IrMinusAssignStmt (loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class         IrPlusAssignStmt (loc: IrLocation, expr: IrExpression) extends IrAssignStmt

case class         IrMethodCallStmt (methCall: IrCallExpr) extends IrStatement
case class         IrIfStmt (cond: IrExpression, ifBlock: IrBlock, elseBlock: Option[IrBlock]) extends IrStatement
case class         IrForStmt (loc: IrLocation, initVal: IrExpression, endVal: IrExpression, inc: Option[IrIntLiteral], bodyBlock: IrBlock) extends IrStatement
case class         IrWhileStmt (boolExpr: IrExpression, bodyBlock: IrBlock) extends IrStatement
case class         IrReturnStmt (value: Option[IrExpression]) extends IrStatement
case class         IrBreakStmt () extends IrStatement
case class         IrContinueStmt () extends IrStatement


// == Expressions ==
abstract class IrExpression extends Ir

// = Method Call or Callout =
abstract class IrCallExpr extends IrExpression
case class         IrMethodCallExpr(name: String, args: List[IrCallArg]) extends IrCallExpr
case class         IrCalloutExpr(name: String, args: List[IrCallArg]) extends IrCallExpr
abstract class IrCallArg
case class         IrCallExprArg(arg : IrExpression) extends IrCallArg
case class         IrCallStringArg(arg : IrStringLiteral) extends IrCallArg

// = Literals =
abstract class     IrLiteral extends IrExpression
case class             IrIntLiteral(value: Option[Int], rep: String) extends IrLiteral
case class             IrBooleanLiteral(value: Boolean) extends IrLiteral
case class             IrCharLiteral(value: Char) extends IrLiteral
case class             IrStringLiteral(value: String) extends IrLiteral
// = Location =
abstract class     IrLocation extends IrExpression
case class             IrSingleLocation(name: String) extends IrLocation
case class             IrArrayLocation(name: String, index: IrExpression) extends IrLocation
// = Ternary Expression =
case class          IrTernOpExpr(cond: IrExpression, leftExpr: IrExpression, rightExpr: IrExpression) extends IrExpression
// = Binary Expression =
case class          IrBinOpExpr(binOp: IrBinOp, leftExpr: IrExpression, rightExpr: IrExpression) extends IrExpression
// = Binary Operators =
abstract class IrBinOp
abstract class     IrArithOp extends IrBinOp
case class             IrMulOp() extends IrArithOp
case class             IrDivOp() extends IrArithOp
case class             IrModOp() extends IrArithOp
case class             IrAddOp() extends IrArithOp
case class             IrSubOp() extends IrArithOp
abstract class     IrRelOp extends IrBinOp
case class              IrLtOp() extends IrRelOp
case class              IrLteOp() extends IrRelOp
case class              IrGtOp() extends IrRelOp
case class              IrGteOp() extends IrRelOp
abstract class     IrEqOp extends IrBinOp
case class              IrEqualOp() extends IrEqOp
case class              IrNotEqualOp() extends IrEqOp
abstract class     IrCondOp extends IrBinOp
case class              IrAndOp() extends IrCondOp
case class              IrOrOp() extends IrCondOp

// = Unary Expression =
case class          IrUnOpExpr(unop: IrUnOp, expr: IrExpression) extends IrExpression
// = Unary Operators = 
abstract class IrUnOp
// Since we can have many unary minuses or unary nots stacked together, keep track of the number of such tokens encountered
case class         IrMinusOp() extends IrUnOp
case class         IrNotOp() extends IrUnOp
case class         IrArraySizeOp() extends IrUnOp
}
