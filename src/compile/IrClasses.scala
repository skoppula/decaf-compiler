// class definitions for the intermediate representation 
abstract class Ir


// == Declarations ==
abstract class IrMemberDecl extends Ir
class              IrCalloutDecl(name: String) extends IrMemberDecl

class              IrMethodDecl(methodType: IrType, methodArgs: List[IrMethodDeclArg], block: IrBlock) extends IrMemberDecl
class IrMethodDeclArg(argType: IrType, name: String)

class              IrFieldDecl(fieldType: IrType, fields: List[IrFieldDeclArg]) extends IrMemberDecl
abstract class IrFieldDeclArg
class              IrSingleFieldDecl(name: String) extends IrFieldDeclArg
class              IrArrayFieldDecl(name: String, size: Integer) extends IrFieldDeclArg



// == Block ==
class IrBlock(fieldDecls: List[IrFieldDecl], stmts: List[IrStatement]) extends Ir

// == Types ==
abstract class IrType extends Ir
class              IrIntType extends IrType
class              IrBoolType extends IrType

// == Statements ==
abstract class IrStatement extends Ir
abstract class     IrAssignStmt extends IrStatement
case class             IrEqualsAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class             IrMinusAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt
case class             IrPlusAssignStmt(loc: IrLocation, expr: IrExpression) extends IrAssignStmt

class              IrMethodCallStmt(methCall: IrCallExpr) extends IrStatement
class              IrIfStmt(cond: IrExpression, bodyBlock: IrBlock, elseBlock: Option[IrBlock]) extends IrStatement
class              IrForStmt (loc: IrLocation, initVal: IrExpression, endVal: IrExpression, inc: Option[IrIntLiteral], bodyBlock: IrBlock) extends IrStatement
class              IrWhileStmt (boolExpr : IrExpression) extends IrStatement
class              IrReturnStmt (value: Option[IrExpression]) extends IrStatement
class              IrBreakStmt extends IrStatement
class              IrContinueStmt extends IrStatement


// == Expressions ==
abstract class IrExpression extends Ir

// = Method Call or Callout =
abstract class IrCallExpr extends IrExpression
class              IrMethodCallExpr(name: String, args: List[IrCallExprArg]) extends IrCallExpr
class              IrCalloutExpr(name: String, args: List[IrCallArg]) extends IrCallExpr
abstract class IrCallArg
class              IrCallExprArg(arg : IrExpression) extends IrCallArg
class              IrCallStringArg(arg : IrStringLiteral) extends IrCallArg

// = Literals =
abstract class     IrLiteral extends IrExpression
class                  IrIntLiteral(value: Int) extends IrLiteral
class                  IrBooleanLiteral(value: Boolean) extends IrLiteral
class                  IrCharLiteral(value: Char) extends IrLiteral
class                  IrStringLiteral(value: String) extends IrLiteral
// = Location =
abstract class     IrLocation extends IrExpression
case class             IrSingleLocation(name: String) extends IrExpression
case class             IrArrayLocation(name: String, index: IrExpression) extends IrExpression
// = Ternary Expression =
class              IrTernOpExpr(cond: IrExpression, leftExpr: IrExpression, rightExpr: IrExpression) extends IrExpression
// = Binary Expression =
class              IrBinOpExpr(binOp: IrBinOp, leftExpr: IrExpression, rightExpr: IrExpression) extends IrExpression
// = Binary Operators =
abstract class IrBinOp
abstract class     IrArithOp extends IrBinOp
class                  IrMulOp extends IrArithOp
class                  IrDivOp extends IrArithOp
class                  IrModOp extends IrArithOp
class                  IrAddOp extends IrArithOp
class                  IrSubOp extends IrArithOp
abstract class     IrBoolOp extends IrBinOp
class                  IrLtOp extends IrBoolOp
class                  IrLteOp extends IrBoolOp
class                  IrGtOp extends IrBoolOp
class                  IrGteOp extends IrBoolOp
class                  IrEqOp extends IrBoolOp
class                  IrNeqOp extends IrBoolOp
class                  IrAndOp extends IrBoolOp
class                  IrOrOp extends IrBoolOp

// = Unary Expression =
class          IrUnopExpr(unop: IrUnop, expr: IrExpression) extends IrExpression
// = Unary Operators = 
abstract class IrUnop
class              IrMinusOp extends IrUnop
class              IrNotOp extends IrUnop
class              IrArraySizeOp extends IrUnop
