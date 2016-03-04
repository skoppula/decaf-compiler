package compile

// class definitions for the intermediate representation

class NodeLocation(ln: Int, cn: Int) {
    // var program : String = pr //TODO: get the program name somehow
    var lineNumber : Int = ln
    var columnNumber : Int = cn
}

abstract class Ir {
    var nodeLoc : NodeLocation = null //TODO: null for now to suppress warnings
    var errors : List[Any] = null //TODO: make an IrError class
    var parent : Ir = null
}

object Ir {

case class IrProgram(calloutDecls: List[IrCalloutDecl], fieldDecls: List[IrFieldDecl], methodDecls: List[IrMethodDecl], loc: NodeLocation) extends Ir {
    nodeLoc = loc
    calloutDecls.foreach(c => c.parent = this)
    fieldDecls.foreach(d => d.parent = this)
    methodDecls.foreach(m => m.parent = this)
}

// == Declarations ==
abstract class IrMemberDecl(loc : NodeLocation) extends Ir {
    nodeLoc = loc
}
case class         IrCalloutDecl(name: String, loc: NodeLocation) extends IrMemberDecl(loc) {
    override def toString(): String = name;
}   

case class         IrFieldDecl(fieldType: IrType, fields: List[IrFieldDeclArg], loc: NodeLocation) extends IrMemberDecl(loc){
}

case class         IrMethodDecl(methodType: IrType, name: String, args: List[IrMethodDeclArg], bodyBlock: IrBlock, loc: NodeLocation) extends IrMemberDecl(loc) {
    bodyBlock.parent = this
}

abstract class IrFieldDeclArg(loc : NodeLocation) {
    var nodeLoc = loc
}
case class         IrSingleFieldDecl(name: String, loc: NodeLocation) extends IrFieldDeclArg(loc)
case class         IrArrayFieldDecl(name: String, size: IrIntLiteral, loc: NodeLocation) extends IrFieldDeclArg(loc)

case class IrMethodDeclArg(argType: IrType, name: String, loc : NodeLocation) {
    var nodeLoc = loc
}

// == Block ==
case class IrBlock(fieldDecls: List[IrFieldDecl], stmts: List[IrStatement], loc: NodeLocation) extends Ir {
    nodeLoc = loc
    fieldDecls.foreach(f => f.parent = this)
    stmts.foreach(s => s.parent = this)    
}

// == Types ==
abstract class IrType(loc: NodeLocation) extends Ir {
    nodeLoc = loc
}
case class         IrIntType(loc: NodeLocation) extends IrType(loc) {
    override def toString(): String = "int";
}
case class         IrBoolType(loc: NodeLocation) extends IrType(loc) {
    override def toString(): String = "bool";
}
case class         IrVoidType(loc: NodeLocation) extends IrType(loc) {
    override def toString(): String = "void";
}

// == Statements ==
abstract class IrStatement(loc: NodeLocation) extends Ir {
    nodeLoc = loc
}
abstract class IrAssignStmt(loc: IrLocation, expr: IrExpression, nodeloc: NodeLocation) extends IrStatement(nodeloc) {
    expr.parent = this
}
case class         IrEqualsAssignStmt (loc: IrLocation, expr: IrExpression, nodeloc: NodeLocation) extends IrAssignStmt(loc, expr, nodeloc) {
    override def toString(): String = loc.toString() + " := " + expr.toString();
}
case class         IrMinusAssignStmt (loc: IrLocation, expr: IrExpression, nodeloc: NodeLocation) extends IrAssignStmt(loc, expr, nodeloc) {
    override def toString(): String = loc.toString() + " -= " + expr.toString(); 
}
case class         IrPlusAssignStmt (loc: IrLocation, expr: IrExpression, nodeloc: NodeLocation) extends IrAssignStmt(loc, expr, nodeloc) {
    override def toString(): String = loc.toString() + " += " + expr.toString();
}

case class         IrMethodCallStmt (methCall: IrCallExpr, loc: NodeLocation) extends IrStatement(loc) {
    methCall.parent = this
}

case class         IrIfStmt (cond: IrExpression, ifBlock: IrBlock, elseBlock: Option[IrBlock], loc: NodeLocation) extends IrStatement(loc) {
    cond.parent = this
    ifBlock.parent = this
    if (elseBlock.isDefined) { 
        elseBlock.get.parent = this 
    }
}

case class         IrForStmt (loc: IrLocation, initVal: IrExpression, endVal: IrExpression, inc: Option[IrIntLiteral], bodyBlock: IrBlock, nodeLoc: NodeLocation) extends IrStatement(nodeLoc) {
    bodyBlock.parent = this
}

case class         IrWhileStmt (boolExpr: IrExpression, bodyBlock: IrBlock, loc: NodeLocation) extends IrStatement(loc) {
    bodyBlock.parent = this
    override def toString(): String = "while" + boolExpr.toString() + bodyBlock.toString();
}
case class         IrReturnStmt (value: Option[IrExpression], loc: NodeLocation) extends IrStatement(loc) {
    if (value.isDefined) {
        value.get.parent = this
    }
}

case class         IrBreakStmt (loc: NodeLocation) extends IrStatement(loc)
case class         IrContinueStmt (loc: NodeLocation) extends IrStatement(loc)


// == Expressions ==
abstract class IrExpression(loc: NodeLocation) extends Ir {
    nodeLoc = loc
}

// = Method Call or Callout =
abstract class IrCallExpr(loc: NodeLocation) extends IrExpression(loc: NodeLocation)
case class         IrMethodCallExpr(name: String, args: List[IrCallArg], loc: NodeLocation) extends IrCallExpr(loc)
case class         IrCalloutExpr(name: String, args: List[IrCallArg], loc: NodeLocation) extends IrCallExpr(loc)
abstract class IrCallArg(loc: NodeLocation)
case class         IrCallExprArg(arg : IrExpression, loc: NodeLocation) extends IrCallArg(loc)
case class         IrCallStringArg(arg : IrStringLiteral, loc: NodeLocation) extends IrCallArg(loc)

// = Literals =
abstract class     IrLiteral(loc: NodeLocation) extends IrExpression(loc) {
    nodeLoc = loc
}
case class             IrIntLiteral(value: Option[Int], rep: String, loc: NodeLocation) extends IrLiteral(loc)
case class             IrBooleanLiteral(value: Boolean, loc: NodeLocation) extends IrLiteral(loc)
case class             IrCharLiteral(value: Char, loc: NodeLocation) extends IrLiteral(loc)
case class             IrStringLiteral(value: String, loc: NodeLocation) extends IrLiteral(loc)
// = Location =
abstract class     IrLocation(loc: NodeLocation) extends IrExpression(loc)
case class             IrSingleLocation(name: String, loc: NodeLocation) extends IrLocation(loc) {
    override def toString(): String = name;
}
case class             IrArrayLocation(name: String, index: IrExpression, loc: NodeLocation) extends IrLocation(loc) {
    index.parent = this
    //TODO: Check that index is in bounds of name
    override def toString(): String = name + "[" + index.toString() + "]";
}
// = Ternary Expression =
case class          IrTernOpExpr(cond: IrExpression, leftExpr: IrExpression, rightExpr: IrExpression, loc: NodeLocation) extends IrExpression(loc) {
    cond.parent = this
    leftExpr.parent = this
    rightExpr.parent = this
    override def toString(): String = cond.toString() + "?" + leftExpr.toString() + ":" + rightExpr.toString();
}
// = Binary Expression =
case class          IrBinOpExpr(binOp: IrBinOp, leftExpr: IrExpression, rightExpr: IrExpression, loc: NodeLocation) extends IrExpression(loc) {
    leftExpr.parent = this
    rightExpr.parent = this
}

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
case class          IrUnOpExpr(unop: IrUnOp, expr: IrExpression, loc: NodeLocation) extends IrExpression(loc) {
    expr.parent = this
}
// = Unary Operators = 
abstract class IrUnOp
// Since we can have many unary minuses or unary nots stacked together, keep track of the number of such tokens encountered
case class         IrMinusOp() extends IrUnOp
case class         IrNotOp() extends IrUnOp
case class         IrArraySizeOp() extends IrUnOp
}