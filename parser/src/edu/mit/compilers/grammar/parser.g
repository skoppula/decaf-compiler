header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

class DecafParser extends Parser;
options
{
  importVocab = DecafScanner;
  k = 3;
  buildAST = true;
}

// Java glue code that makes error reporting easier.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  // Do our own reporting of errors so the parser can return a non-zero status
  // if any errors are detected.
  /** Reports if any errors were reported during parse. */
  private boolean error;

  @Override
  public void reportError (RecognitionException ex) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
  }
  public boolean getError () {
    return error;
  }

  // Selectively turns on debug mode.

  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws TokenStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws TokenStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

program : (callout_decl)* (field_decl)* (method_decl)* EOF;
callout_decl : CALLOUT ID SEMICOLON;
field_decl : type (ID | ID LBRACK INT_LITERAL RBRACK) (COMMA (ID | ID LBRACK INT_LITERAL RBRACK))*  SEMICOLON;
method_decl : (type | VOID) ID LPAREN ((type ID) (COMMA type ID)*)? RPAREN block;
block : LBRACE (field_decl)* (statement)* RBRACE;
type : (INT | BOOLEAN);
statement
  : ( location assign_op expr SEMICOLON
    | method_call SEMICOLON
    | IF LPAREN expr RPAREN block (ELSE block)?
    | FOR LPAREN ID EQ expr COMMA expr (COMMA INT_LITERAL)? RPAREN block
    | WHILE LPAREN expr RPAREN block
    | RETURN (expr)? SEMICOLON
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    )
  ;

method_call
  : method_name LPAREN ( (expr | STRING_LITERAL) (COMMA (expr | STRING_LITERAL))*)? RPAREN
    //( method_name LPAREN (expr (COMMA expr)*)?  RPAREN
    //| method_name LPAREN (callout_arg (COMMA callout_arg)*)? RPAREN
    //)
  ;

method_name : {LA(2) == LPAREN}? ID;

location : {LA(2) != LPAREN}? (ID | ID LBRACK expr RBRACK);

expr
  : ( method_call expr_
    | location expr_
    | literal expr_
    | AT ID expr_
    | MINUS expr expr_
    | NOT expr expr_
    | LPAREN expr RPAREN expr_
    )
  ;

expr_
  : ( QUESTION expr COLON expr expr_
    | bin_op expr expr_
    | // epsilon
    )
  ;

//expr
  //:(| location
    //| method_call
    //| literal
    //| AT ID
    //| expr bin_op expr // Need to eliminate left recursion 
    //| MINUS expr
    //| NOT expr
    //| LPAREN expr RPAREN
    //| expr QUESTION expr COLON expr // Need to eliminate left recursion
//  ;

// callout_arg : expr | STRING_LITERAL;

// === Literals ===
literal : (INT_LITERAL | CHAR_LITERAL | bool_literal );
bool_literal : (TRUE | FALSE);

// === Operators ===
bin_op : (OR | AND | EQ_OP | REL_OP | add_op | mul_op);
//bin_op : (arith_op | REL_OP | EQ_OP | cond_op);
assign_op : (ASSIGN_OP_DELTA | EQ);
// arith_op : (add_op | mul_op);
add_op : (PLUS | MINUS);
mul_op : (MUL | DIV | MOD);
// cond_op : (AND | OR);

// == Hacking around binary operator precedence ==
// Increasing order of precedence
// Likely will have to reformat below to fit the hacks around left recursion
/*
bin_op : bin_or;
bin_or : bin_and (OR bin_and)*;
bin_and : bin_eq (AND bin_eq)*;
bin_eq : bin_rel (EQ_OP bin_rel)*;
bin_rel : bin_add (REL_OP bin_add)*;
bin_add : bin_mul (add_op bin_mul)*;
bin_mul : un_op (mul_op un_op)*;

un_op : un_not;
un_not : (NOT)* un_minus;
un_minus : (MINUS)* expr;
*/

// OLD RULES
// program : TK_class ID LCURLY RCURLY EOF;
