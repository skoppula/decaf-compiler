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

method_call : method_name LPAREN (callout_arg (COMMA callout_arg)*)? RPAREN;
callout_arg : expr | STRING_LITERAL;
method_name : ID;
location : (ID | ID LBRACK expr RBRACK);

// == Expressions ==
// Hacking around binary operator precedence and fixing left recursion
expr : expr_bin (options { greedy = true; }: QUESTION expr COLON expr)*;
expr_bin : expr_or;
expr_or : expr_and (options { greedy = true; }: OR expr_and)*;
expr_and : expr_eq (options { greedy = true; }: AND expr_eq)*;
expr_eq : expr_rel (options { greedy = true; }: EQ_OP expr_rel)*;
expr_rel : expr_add (options { greedy = true; }: REL_OP expr_add)*;
expr_add : expr_mul (options { greedy = true; }: add_op expr_mul)*;
expr_mul : expr_atom (options { greedy = true; }: mul_op expr_atom)*;

expr_atom
  : ( method_call
    | location
    | literal
    | AT ID
    | MINUS expr 
    | NOT expr
    | LPAREN expr RPAREN
    )
  ;

// === Literals ===
literal : (INT_LITERAL | CHAR_LITERAL | bool_literal );
bool_literal : (TRUE | FALSE);

// === Operators ===
assign_op : (PLUSEQ | MINUSEQ | EQ);
add_op : (PLUS | MINUS);
mul_op : (MUL | DIV | MOD);
