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

// Tokens for AST construction
tokens 
{
    ASSIGNMENT;
    ARRAY_ACCESS;
    ARRAY_ID;

    BLOCK;
    EXPR;

    FIELD_DECL;
    FIELD_DECL_ARG;

    LITERAL;
    LOCATION;

    PROGRAM;

    STATEMENT;

    TYPE;

    METHOD_DECL;
    METHOD_DECL_ARG;
    METHOD_CALL;
    METHOD_NAME;
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

program 
    : (callout_decl)* (field_decl)* (method_decl)* EOF!
      { #program = #([PROGRAM, "program"], #program); }
    ;

callout_decl 
    : CALLOUT^ ID SEMICOLON!
    ;

field_decl 
    : //type (ID | ID LBRACK! INT_LITERAL RBRACK!) (COMMA! (ID | ID LBRACK! INT_LITERAL RBRACK!))* SEMICOLON!
      type field_decl_arg (COMMA! field_decl_arg)* SEMICOLON!
      { #field_decl = #([FIELD_DECL, "field declaration"], #field_decl); }
    ;

field_decl_arg
    : (ID | array_id)
      { #field_decl_arg = #([FIELD_DECL_ARG, "field declaration argument"], #field_decl_arg); }
    ;

array_id
    : ID LBRACK! INT_LITERAL RBRACK!      
      { #array_id = #([ARRAY_ID, "array id"], #array_id); }
    ;

method_decl 
    : (type | VOID) ID LPAREN! ((type ID) (COMMA! type ID)*)? RPAREN! block
      { #method_decl = #([METHOD_DECL, "method declaration"], #method_decl); }        
    ;

method_decl_arg
    : type ID
      { #method_decl_arg = #([METHOD_DECL_ARG, "method declaration argument"], #method_decl_arg); }
    ;

block 
    : LBRACE! (field_decl)* (statement)* RBRACE!
      { #block = #([BLOCK, "block"], #block); }   
    ;

type 
    : (INT | BOOLEAN)
      { #type = #([TYPE, "type"], #type); }   
    ;
statement
  : ( //location assign_op expr SEMICOLON!
     location (PLUSEQ^ | MINUSEQ^ | EQ^) expr SEMICOLON!
    | method_call SEMICOLON!
    | IF^ LPAREN! expr RPAREN! block (ELSE! block)?
    | FOR^ LPAREN! ID EQ! expr COMMA! expr (COMMA INT_LITERAL)? RPAREN! block
    | WHILE^ LPAREN! expr RPAREN! block
    | RETURN^ (expr)? SEMICOLON!
    | BREAK^ SEMICOLON!
    | CONTINUE^ SEMICOLON!
    )
    { #statement = #([STATEMENT, "statement"], #statement); }
  ;

method_call 
    : method_name LPAREN! (callout_arg (COMMA! callout_arg)*)? RPAREN!
      { #method_call = #([METHOD_CALL, "method call"], #method_call); }
    ;

callout_arg 
    : expr 
    | STRING_LITERAL
    ;

method_name 
    : ID
      { #method_name = #([METHOD_NAME, "method name"], #method_name); }
    ;

location 
    : (ID | array_access)
      { #location = #([LOCATION, "location"], #location); }
    ;

array_access
    : ID LBRACK! expr RBRACK!
      { #array_access = #([ARRAY_ACCESS, "array access"], #array_access); }
    ;

// == Expressions ==
// Hacking around binary operator precedence and fixing left recursion
expr 
    : expr_bin (options { greedy = true; }: QUESTION^ expr COLON! expr)*
      { #expr = #([EXPR, "expr"], #expr); }
    ;
expr_bin : expr_or;
expr_or : expr_and (options { greedy = true; }: OR^ expr_and)*;
expr_and : expr_eq (options { greedy = true; }: AND^ expr_eq)*;
expr_eq : expr_rel (options { greedy = true; }: EQ_OP^ expr_rel)*;
expr_rel : expr_add (options { greedy = true; }: REL_OP^ expr_add)*;
expr_add : expr_mul (options { greedy = true; }: (PLUS^ | MINUS^) expr_mul)*;
expr_mul : expr_not (options { greedy = true; }: (MUL^ | DIV^ | MOD^) expr_not)*;
expr_not : (NOT^)* expr_unary_min;
expr_unary_min : (MINUS^)* expr_array_len;
expr_array_len : (AT^)? expr_atom;

expr_atom
  : ( method_call
    | location
    | literal
    | LPAREN! expr RPAREN!
    )
  ;

// === Literals ===
literal 
    : (INT_LITERAL | CHAR_LITERAL | bool_literal)
    { #literal = #([LITERAL, "literal"], #literal); }   
    ;

bool_literal : (TRUE | FALSE);

// === Operators ===
//assign_op : (PLUSEQ | MINUSEQ | EQ);
//add_op : (PLUS | MINUS);
//mul_op : (MUL | DIV | MOD);
