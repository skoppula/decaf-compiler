header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

{@SuppressWarnings("unchecked")}
class DecafScanner extends Lexer;
options
{
  k = 2;
}


// Reserved keywords
tokens 
{
BOOLEAN = "boolean";
BREAK = "break";
CALLOUT = "callout";
CONTINUE = "continue";
ELSE = "else";
FALSE = "false";
FOR = "for";
WHILE = "while";
IF = "if";
INT = "int";
RETURN = "return";
TRUE = "true";
VOID = "void";
}


// Selectively turns on debug tracing mode.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws CharStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws CharStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

LBRACE options { paraphrase = "{"; } : '{';
RBRACE options { paraphrase = "}"; } : '}';
LPAREN options { paraphrase = "("; } : '(';
RPAREN options { paraphrase = ")"; } : ')';
LBRACK options { paraphrase = "["; } : '[';
RBRACK options { paraphrase = "]"; } : ']';
AT options { paraphrase = "@"; } : '@';
NOT options { paraphrase = "!"; } : '!';
QUESTION options { paraphrase = "?"; } : '?';
COLON options { paraphrase = ":"; } : ':';
SEMICOLON options { paraphrase = ";"; } : ';';
COMMA options { paraphrase = ","; } : ',';
PLUS options { paraphrase = "+"; } : {LA(2) != '='}? '+';
MINUS options { paraphrase = "-"; } : {LA(2) != '='}? '-';




ID options { paraphrase = "ID"; } : ALPHA (ALPHA_NUM)*;

// === Operators ===
// BIN_OP : (ARITH_OP | REL_OP | EQ_OP | COND_OP);
ASSIGN_OP : ("+=" | "-=" | {LA(2) != '='}? '=');
EQ_OP :  ("==" | "!=");
COND_OP : ("&&" | "||");
ARITH_OP : ( PLUS | MINUS | '*' | '/' | '%');
REL_OP : ("<=" | ">=" | '<' | '>');

// === Literals ===
// LITERAL : (INT_LITERAL | CHAR_LITERAL | BOOL_LITERAL ); // Consider putting this one in the parser
INT_LITERAL options { paraphrase = "INT"; } : (DECIMAL_LITERAL | HEX_LITERAL);

// BOOL_LITERAL options { paraphrase = "BOOL"; } : ("true" | "false"); // These are reserved keywords so they go into the tokens section
CHAR_LITERAL options { paraphrase = "CHAR"; }: '\'' CHAR '\'' ;
STRING_LITERAL options { paraphrase = "STRING"; } : '"' (CHAR)*  '"' ;

WS : (' '  | '\t' | '\n' {newline();} ) {_ttype = Token.SKIP;} ;// Don't want to skip token when we see sequence \n <EOF>
COMMENT : "//" (~'\n')* '\n' { _ttype = Token.SKIP; newline(); };

protected DECIMAL_LITERAL : DIGIT (DIGIT)*;
protected HEX_LITERAL : "0x" HEX_DIGIT (HEX_DIGIT)*;
protected ALPHA_NUM : (ALPHA | DIGIT);
protected ALPHA : ('a'..'z' | 'A'..'Z'| '_');
protected HEX_DIGIT : (DIGIT | 'a'..'f' | 'A'..'F');
protected DIGIT : ('0'..'9');
protected CHAR : (ESC|'\40'..'\41'|'\43'..'\46'|'\50'..'\133'|'\135'..'\176'); // Printable range (octal) is '\40'..'\176'. Exclude " , ' , \.
protected ESC :  '\\' ('"'|'\''|'\\'|'t'|'n');
