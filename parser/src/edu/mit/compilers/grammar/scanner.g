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
MUL options { paraphrase = "*"; } : '*';
DIV options { paraphrase = "/"; } : '/';
MOD options { paraphrase = "%"; } : '%';
EQ options { paraphrase = "="; } : {LA(2) != '='}? '=';
PLUSEQ options { paraphrase = "+="; } : "+=";
MINUSEQ options { paraphrase = "-="; } : "-=";
AND options { paraphrase = "&&"; } : "&&";
OR options { paraphrase = "||"; } : "||";

ID options { paraphrase = "ID"; } : ALPHA (ALPHA_NUM)*;

// === Operators ===
// Some of the operators are put into the parser to prevent lexical nondeterminism
EQ_OP :  ("==" | "!=");
REL_OP : ("<=" | ">=" | '<' | '>');

// === Literals ===
// Since "true" and "false" are reserved keywords, they are parsed as individual tokens and used directly in the parser
INT_LITERAL options { paraphrase = "INT"; } : (DECIMAL_LITERAL | HEX_LITERAL);
CHAR_LITERAL options { paraphrase = "CHAR"; } : '\'' CHAR '\'';
STRING_LITERAL options { paraphrase = "STRING"; } : '"' (CHAR)*  '"';

WS : (' ' | '\t' | '\n' { newline(); } ) { _ttype = Token.SKIP; };
COMMENT : "//" (~'\n')* '\n' { _ttype = Token.SKIP; newline(); };

protected DECIMAL_LITERAL : DIGIT (DIGIT)*;
protected HEX_LITERAL : "0x" HEX_DIGIT (HEX_DIGIT)*;
protected ALPHA_NUM : (ALPHA | DIGIT);
protected ALPHA : ('a'..'z' | 'A'..'Z'| '_');
protected HEX_DIGIT : (DIGIT | 'a'..'f' | 'A'..'F');
protected DIGIT : ('0'..'9');

// Printable range (octal) is '\40'..'\176'. Exclude '"' , '\'' , '\\'.
protected CHAR : (ESC|'\40'..'\41'|'\43'..'\46'|'\50'..'\133'|'\135'..'\176');
protected ESC :  '\\' ('"'|'\''|'\\'|'t'|'n');
