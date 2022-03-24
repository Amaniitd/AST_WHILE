signature Parse_TOKENS =
sig
type ('a,'b) token
type svalue
val COMMA: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val NOT: (string) *  'a * 'a -> (svalue,'a) token
val OR: (string) *  'a * 'a -> (svalue,'a) token
val AND: (string) *  'a * 'a -> (svalue,'a) token
val MOD: (string) *  'a * 'a -> (svalue,'a) token
val DIV: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val MINUS: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val NOTEQ: (string) *  'a * 'a -> (svalue,'a) token
val GREATERTHANEQ: (string) *  'a * 'a -> (svalue,'a) token
val LESSTHANEQ: (string) *  'a * 'a -> (svalue,'a) token
val GREATERTHAN: (string) *  'a * 'a -> (svalue,'a) token
val LESSTHAN: (string) *  'a * 'a -> (svalue,'a) token
val EQ: (string) *  'a * 'a -> (svalue,'a) token
val ASSIGN: (string) *  'a * 'a -> (svalue,'a) token
val DCOLON: (string) *  'a * 'a -> (svalue,'a) token
val RCURLY: (string) *  'a * 'a -> (svalue,'a) token
val LCURLY: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN: (string) *  'a * 'a -> (svalue,'a) token
val LPAREN: (string) *  'a * 'a -> (svalue,'a) token
val TERM: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val TBOOL: (string) *  'a * 'a -> (svalue,'a) token
val TINT: (string) *  'a * 'a -> (svalue,'a) token
val VAR: (string) *  'a * 'a -> (svalue,'a) token
val PROGRAM: (string) *  'a * 'a -> (svalue,'a) token
val WRITE: (string) *  'a * 'a -> (svalue,'a) token
val READ: (string) *  'a * 'a -> (svalue,'a) token
val ENDWH: (string) *  'a * 'a -> (svalue,'a) token
val DO: (string) *  'a * 'a -> (svalue,'a) token
val WHILE: (string) *  'a * 'a -> (svalue,'a) token
val ENDIF: (string) *  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val BOOL: (bool) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Parse_LRVALS=
sig
structure Tokens : Parse_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
