structure Tokens=Tokens

	type pos = int
	type pol = int
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue, pos) token

	val linenum = ref 1;
	val column = ref 1;
	val eof = fn() => Tokens.EOF(!column,!column)

%%

%header (functor AssignLexFun(structure Tokens:Assign_TOKENS));
alpha = [A-Za-z];
whitespace = [\ \t];
digit = [0-9];
vchar = [0-9A-Za-z];

%%
\n => (linenum := !linenum + 1; column := 1; lex());
{whitespace}+ => (column := !column + size(yytext); lex());
";" => (column := !column + size(yytext); Tokens.TERM(yytext,!linenum,!column));

"(" => (column := !column + size(yytext); Tokens.LPAREN(yytext,!linenum,!column));
")" => (column := !column + size(yytext); Tokens.RPAREN(yytext,!linenum,!column));
"{" => (column := !column + size(yytext); Tokens.LCURLY(yytext,!linenum,!column));
"}" => (column := !column + size(yytext); Tokens.RCURLY(yytext,!linenum,!column));
"," => (column := !column + size(yytext); Tokens.COMMA(yytext,!linenum,!column));


"::" => (column := !column + size(yytext); Tokens.DCOLON(yytext,!linenum,!column));

":=" => (column := !column + size(yytext); Tokens.ASSIGN(yytext,!linenum,!column));

"=" => (column := !column + size(yytext); Tokens.EQ(yytext,!linenum,!column));
"<" => (column := !column + size(yytext); Tokens.LESSTHAN(yytext,!linenum,!column));
">" => (column := !column + size(yytext); Tokens.GREATERTHAN(yytext,!linenum,!column));
"<=" => (column := !column + size(yytext); Tokens.LESSTHANEQ(yytext,!linenum,!column));
">=" => (column := !column + size(yytext); Tokens.GREATERTHANEQ(yytext,!linenum,!column));
"<>" => (column := !column + size(yytext); Tokens.NOTEQ(yytext,!linenum,!column));

"+" => (column := !column + size(yytext); Tokens.PLUS(yytext,!linenum,!column));
"-" => (column := !column + size(yytext); Tokens.MINUS(yytext,!linenum,!column));
"*" => (column := !column + size(yytext); Tokens.TIMES(yytext,!linenum,!column));
"/" => (column := !column + size(yytext); Tokens.DIV(yytext,!linenum,!column));
"%" => (column := !column + size(yytext); Tokens.MOD(yytext,!linenum,!column));

"&&" => (column := !column + size(yytext); Tokens.AND(yytext,!linenum,!column));
"||" => (column := !column + size(yytext); Tokens.OR(yytext,!linenum,!column));
"!" => (column := !column + size(yytext); Tokens.NOT(yytext,!linenum,!column));







{digit}+ => (column := !column + size(yytext); Tokens.INT(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!linenum,!column));
{alpha}+ => (column := !column + size(yytext);(
if yytext="tt" then Tokens.BOOL(true,!linenum,!column)
else if yytext="ff" then Tokens.BOOL(false,!linenum,!column)
else if yytext="if" then Tokens.IF(yytext,!linenum,!column)
else if yytext="then" then Tokens.THEN(yytext,!linenum,!column)
else if yytext="else" then Tokens.ELSE(yytext,!linenum,!column)
else if yytext="endif" then Tokens.ENDIF(yytext,!linenum,!column)
else if yytext="while" then Tokens.WHILE(yytext,!linenum,!column)
else if yytext="do" then Tokens.DO(yytext,!linenum,!column)
else if yytext="endwh" then Tokens.ENDWH(yytext,!linenum,!column)
else if yytext="program" then Tokens.PROGRAM(yytext,!linenum,!column)
else if yytext="read" then Tokens.READ(yytext,!linenum,!column)
else if yytext="write" then Tokens.WRITE(yytext,!linenum,!column)
else if yytext="var" then Tokens.VAR(yytext,!linenum,!column)
else if yytext="int" then Tokens.TINT(yytext,!linenum,!column)
else if yytext="bool" then Tokens.TBOOL(yytext,!linenum,!column)

else Tokens.ID(yytext,!linenum,!column)
));
{alpha}+{vchar}* => (column := !column + size(yytext); Tokens.ID(yytext,!linenum,!column)); 
. => (lex());
