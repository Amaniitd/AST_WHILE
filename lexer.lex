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
"=" => (column := !column + size(yytext); Tokens.EQ(yytext,!linenum,!column));
":" => (column := !column + size(yytext); Tokens.SC(yytext,!linenum,!column));
"->" => (column := !column + size(yytext); Tokens.SA(yytext,!linenum,!column));
"=>" => (column := !column + size(yytext); Tokens.DA(yytext,!linenum,!column));
{digit}+ => (column := !column + size(yytext); Tokens.INT(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!linenum,!column));
{alpha}+ => (column := !column + size(yytext);
	    (if yytext="TRUE" then
		Tokens.CONST(true,!linenum,!column)
	     else if yytext="FALSE" then
                Tokens.CONST(false,!linenum,!column)
	     else if yytext="NOT" then
		Tokens.NOT(yytext,!linenum,!column)
	     else if yytext="AND" then
		Tokens.AND(yytext,!linenum,!column)
	     else if yytext="OR" then
		Tokens.OR(yytext,!linenum,!column)
	     else if yytext="XOR" then
		Tokens.XOR(yytext,!linenum,!column)
	     else if yytext="EQUALS" then
		Tokens.EQUALS(yytext,!linenum,!column)
	     else if yytext="IMPLIES" then
		Tokens.IMPLIES(yytext,!linenum,!column)
	     else if yytext="PLUS" then
                Tokens.PLUS(yytext,!linenum,!column)
	     else if yytext="MINUS" then
                Tokens.MINUS(yytext,!linenum,!column)
	     else if yytext="TIMES" then
                Tokens.TIMES(yytext,!linenum,!column)
	     else if yytext="NEGATE" then
                Tokens.NEGATE(yytext,!linenum,!column)
	     else if yytext="LESSTHAN" then
                Tokens.LESSTHAN(yytext,!linenum,!column)
	     else if yytext="GREATERTHAN" then
                Tokens.GREATERTHAN(yytext,!linenum,!column)
	     else if yytext="if" then
		Tokens.IF(yytext,!linenum,!column)
	     else if yytext="then" then
		Tokens.THEN(yytext,!linenum,!column)
	     else if yytext="else" then
		Tokens.ELSE(yytext,!linenum,!column)
             else if yytext="fi" then
                Tokens.FI(yytext,!linenum,!column)
	     else if yytext="let" then
                Tokens.LET(yytext,!linenum,!column)
	     else if yytext="in" then
                Tokens.IN(yytext,!linenum,!column)
	     else if yytext="end" then
                Tokens.END(yytext,!linenum,!column)
	     else if yytext="fn" then
		Tokens.FN(yytext,!linenum,!column)
             else if yytext="fun" then
                Tokens.FUN(yytext,!linenum,!column)
             else if yytext="int" then
                Tokens.TINT(yytext,!linenum,!column)
             else if yytext="bool" then
                Tokens.TBOOL(yytext,!linenum,!column)
	     else Tokens.ID(yytext,!linenum,!column)));
{alpha}+{vchar}* => (column := !column + size(yytext); Tokens.ID(yytext,!linenum,!column)); 
. => (lex());
