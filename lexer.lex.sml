functor AssignLexFun(structure Tokens:Assign_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure Tokens=Tokens

	type pos = int
	type pol = int
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue, pos) token

	val linenum = ref 1;
	val column = ref 1;
	val eof = fn() => Tokens.EOF(!column,!column)

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\034\036\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\034\033\003\003\003\032\030\003\029\028\027\026\025\024\003\023\
\\021\021\021\021\021\021\021\021\021\021\018\017\014\013\011\003\
\\003\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\003\003\003\003\003\
\\003\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\007\005\004\003\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\016\015\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\020\000\000\019\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 67)], trans = 0},
{fin = [(N 14),(N 67)], trans = 0},
{fin = [(N 67)], trans = 5},
{fin = [(N 53)], trans = 0},
{fin = [(N 12),(N 67)], trans = 0},
{fin = [(N 61),(N 65),(N 67)], trans = 8},
{fin = [(N 61),(N 65)], trans = 8},
{fin = [(N 65)], trans = 10},
{fin = [(N 28),(N 67)], trans = 11},
{fin = [(N 34)], trans = 0},
{fin = [(N 24),(N 67)], trans = 0},
{fin = [(N 26),(N 67)], trans = 14},
{fin = [(N 37)], trans = 0},
{fin = [(N 31)], trans = 0},
{fin = [(N 6),(N 67)], trans = 0},
{fin = [(N 67)], trans = 18},
{fin = [(N 22)], trans = 0},
{fin = [(N 19)], trans = 0},
{fin = [(N 58),(N 67)], trans = 21},
{fin = [(N 58)], trans = 21},
{fin = [(N 45),(N 67)], trans = 0},
{fin = [(N 41),(N 67)], trans = 0},
{fin = [(N 16),(N 67)], trans = 0},
{fin = [(N 39),(N 67)], trans = 0},
{fin = [(N 43),(N 67)], trans = 0},
{fin = [(N 10),(N 67)], trans = 0},
{fin = [(N 8),(N 67)], trans = 0},
{fin = [(N 67)], trans = 30},
{fin = [(N 50)], trans = 0},
{fin = [(N 47),(N 67)], trans = 0},
{fin = [(N 55),(N 67)], trans = 0},
{fin = [(N 4),(N 67)], trans = 34},
{fin = [(N 4)], trans = 34},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (linenum := !linenum + 1; column := 1; lex())
| 10 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.RPAREN(yytext,!linenum,!column) end
| 12 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.LCURLY(yytext,!linenum,!column) end
| 14 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.RCURLY(yytext,!linenum,!column) end
| 16 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.COMMA(yytext,!linenum,!column) end
| 19 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.DCOLON(yytext,!linenum,!column) end
| 22 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.ASSIGN(yytext,!linenum,!column) end
| 24 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.EQ(yytext,!linenum,!column) end
| 26 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.LESSTHAN(yytext,!linenum,!column) end
| 28 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.GREATERTHAN(yytext,!linenum,!column) end
| 31 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.LESSTHANEQ(yytext,!linenum,!column) end
| 34 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.GREATERTHANEQ(yytext,!linenum,!column) end
| 37 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.NOTEQ(yytext,!linenum,!column) end
| 39 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.PLUS(yytext,!linenum,!column) end
| 4 => let val yytext=yymktext() in column := !column + size(yytext); lex() end
| 41 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.MINUS(yytext,!linenum,!column) end
| 43 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.TIMES(yytext,!linenum,!column) end
| 45 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.DIV(yytext,!linenum,!column) end
| 47 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.MOD(yytext,!linenum,!column) end
| 50 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.AND(yytext,!linenum,!column) end
| 53 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.OR(yytext,!linenum,!column) end
| 55 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.NOT(yytext,!linenum,!column) end
| 58 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.INT(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!linenum,!column) end
| 6 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.TERM(yytext,!linenum,!column) end
| 61 => let val yytext=yymktext() in column := !column + size(yytext);(
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
) end
| 65 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.ID(yytext,!linenum,!column) end
| 67 => (lex())
| 8 => let val yytext=yymktext() in column := !column + size(yytext); Tokens.LPAREN(yytext,!linenum,!column) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
