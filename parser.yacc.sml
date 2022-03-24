functor ParseLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Parse_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\037\000\016\000\036\000\018\000\035\000\037\000\034\000\000\000\
\\001\000\002\000\037\000\016\000\036\000\018\000\051\000\030\000\050\000\
\\031\000\049\000\037\000\034\000\038\000\048\000\000\000\
\\001\000\003\000\022\000\007\000\021\000\010\000\020\000\011\000\019\000\
\\016\000\015\000\000\000\
\\001\000\004\000\064\000\036\000\053\000\000\000\
\\001\000\005\000\086\000\000\000\
\\001\000\006\000\088\000\000\000\
\\001\000\008\000\054\000\036\000\053\000\000\000\
\\001\000\009\000\085\000\000\000\
\\001\000\012\000\003\000\000\000\
\\001\000\013\000\010\000\000\000\
\\001\000\014\000\041\000\015\000\040\000\000\000\
\\001\000\016\000\004\000\000\000\
\\001\000\016\000\015\000\000\000\
\\001\000\016\000\028\000\000\000\
\\001\000\016\000\029\000\000\000\
\\001\000\016\000\079\000\000\000\
\\001\000\017\000\025\000\000\000\
\\001\000\018\000\082\000\030\000\050\000\031\000\049\000\038\000\048\000\000\000\
\\001\000\019\000\078\000\036\000\053\000\000\000\
\\001\000\019\000\084\000\030\000\071\000\031\000\070\000\000\000\
\\001\000\020\000\012\000\000\000\
\\001\000\021\000\026\000\000\000\
\\001\000\022\000\005\000\000\000\
\\001\000\023\000\024\000\000\000\
\\001\000\023\000\027\000\000\000\
\\001\000\024\000\063\000\025\000\062\000\026\000\061\000\027\000\060\000\
\\028\000\059\000\029\000\058\000\000\000\
\\001\000\038\000\072\000\000\000\
\\001\000\038\000\073\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\013\000\010\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\039\000\023\000\000\000\
\\099\000\003\000\022\000\007\000\021\000\010\000\020\000\011\000\019\000\
\\016\000\015\000\000\000\
\\100\000\000\000\
\\101\000\030\000\071\000\031\000\070\000\000\000\
\\102\000\036\000\053\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\032\000\068\000\033\000\067\000\034\000\066\000\000\000\
\\108\000\032\000\068\000\033\000\067\000\034\000\066\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\035\000\052\000\000\000\
\\114\000\035\000\052\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\"
val actionRowNumbers =
"\009\000\012\000\023\000\010\000\
\\033\000\032\000\031\000\021\000\
\\013\000\030\000\003\000\037\000\
\\024\000\061\000\017\000\022\000\
\\025\000\014\000\015\000\001\000\
\\001\000\013\000\011\000\038\000\
\\029\000\002\000\043\000\042\000\
\\058\000\053\000\055\000\007\000\
\\001\000\001\000\026\000\056\000\
\\004\000\036\000\035\000\034\000\
\\039\000\051\000\049\000\047\000\
\\041\000\040\000\073\000\027\000\
\\028\000\002\000\001\000\001\000\
\\003\000\059\000\019\000\016\000\
\\066\000\065\000\064\000\063\000\
\\062\000\067\000\003\000\018\000\
\\072\000\071\000\070\000\018\000\
\\069\000\068\000\074\000\075\000\
\\020\000\054\000\052\000\008\000\
\\057\000\060\000\005\000\048\000\
\\018\000\046\000\050\000\045\000\
\\003\000\006\000\044\000\000\000"
val gotoT =
"\
\\001\000\087\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\004\000\005\000\005\000\004\000\000\000\
\\000\000\
\\000\000\
\\002\000\009\000\003\000\006\000\004\000\005\000\005\000\004\000\000\000\
\\000\000\
\\006\000\012\000\007\000\011\000\000\000\
\\000\000\
\\007\000\016\000\008\000\015\000\009\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\031\000\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\011\000\036\000\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\006\000\037\000\007\000\011\000\000\000\
\\000\000\
\\007\000\016\000\008\000\040\000\009\000\014\000\000\000\
\\000\000\
\\010\000\045\000\011\000\044\000\012\000\043\000\013\000\042\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\020\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\053\000\016\000\028\000\000\000\
\\011\000\054\000\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\017\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\063\000\000\000\
\\000\000\
\\018\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\072\000\011\000\054\000\012\000\043\000\013\000\042\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\020\000\041\000\000\000\
\\014\000\073\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\074\000\016\000\028\000\000\000\
\\007\000\016\000\008\000\075\000\009\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\008\000\078\000\009\000\014\000\000\000\
\\013\000\079\000\020\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\081\000\013\000\042\000\020\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\072\000\012\000\043\000\013\000\042\000\020\000\041\000\000\000\
\\019\000\063\000\000\000\
\\000\000\
\\000\000\
\\007\000\016\000\008\000\085\000\009\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 88
val numrules = 47
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | COMMA of unit ->  (string) | INT of unit ->  (int)
 | NOT of unit ->  (string) | OR of unit ->  (string)
 | AND of unit ->  (string) | MOD of unit ->  (string)
 | DIV of unit ->  (string) | TIMES of unit ->  (string)
 | MINUS of unit ->  (string) | PLUS of unit ->  (string)
 | NOTEQ of unit ->  (string) | GREATERTHANEQ of unit ->  (string)
 | LESSTHANEQ of unit ->  (string) | GREATERTHAN of unit ->  (string)
 | LESSTHAN of unit ->  (string) | EQ of unit ->  (string)
 | ASSIGN of unit ->  (string) | DCOLON of unit ->  (string)
 | RCURLY of unit ->  (string) | LCURLY of unit ->  (string)
 | RPAREN of unit ->  (string) | LPAREN of unit ->  (string)
 | TERM of unit ->  (string) | ID of unit ->  (string)
 | TBOOL of unit ->  (string) | TINT of unit ->  (string)
 | VAR of unit ->  (string) | PROGRAM of unit ->  (string)
 | WRITE of unit ->  (string) | READ of unit ->  (string)
 | ENDWH of unit ->  (string) | DO of unit ->  (string)
 | WHILE of unit ->  (string) | ENDIF of unit ->  (string)
 | ELSE of unit ->  (string) | THEN of unit ->  (string)
 | IF of unit ->  (string) | BOOL of unit ->  (bool)
 | Numeral of unit ->  (Ast.Num) | MultOp of unit ->  (Ast.IntOp)
 | AddOp of unit ->  (Ast.IntOp) | RelOp of unit ->  (Ast.RelOp)
 | Comparision of unit ->  (Ast.BoolCompExp)
 | BoolTerm of unit ->  (Ast.BoolExp)
 | BoolFactor of unit ->  (Ast.BoolExp)
 | IntFactor of unit ->  (Ast.IntExp)
 | IntTerm of unit ->  (Ast.IntExp)
 | BoolExp of unit ->  (Ast.BoolExp) | IntExp of unit ->  (Ast.IntExp)
 | Cmd of unit ->  (Ast.Cmd) | CmdSeq of unit ->  (Ast.CmdSeq)
 | Variable of unit ->  (string)
 | VariableList of unit ->  (Ast.VariableList)
 | BoolDecl of unit ->  (Ast.VariableList)
 | IntDecl of unit ->  (Ast.VariableList)
 | Decl of unit ->  (Ast.VariableList)
 | DeclSeq of unit ->  (Ast.DeclSeq)
 | program of unit ->  (Ast.Program)
end
type svalue = MlyValue.svalue
type result = Ast.Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "BOOL"
  | (T 2) => "IF"
  | (T 3) => "THEN"
  | (T 4) => "ELSE"
  | (T 5) => "ENDIF"
  | (T 6) => "WHILE"
  | (T 7) => "DO"
  | (T 8) => "ENDWH"
  | (T 9) => "READ"
  | (T 10) => "WRITE"
  | (T 11) => "PROGRAM"
  | (T 12) => "VAR"
  | (T 13) => "TINT"
  | (T 14) => "TBOOL"
  | (T 15) => "ID"
  | (T 16) => "TERM"
  | (T 17) => "LPAREN"
  | (T 18) => "RPAREN"
  | (T 19) => "LCURLY"
  | (T 20) => "RCURLY"
  | (T 21) => "DCOLON"
  | (T 22) => "ASSIGN"
  | (T 23) => "EQ"
  | (T 24) => "LESSTHAN"
  | (T 25) => "GREATERTHAN"
  | (T 26) => "LESSTHANEQ"
  | (T 27) => "GREATERTHANEQ"
  | (T 28) => "NOTEQ"
  | (T 29) => "PLUS"
  | (T 30) => "MINUS"
  | (T 31) => "TIMES"
  | (T 32) => "DIV"
  | (T 33) => "MOD"
  | (T 34) => "AND"
  | (T 35) => "OR"
  | (T 36) => "NOT"
  | (T 37) => "INT"
  | (T 38) => "COMMA"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.RCURLY RCURLY1, _, RCURLY1right)) :: ( _, (
 MlyValue.CmdSeq CmdSeq1, _, _)) :: ( _, ( MlyValue.LCURLY LCURLY1, _,
 _)) :: ( _, ( MlyValue.DeclSeq DeclSeq1, _, _)) :: ( _, ( 
MlyValue.DCOLON DCOLON1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, _)) :: rest671)) => let
 val  result = MlyValue.program (fn _ => let val  PROGRAM1 = PROGRAM1
 ()
 val  ID1 = ID1 ()
 val  DCOLON1 = DCOLON1 ()
 val  (DeclSeq as DeclSeq1) = DeclSeq1 ()
 val  LCURLY1 = LCURLY1 ()
 val  (CmdSeq as CmdSeq1) = CmdSeq1 ()
 val  RCURLY1 = RCURLY1 ()
 in (Ast.Program(DeclSeq, CmdSeq))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, RCURLY1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.DeclSeq DeclSeq1, _, DeclSeq1right)) :: ( _,
 ( MlyValue.Decl Decl1, Decl1left, _)) :: rest671)) => let val  result
 = MlyValue.DeclSeq (fn _ => let val  (Decl as Decl1) = Decl1 ()
 val  (DeclSeq as DeclSeq1) = DeclSeq1 ()
 in (Ast.DeclSeq(Decl, DeclSeq))
end)
 in ( LrTable.NT 1, ( result, Decl1left, DeclSeq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Decl Decl1, Decl1left, Decl1right)) :: 
rest671)) => let val  result = MlyValue.DeclSeq (fn _ => let val  (
Decl as Decl1) = Decl1 ()
 in (Ast.DeclSeq(Decl, Ast.EOF))
end)
 in ( LrTable.NT 1, ( result, Decl1left, Decl1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.IntDecl IntDecl1, IntDecl1left, 
IntDecl1right)) :: rest671)) => let val  result = MlyValue.Decl (fn _
 => let val  (IntDecl as IntDecl1) = IntDecl1 ()
 in (IntDecl)
end)
 in ( LrTable.NT 2, ( result, IntDecl1left, IntDecl1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.BoolDecl BoolDecl1, BoolDecl1left, 
BoolDecl1right)) :: rest671)) => let val  result = MlyValue.Decl (fn _
 => let val  (BoolDecl as BoolDecl1) = BoolDecl1 ()
 in (BoolDecl)
end)
 in ( LrTable.NT 2, ( result, BoolDecl1left, BoolDecl1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.TINT TINT1, _, TINT1right)) :: ( _, ( 
MlyValue.ASSIGN ASSIGN1, _, _)) :: ( _, ( MlyValue.VariableList 
VariableList1, _, _)) :: ( _, ( MlyValue.VAR VAR1, VAR1left, _)) :: 
rest671)) => let val  result = MlyValue.IntDecl (fn _ => let val  VAR1
 = VAR1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 val  ASSIGN1 = ASSIGN1 ()
 val  TINT1 = TINT1 ()
 in (VariableList)
end)
 in ( LrTable.NT 3, ( result, VAR1left, TINT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.TBOOL TBOOL1, _, TBOOL1right)) :: ( _, ( 
MlyValue.ASSIGN ASSIGN1, _, _)) :: ( _, ( MlyValue.VariableList 
VariableList1, _, _)) :: ( _, ( MlyValue.VAR VAR1, VAR1left, _)) :: 
rest671)) => let val  result = MlyValue.BoolDecl (fn _ => let val  
VAR1 = VAR1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 val  ASSIGN1 = ASSIGN1 ()
 val  TBOOL1 = TBOOL1 ()
 in (VariableList)
end)
 in ( LrTable.NT 4, ( result, VAR1left, TBOOL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: ( _, ( MlyValue.COMMA COMMA1, _, _)) :: ( _, (
 MlyValue.Variable Variable1, Variable1left, _)) :: rest671)) => let
 val  result = MlyValue.VariableList (fn _ => let val  (Variable as 
Variable1) = Variable1 ()
 val  COMMA1 = COMMA1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (Ast.VariableList(VariableList, Variable))
end)
 in ( LrTable.NT 5, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (Ast.VariableList(Variable, Ast.EOF))
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: ( _, ( 
MlyValue.Cmd Cmd1, Cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.CmdSeq (fn _ => let val  (Cmd as Cmd1) = Cmd1 ()
 val  TERM1 = TERM1 ()
 in (Ast.CmdSeq(Cmd, Ast.EOF))
end)
 in ( LrTable.NT 7, ( result, Cmd1left, TERM1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.CmdSeq CmdSeq1, _, CmdSeq1right)) :: ( _, (
 MlyValue.TERM TERM1, _, _)) :: ( _, ( MlyValue.Cmd Cmd1, Cmd1left, _)
) :: rest671)) => let val  result = MlyValue.CmdSeq (fn _ => let val 
 (Cmd as Cmd1) = Cmd1 ()
 val  TERM1 = TERM1 ()
 val  (CmdSeq as CmdSeq1) = CmdSeq1 ()
 in (Ast.CmdSeq(Cmd, CmdSeq))
end)
 in ( LrTable.NT 7, ( result, Cmd1left, CmdSeq1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.IntExp IntExp1, _, IntExp1right)) :: ( _, (
 MlyValue.ASSIGN ASSIGN1, _, _)) :: ( _, ( MlyValue.Variable Variable1
, Variable1left, _)) :: rest671)) => let val  result = MlyValue.Cmd
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 val  ASSIGN1 = ASSIGN1 ()
 val  (IntExp as IntExp1) = IntExp1 ()
 in (Ast.AssignIntCmd(Variable, IntExp))
end)
 in ( LrTable.NT 8, ( result, Variable1left, IntExp1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.BoolExp BoolExp1, _, BoolExp1right)) :: ( _
, ( MlyValue.ASSIGN ASSIGN1, _, _)) :: ( _, ( MlyValue.Variable 
Variable1, Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmd (fn _ => let val  (Variable as Variable1) = Variable1 ()
 val  ASSIGN1 = ASSIGN1 ()
 val  (BoolExp as BoolExp1) = BoolExp1 ()
 in (Ast.AssignBoolCom(Variable, BoolExp))
end)
 in ( LrTable.NT 8, ( result, Variable1left, BoolExp1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.READ READ1, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmd (fn _ => let val  READ1 = READ1 ()
 val  (ID as ID1) = ID1 ()
 in (Ast.ReadCmd(ID))
end)
 in ( LrTable.NT 8, ( result, READ1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.WRITE WRITE1, WRITE1left, _)) :: rest671)) => let val  result
 = MlyValue.Cmd (fn _ => let val  WRITE1 = WRITE1 ()
 val  (ID as ID1) = ID1 ()
 in (Ast.WriteCmd(ID))
end)
 in ( LrTable.NT 8, ( result, WRITE1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ENDIF ENDIF1, _, ENDIF1right)) :: ( _, ( 
MlyValue.CmdSeq CmdSeq2, _, _)) :: ( _, ( MlyValue.ELSE ELSE1, _, _))
 :: ( _, ( MlyValue.CmdSeq CmdSeq1, _, _)) :: ( _, ( MlyValue.THEN 
THEN1, _, _)) :: ( _, ( MlyValue.BoolExp BoolExp1, _, _)) :: ( _, ( 
MlyValue.IF IF1, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmd (fn _ => let val  IF1 = IF1 ()
 val  (BoolExp as BoolExp1) = BoolExp1 ()
 val  THEN1 = THEN1 ()
 val  CmdSeq1 = CmdSeq1 ()
 val  ELSE1 = ELSE1 ()
 val  CmdSeq2 = CmdSeq2 ()
 val  ENDIF1 = ENDIF1 ()
 in (Ast.IfCmd(BoolExp, CmdSeq1, CmdSeq2))
end)
 in ( LrTable.NT 8, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ENDWH ENDWH1, _, ENDWH1right)) :: ( _, ( 
MlyValue.CmdSeq CmdSeq1, _, _)) :: ( _, ( MlyValue.DO DO1, _, _)) :: (
 _, ( MlyValue.BoolExp BoolExp1, _, _)) :: ( _, ( MlyValue.WHILE 
WHILE1, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.Cmd
 (fn _ => let val  WHILE1 = WHILE1 ()
 val  (BoolExp as BoolExp1) = BoolExp1 ()
 val  DO1 = DO1 ()
 val  (CmdSeq as CmdSeq1) = CmdSeq1 ()
 val  ENDWH1 = ENDWH1 ()
 in (Ast.WhileCmd(BoolExp, CmdSeq))
end)
 in ( LrTable.NT 8, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: ( _
, ( MlyValue.AddOp AddOp1, _, _)) :: ( _, ( MlyValue.IntExp IntExp1, 
IntExp1left, _)) :: rest671)) => let val  result = MlyValue.IntExp (fn
 _ => let val  (IntExp as IntExp1) = IntExp1 ()
 val  (AddOp as AddOp1) = AddOp1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in (Ast.IntOpExp(AddOp, IntExp, IntTerm))
end)
 in ( LrTable.NT 9, ( result, IntExp1left, IntTerm1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, 
IntTerm1right)) :: rest671)) => let val  result = MlyValue.IntExp (fn
 _ => let val  (IntTerm as IntTerm1) = IntTerm1 ()
 in (IntTerm)
end)
 in ( LrTable.NT 9, ( result, IntTerm1left, IntTerm1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( MlyValue.MultOp MultOp1, _, _)) :: ( _, ( MlyValue.IntTerm 
IntTerm1, IntTerm1left, _)) :: rest671)) => let val  result = 
MlyValue.IntTerm (fn _ => let val  (IntTerm as IntTerm1) = IntTerm1 ()
 val  (MultOp as MultOp1) = MultOp1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (Ast.IntOpExp(MultOp, IntTerm, IntFactor))
end)
 in ( LrTable.NT 11, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 20, ( ( _, ( MlyValue.IntFactor IntFactor1, IntFactor1left, 
IntFactor1right)) :: rest671)) => let val  result = MlyValue.IntTerm
 (fn _ => let val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (IntFactor)
end)
 in ( LrTable.NT 11, ( result, IntFactor1left, IntFactor1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.IntExp IntExp1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  LPAREN1 = LPAREN1 ()
 val  (IntExp as IntExp1) = IntExp1 ()
 val  RPAREN1 = RPAREN1 ()
 in (IntExp)
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.Numeral Numeral1, Numeral1left, 
Numeral1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Numeral as Numeral1) = Numeral1 ()
 in (Ast.IntConstExp(Numeral))
end)
 in ( LrTable.NT 12, ( result, Numeral1left, Numeral1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.BoolTerm BoolTerm1, _, BoolTerm1right)) :: 
( _, ( MlyValue.OR OR1, _, _)) :: ( _, ( MlyValue.BoolExp BoolExp1, 
BoolExp1left, _)) :: rest671)) => let val  result = MlyValue.BoolExp
 (fn _ => let val  (BoolExp as BoolExp1) = BoolExp1 ()
 val  (OR as OR1) = OR1 ()
 val  (BoolTerm as BoolTerm1) = BoolTerm1 ()
 in (Ast.BoolOpExp(Ast.OR, BoolExp, BoolTerm))
end)
 in ( LrTable.NT 10, ( result, BoolExp1left, BoolTerm1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.BoolTerm BoolTerm1, BoolTerm1left, 
BoolTerm1right)) :: rest671)) => let val  result = MlyValue.BoolExp
 (fn _ => let val  (BoolTerm as BoolTerm1) = BoolTerm1 ()
 in (BoolTerm)
end)
 in ( LrTable.NT 10, ( result, BoolTerm1left, BoolTerm1right), rest671
)
end
|  ( 25, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( MlyValue.AND AND1, _, _)) :: ( _, ( MlyValue.BoolTerm 
BoolTerm1, BoolTerm1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolTerm (fn _ => let val  (BoolTerm as BoolTerm1) = 
BoolTerm1 ()
 val  (AND as AND1) = AND1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (Ast.BoolOpExp(Ast.AND, BoolTerm, BoolFactor))
end)
 in ( LrTable.NT 14, ( result, BoolTerm1left, BoolFactor1right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.BoolFactor BoolFactor1, BoolFactor1left, 
BoolFactor1right)) :: rest671)) => let val  result = MlyValue.BoolTerm
 (fn _ => let val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (BoolFactor)
end)
 in ( LrTable.NT 14, ( result, BoolFactor1left, BoolFactor1right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.BoolFactor (fn _ => let val  (
BOOL as BOOL1) = BOOL1 ()
 in (Ast.BoolConstExp(BOOL))
end)
 in ( LrTable.NT 13, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.BoolExp BoolExp1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.BoolFactor
 (fn _ => let val  LPAREN1 = LPAREN1 ()
 val  (BoolExp as BoolExp1) = BoolExp1 ()
 val  RPAREN1 = RPAREN1 ()
 in (BoolExp)
end)
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.Comparision Comparision1, Comparision1left,
 Comparision1right)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (Comparision as Comparision1) = 
Comparision1 ()
 in (Comparision)
end)
 in ( LrTable.NT 13, ( result, Comparision1left, Comparision1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( MlyValue.NOT NOT1, NOT1left, _)) :: rest671)) => let val 
 result = MlyValue.BoolFactor (fn _ => let val  (NOT as NOT1) = NOT1
 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (Ast.BoolNotExp(Ast.NOT, BoolFactor))
end)
 in ( LrTable.NT 13, ( result, NOT1left, BoolFactor1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: ( _, ( 
MlyValue.RelOp RelOp1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.Comparision (fn _ => let
 val  ID1 = ID1 ()
 val  (RelOp as RelOp1) = RelOp1 ()
 val  ID2 = ID2 ()
 in (Ast.BoolCompExp(RelOp, IntExp1, IntExp2))
end)
 in ( LrTable.NT 15, ( result, ID1left, ID2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Variable (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (Ast.ID)
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.LESSTHAN LESSTHAN1, LESSTHAN1left, 
LESSTHAN1right)) :: rest671)) => let val  result = MlyValue.RelOp (fn
 _ => let val  LESSTHAN1 = LESSTHAN1 ()
 in (Ast.LT)
end)
 in ( LrTable.NT 16, ( result, LESSTHAN1left, LESSTHAN1right), rest671
)
end
|  ( 34, ( ( _, ( MlyValue.GREATERTHAN GREATERTHAN1, GREATERTHAN1left,
 GREATERTHAN1right)) :: rest671)) => let val  result = MlyValue.RelOp
 (fn _ => let val  GREATERTHAN1 = GREATERTHAN1 ()
 in (Ast.GT)
end)
 in ( LrTable.NT 16, ( result, GREATERTHAN1left, GREATERTHAN1right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.LESSTHANEQ LESSTHANEQ1, LESSTHANEQ1left, 
LESSTHANEQ1right)) :: rest671)) => let val  result = MlyValue.RelOp
 (fn _ => let val  LESSTHANEQ1 = LESSTHANEQ1 ()
 in (Ast.LEQ)
end)
 in ( LrTable.NT 16, ( result, LESSTHANEQ1left, LESSTHANEQ1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.GREATERTHANEQ GREATERTHANEQ1, 
GREATERTHANEQ1left, GREATERTHANEQ1right)) :: rest671)) => let val  
result = MlyValue.RelOp (fn _ => let val  GREATERTHANEQ1 = 
GREATERTHANEQ1 ()
 in (Ast.GEQ)
end)
 in ( LrTable.NT 16, ( result, GREATERTHANEQ1left, GREATERTHANEQ1right
), rest671)
end
|  ( 37, ( ( _, ( MlyValue.NOTEQ NOTEQ1, NOTEQ1left, NOTEQ1right)) :: 
rest671)) => let val  result = MlyValue.RelOp (fn _ => let val  NOTEQ1
 = NOTEQ1 ()
 in (Ast.NEQ)
end)
 in ( LrTable.NT 16, ( result, NOTEQ1left, NOTEQ1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.EQ EQ1, EQ1left, EQ1right)) :: rest671)) =>
 let val  result = MlyValue.RelOp (fn _ => let val  (EQ as EQ1) = EQ1
 ()
 in (Ast.EQ)
end)
 in ( LrTable.NT 16, ( result, EQ1left, EQ1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.PLUS PLUS1, PLUS1left, PLUS1right)) :: 
rest671)) => let val  result = MlyValue.AddOp (fn _ => let val  (PLUS
 as PLUS1) = PLUS1 ()
 in (Ast.PLUS)
end)
 in ( LrTable.NT 17, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.MINUS MINUS1, MINUS1left, MINUS1right)) :: 
rest671)) => let val  result = MlyValue.AddOp (fn _ => let val  (MINUS
 as MINUS1) = MINUS1 ()
 in (ASt.MINUS)
end)
 in ( LrTable.NT 17, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.TIMES TIMES1, TIMES1left, TIMES1right)) :: 
rest671)) => let val  result = MlyValue.MultOp (fn _ => let val  (
TIMES as TIMES1) = TIMES1 ()
 in (Ast.TIMES)
end)
 in ( LrTable.NT 18, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.DIV DIV1, DIV1left, DIV1right)) :: rest671)
) => let val  result = MlyValue.MultOp (fn _ => let val  (DIV as DIV1)
 = DIV1 ()
 in (Ast.DIV)
end)
 in ( LrTable.NT 18, ( result, DIV1left, DIV1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.MOD MOD1, MOD1left, MOD1right)) :: rest671)
) => let val  result = MlyValue.MultOp (fn _ => let val  (MOD as MOD1)
 = MOD1 ()
 in (Ast.MOD)
end)
 in ( LrTable.NT 18, ( result, MOD1left, MOD1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.Numeral (fn _ => let val  (INT as INT1
) = INT1 ()
 in (Ast.NUM(INT, Ast.EOF()))
end)
 in ( LrTable.NT 19, ( result, INT1left, INT1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.INT INT1, _, INT1right)) :: ( _, ( 
MlyValue.MINUS MINUS1, MINUS1left, _)) :: rest671)) => let val  result
 = MlyValue.Numeral (fn _ => let val  MINUS1 = MINUS1 ()
 val  (INT as INT1) = INT1 ()
 in (Ast.NUM(INT, "-"))
end)
 in ( LrTable.NT 19, ( result, MINUS1left, INT1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.INT INT1, _, INT1right)) :: ( _, ( 
MlyValue.PLUS PLUS1, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.Numeral (fn _ => let val  PLUS1 = PLUS1 ()
 val  (INT as INT1) = INT1 ()
 in (Ast.NUM(INT, "+"))
end)
 in ( LrTable.NT 19, ( result, PLUS1left, INT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Parse_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun IF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.IF (fn () => i),p1,p2))
fun THEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.THEN (fn () => i),p1,p2))
fun ELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.ELSE (fn () => i),p1,p2))
fun ENDIF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.ENDIF (fn () => i),p1,p2))
fun WHILE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.WHILE (fn () => i),p1,p2))
fun DO (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.DO (fn () => i),p1,p2))
fun ENDWH (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.ENDWH (fn () => i),p1,p2))
fun READ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.READ (fn () => i),p1,p2))
fun WRITE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.WRITE (fn () => i),p1,p2))
fun PROGRAM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.PROGRAM (fn () => i),p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
fun TINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.TINT (fn () => i),p1,p2))
fun TBOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.TBOOL (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun TERM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.TERM (fn () => i),p1,p2))
fun LPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.LPAREN (fn () => i),p1,p2))
fun RPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.RPAREN (fn () => i),p1,p2))
fun LCURLY (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.LCURLY (fn () => i),p1,p2))
fun RCURLY (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.RCURLY (fn () => i),p1,p2))
fun DCOLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.DCOLON (fn () => i),p1,p2))
fun ASSIGN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.ASSIGN (fn () => i),p1,p2))
fun EQ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.EQ (fn () => i),p1,p2))
fun LESSTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.LESSTHAN (fn () => i),p1,p2))
fun GREATERTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.GREATERTHAN (fn () => i),p1,p2))
fun LESSTHANEQ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.LESSTHANEQ (fn () => i),p1,p2))
fun GREATERTHANEQ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.GREATERTHANEQ (fn () => i),p1,p2))
fun NOTEQ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.NOTEQ (fn () => i),p1,p2))
fun PLUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.PLUS (fn () => i),p1,p2))
fun MINUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.MINUS (fn () => i),p1,p2))
fun TIMES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.TIMES (fn () => i),p1,p2))
fun DIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.DIV (fn () => i),p1,p2))
fun MOD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.MOD (fn () => i),p1,p2))
fun AND (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.AND (fn () => i),p1,p2))
fun OR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.OR (fn () => i),p1,p2))
fun NOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.NOT (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun COMMA (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.COMMA (fn () => i),p1,p2))
end
end
