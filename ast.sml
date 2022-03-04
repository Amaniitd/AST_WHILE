structure Ast =
struct

datatype RelOp = LT | LEQ | EQ | NEQ | GEQ | GT

datatype IntOp = PLUS | MINUS | TIMES | DIV | MOD

datatype BoolOp = AND | OR  

datatype UniBoolOp = NOT

datatype Program = PROG of DeclSeq * CmdSeq

and DeclSeq = DSeq of VariableList * DeclSeq | VariableList  

and VariableList = VList of VariableList * string | string 

and CmdSeq = CMDSEQ of CmdSeq * Cmd | Cmd

and Cmd = AssignIntCmd of string * IntExp 
                | AssignBoolCmd of string * BoolExp
				| IfCmd of BoolExp * CmdSeq * CmdSeq
				| WhileCmd of BoolExp * CmdSeq
				| ReadCmd of string
				| WriteCmd of string

and Exp = IntExp of IntExp
				| BoolExp of BoolExp

and IntExp = IntOpExp of IntOp * IntExp * IntExp
				| IntVarExp of string
				| IntConstExp of NUM

and BoolExp = BoolOpExp of BoolOp * BoolExp * BoolExp
				| BoolVarExp of string
				| BoolConstExp of bool
				| BoolCompExp of RelOp * Exp * Exp
				| BoolNotExp of UniBoolOp * BoolExp 


and NUM = Numeral of  char * int | int
                
end