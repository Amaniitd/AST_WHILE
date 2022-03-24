

%%

%name Parse

%term EOF | BOOL of bool | IF of string | THEN of string | ELSE of string | ENDIF of string | WHILE of string | DO of string | ENDWH of string | READ of string | WRITE of string | PROGRAM of string | VAR of string | TINT of string | TBOOL of string | ID of string | TERM of string | LPAREN of string | RPAREN of string | LCURLY of string | RCURLY of string | DCOLON of string | ASSIGN of string | EQ of string | LESSTHAN of string | GREATERTHAN of string | LESSTHANEQ of string | GREATERTHANEQ of string | NOTEQ of string | PLUS of string | MINUS of string | TIMES of string | DIV of string | MOD of string | AND of string | OR of string | NOT of string | INT of int | COMMA of string

%nonterm program of Ast.Program | DeclSeq of Ast.DeclSeq | IntDecl of Ast.VariableList | BoolDecl of Ast.VariableList | IntVariableList of Ast.VariableList | BoolVariableList of Ast.VariableList | IntVariable of string | BoolVariable of string | CmdSeq of Ast.CmdSeq | Cmd of Ast.Cmd | IntExp of Ast.IntExp | BoolExp of Ast.BoolExp | IntTerm of Ast.IntExp | IntFactor of Ast.IntExp | BoolFactor of Ast.BoolExp | BoolTerm of Ast.BoolExp | Comparision of Ast.BoolCompExp | RelOp of Ast.RelOp | AddOp of Ast.IntOp | MultOp of Ast.IntOp | Numeral of Ast.Num 

%pos int
%eop EOF
%noshift EOF 



%start program
%verbose

%%

program : PROGRAM ID DCOLON DeclSeq LCURLY CmdSeq RCURLY (Ast.Program(DeclSeq, CmdSeq))

DeclSeq : Decl DeclSeq (Ast.DeclSeq(Decl, DeclSeq))
    | Decl (Ast.DeclSeq(Decl, Ast.EOF()))

Decl : IntDecl (IntDecl)
    | BoolDecl (BoolDecl)

IntDecl : VAR IntVariableList ASSIGN TINT  (IntVariableList)
BoolDecl : VAR BoolVariableList ASSIGN TBOOL (BoolVariableList)

IntVariableList : IntVariable COMMA IntVariableList (Ast.VariableList(IntVariableList, IntVariable))
    | IntVariable (Ast.VariableList(IntVariable, Ast.EOF()))

BoolVariableList : BoolVariable COMMA BoolVariableList (Ast.VariableList(BoolVariableList, BoolVariable))
    | BoolVariable (Ast.VariableList(BoolVariable, Ast.EOF()))

CmdSeq : Cmd TERM  (Ast.CmdSeq(Cmd, Ast.EOF()))
    | Cmd Term CmdSeq (Ast.CmdSeq(CmdSeq, Cmd))

Cmd : IntVariable ASSIGN IntExp (Ast.AssignIntCmd(IntVariable, IntExp))
    | BoolVariable ASSIGN BoolExp (Ast.AssignBoolCom(BoolVariable, BoolExp))
    | READ IntVariable (Ast.ReadCmd(IntVariable))
    | WRITE IntExp (Ast.WriteCmd(IntExp))
    | IF BoolExp THEN CmdSeq ELSE CmdSeq ENDIF (Ast.IfCmd(BoolExp, CmdSeq1, CmdSeq2))
    | WHILE BoolExp DO CmdSeq ENDWH (Ast.WhileCmd(BoolExp, CmdSeq))

IntExp : IntExp AddOp IntTerm (Ast.IntOpExp(AddOp, IntExp, IntTerm))
    | IntTerm 

IntTerm : IntTerm MultOp IntFactor (Ast.IntOpExp(MultOp, IntTerm, IntFactor))
    | IntFactor (IntFactor)

IntFactor : LPAREN IntExp RPAREN (IntExp)
    | IntVariable (Ast.IntVarExp(IntVariable))
    | Numeral (Ast.IntConstExp(Numeral))

BoolExp :BoolExp OR BoolTerm (Ast.BoolOpExp(OR, BoolExp, BoolTerm))
    | BoolTerm (BoolTerm)

BoolTerm : BoolTerm AND BoolFactor (Ast.BoolOpExp(AND, BoolTerm, BoolFactor))
    | BoolFactor (BoolFactor)

BoolFactor : TRUE (Ast.BoolConstExp(TRUE))
    | FALSE (Ast.BoolConstExp(FALSE))
    | LPAREN BoolExp RPAREN 
    | BoolVariable (Ast.BoolVarExp(BoolVariable))
    | Comparision (Comparision)
    | NOT BoolFactor (Ast.BoolNotExp(NOT, BoolFactor))

Comparision : IntExp RelOp IntExp (Ast.BoolCompExp(RelOp, IntExp1, IntExp2))
    | BoolExp RelOp BoolExp (Ast.BoolCompExp(RelOp, BoolExp1, BoolExp2))

IntVariable : ID

BoolVariable : ID

RelOp : LESSTHAN
    | GREATERTHAN
    | LESSTHANEQ
    | GREATERTHANEQ
    | NOTEQ
    | EQ

AddOp : PLUS
    | MINUS

MultOp : TIMES
    | DIV
    | MOD

Numeral : INT (Ast.NUM(INT, Ast.EOF()))
    | MINUS INT (Ast.NUM('+', INT))
    | PLUS INT (Ast.NUM('-', INT))

    