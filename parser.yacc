

%%

%name Parse

%term EOF | BOOL of bool | IF of string | THEN of string | ELSE of string | ENDIF of string | WHILE of string | DO of string | ENDWH of string | READ of string | WRITE of string | PROGRAM of string | VAR of string | TINT of string | TBOOL of string | ID of string | TERM of string | LPAREN of string | RPAREN of string | LCURLY of string | RCURLY of string | DCOLON of string | ASSIGN of string | EQ of string | LESSTHAN of string | GREATERTHAN of string | LESSTHANEQ of string | GREATERTHANEQ of string | NOTEQ of string | PLUS of string | MINUS of string | TIMES of string | DIV of string | MOD of string | AND of string | OR of string | NOT of string | INT of int | COMMA of string 

%nonterm program of Ast.Program | DeclSeq of Ast.DeclSeq | Decl of Ast.VariableList | IntDecl of Ast.VariableList | BoolDecl of Ast.VariableList | VariableList of Ast.VariableList | Variable of string | CmdSeq of Ast.CmdSeq | Cmd of Ast.Cmd | IntExp of Ast.IntExp | BoolExp of Ast.BoolExp | IntTerm of Ast.IntExp | IntFactor of Ast.IntExp | BoolFactor of Ast.BoolExp | BoolTerm of Ast.BoolExp | Comparision of Ast.BoolCompExp | RelOp of Ast.RelOp | AddOp of Ast.IntOp | MultOp of Ast.IntOp | Numeral of Ast.Num 

%pos int
%eop EOF
%noshift EOF 

%left AND OR
%right IF THEN ELSE ENDIF WHILE DO ENDWH 
%left PLUS MINUS TIMES DIV MOD TERM
%right ASSIGN

%start program
%verbose

%%

program : PROGRAM ID DCOLON DeclSeq LCURLY CmdSeq RCURLY (Ast.Program(DeclSeq, CmdSeq))

DeclSeq : Decl DeclSeq (Ast.DeclSeq(Decl, DeclSeq))
    | Decl (Ast.DeclSeq(Decl, Ast.EOF))

Decl : IntDecl (IntDecl)
    | BoolDecl (BoolDecl)

IntDecl : VAR VariableList ASSIGN TINT  (VariableList)
BoolDecl : VAR VariableList ASSIGN TBOOL (VariableList)

VariableList : Variable COMMA VariableList (Ast.VariableList(VariableList, Variable))
    | Variable (Ast.VariableList(Variable, Ast.EOF))

CmdSeq : Cmd TERM  (Ast.CmdSeq(Cmd, Ast.EOF))
    | Cmd TERM CmdSeq (Ast.CmdSeq(Cmd, CmdSeq))

Cmd : Variable ASSIGN IntExp (Ast.AssignIntCmd(Variable, IntExp))
    | Variable ASSIGN BoolExp (Ast.AssignBoolCom(Variable, BoolExp))
    | READ ID (Ast.ReadCmd(ID))
    | WRITE ID (Ast.WriteCmd(ID))
    | IF BoolExp THEN CmdSeq ELSE CmdSeq ENDIF (Ast.IfCmd(BoolExp, CmdSeq1, CmdSeq2))
    | WHILE BoolExp DO CmdSeq ENDWH (Ast.WhileCmd(BoolExp, CmdSeq))

IntExp : IntExp AddOp IntTerm (Ast.IntOpExp(AddOp, IntExp, IntTerm))
    | IntTerm  (IntTerm)

IntTerm : IntTerm MultOp IntFactor (Ast.IntOpExp(MultOp, IntTerm, IntFactor))
    | IntFactor (IntFactor)

IntFactor : LPAREN IntExp RPAREN (IntExp)
    | Numeral (Ast.IntConstExp(Numeral))

BoolExp :BoolExp OR BoolTerm (Ast.BoolOpExp(Ast.OR, BoolExp, BoolTerm))
    | BoolTerm (BoolTerm)

BoolTerm : BoolTerm AND BoolFactor (Ast.BoolOpExp(Ast.AND, BoolTerm, BoolFactor))
    | BoolFactor (BoolFactor)

BoolFactor : BOOL (Ast.BoolConstExp(BOOL))
    | LPAREN BoolExp RPAREN  (BoolExp)
    | Comparision (Comparision)
    | NOT BoolFactor (Ast.BoolNotExp(Ast.NOT, BoolFactor))

Comparision : ID RelOp ID (Ast.BoolCompExp(RelOp, IntExp1, IntExp2))

Variable : ID (Ast.ID)


RelOp : LESSTHAN (Ast.LT)
    | GREATERTHAN (Ast.GT)
    | LESSTHANEQ (Ast.LEQ)
    | GREATERTHANEQ (Ast.GEQ)
    | NOTEQ (Ast.NEQ)
    | EQ    (Ast.EQ)

AddOp : PLUS (Ast.PLUS)
    | MINUS (ASt.MINUS)

MultOp : TIMES (Ast.TIMES)
    | DIV (Ast.DIV)
    | MOD (Ast.MOD)

Numeral : INT (Ast.NUM(INT, Ast.EOF()))
    | MINUS INT (Ast.NUM(INT, "-"))
    | PLUS INT (Ast.NUM(INT, "+"))

    