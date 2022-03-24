
(*  *)

%%



%name While
%arg (fileName) : string
%eop EOF
%noshift EOF
%verbose

%term
    EOF |
    PROG |  VAR |  INTTYPE |  BOOLTYPE |  COMMA |  COLON | TWOCOLONS |  SEMICOLON | LBRACES |  RBRACES |  LPAREN |
    RPAREN |  ASSIGNMENT |  READ |  WRITE |  IF | THEN | ELSE |  ENDIF |  WHILE |  DO |  ENDWH |  TT |  FF |  NOT |
    AND |  OR |  LT |  LEQ |  EQ |  GT |  GEQ |  NEQ |  PLUS |  MINUS |  TIMES |  DIV |  MOD | TILDE |
    IDENTIFIER of string |
    NUMERAL of int
    
(* %nonterm
    START | BLK | DECSEQ | DEC | TYPE | VARLIST | CMDSEQ |
    CMDS | CMD | EXPRESSION | IEXP | INTTERM | INTFACTOR |
    BEXP | BOOLTERM | BOOLFACTOR | COMPARISON | VARIABLE |
    RELOP | ADDOP | MULTOP *)

%nonterm
    START | BLK | DECSEQ | DEC | TYPE | VARLIST | CMDSEQ |
    CMDS | CMD | EXPRESSION | ANDEXP | EQEXP | RELEXP | ADDEXP |
    MULTEXP | CASTEXP | PRIMARYEXP


%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV

%right ASSIGNMENT
%pos int


%value NUMERAL (IntInf.fromInt 1)
%value IDENTIFIER ("")


%%

START   :   PROG IDENTIFIER TWOCOLONS BLK       (Ast.program(identifier,BLK))
BLK     :   DECSEQ CMDSEQ                       (DECSEQ,CMDSEQ)
DECSEQ  :   DEC DECSEQ                          (DEC::DECSEQ)
            |                                   ([])                 
DEC     :   VAR IDENTIFIER VARLIST COLON TYPE SEMICOLON     (Ast.decl(IDENTIFIER::VARLIST,TYPE))
TYPE    :   INTTYPE                             (Ast.Type(INTTYPE))
            | BOOLTYPE                          (Ast.Type(BOOLTYPE))
VARLIST :   COMMA IDENTIFIER VARLIST            (IDENTIFIER::VARLIST)
            |                                   ([])
CMDSEQ  :   LBRACES CMDS RBRACES                (CMDS)
CMDS    :   CMD SEMICOLON CMDS                  (CMD::CMDS)
            |                                   ([])
CMD     :   IDENTIFIER ASSIGNMENT EXPRESSION    (Ast.Assign(IDENTIFIER,EXPRESSION))
            | READ IDENTIFIER                   (Ast.Read(IDENTIFIER))
            | WRITE EXPRESSION                  (Ast.Write(EXPRESSION))
            | IF EXPRESSION THEN CMDSEQ ELSE CMDSEQ ENDIF   (Ast.If(EXPRESSION,CMDSEQ,CMDSEQ))
            | WHILE EXPRESSION DO CMDSEQ ENDWH  (Ast.While(EXPRESSION,CMDSEQ))

EXPRESSION: EXPRESSION OR ANDEXP                (Ast.or_op(EXPRESSION,ANDEXP))
            | ANDEXP                            (ANDEXP)

ANDEXP  :   ANDEXP AND EQEXP                    (Ast.and_op(ANDEXP,EQEXP))
            | EQEXP                             (EQEXP)

EQEXP   :   EQEXP EQ RELEXP                     (Ast.eq_op(EQEXP,RELEXP))
            | EQEXP NEQ RELEXP                  (Ast.neq_op(EQEXP,RELEXP))
            | RELEXP                            (RELEXP)

RELEXP  :   ADDEXP                              (ADDEXP)
            | RELEXP LT ADDEXP                  (Ast.lt_op(RELEXP,ADDEXP))
            | RELEXP LEQ ADDEXP                 (Ast.leq_op(RELEXP,ADDEXP))
            | RELEXP GT ADDEXP                  (Ast.gt_op(RELEXP,ADDEXP))
            | RELEXP GEQ ADDEXP                 (Ast.geq_op(RELEXP,ADDEXP))

ADDEXP  :   MULTEXP                             (MULTEXP)
            | ADDEXP  PLUS MULTEXP              (Ast.plus(ADDEXP,MULTEXP))
            | ADDEXP MINUS MULTEXP              (Ast.minus(ADDEXP,MULTEXP))

MULTEXP :   CASTEXP                             (CASTEXP)
            | MULTEXP TIMES CASTEXP             (Ast.times(MULTEXP,CASTEXP))
            | MULTEXP DIV CASTEXP               (Ast.divide(MULTEXP,CASTEXP))
            | MULTEXP MOD CASTEXP               (Ast.mod(MULTEXP,CASTEXP))
CASTEXP :   PRIMARYEXP                          (PRIMARYEXP)
            | PLUS CASTEXP                      (Ast.pos_op(CASTEXP))
            | TILDE CASTEXP                     (Ast.neq_op(CASTEXP))
            | NOT CASTEXP                       (Ast.not_op(CASTEXP))
PRIMARYEXP: IDENTIFIER                          (Ast.Id(IDENTIFIER))
            | NUMERAL                           (Ast.Const(NUMERAL))
            | LPAREN EXPRESSION RPAREN          (EXPRESSION)







(* EXPRESSION: IEXP | BEXP
IEXP    :   IEXP ADDOP INTTERM | INTTERM
INTTERM :   INTTERM MULTOP INTFACTOR | INTFACTOR
INTFACTOR:  NUMERAL | VARIABLE |
            LPAREN IEXP RPAREN | TILDE INTFACTOR
BEXP    :   BEXP OR BOOLTERM | BOOLTERM
BOOLTERM:   BOOLTERM AND BOOLFACTOR | BOOLFACTOR
BOOLFACTOR: TT | FF | VARIABLE | COMPARISON |
            LPAREN BEXP RPAREN | NOT BOOLFACTOR
COMPARISON: IEXP  RELOP IEXP
VARIABLE:   IDENTIFIER
RELOP   :   LT | LEQ | EQ | GT | GEQ | NEQ
ADDOP   :   PLUS | MINUS
MULTOP  :   TIMES | DIV | MOD *)







