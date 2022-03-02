#  Abstract Syntax Trees (AST) for WHILE
EBNF for WHILE programming language :https://gist.github.com/Amaniitd/357b62fb419c011f5c7e92c6a3e02f7e
(given by Prof. SAK)

## Context-free grammar
The grammar G = <N, T, P, Program> where,

      N = {Program, Block, DeclarationSeq, Declaration, Type, VariableList,
      CommandSeq, Command, Expression, IntExpression, IntTerm, IntFactor, 
      BoolExpression, BoolTerm, BoolFactor, Comparison, variable, RelOp, 
      AddOp, MultOp, Identifier, Numeral}

T = {}

P consists of the following productions rules:

      Program -> “program” Identifier “::”Block .

      Block -> DeclarationSeq CommandSeq .

      DeclarationSeq -> {Declaration} .

      Declaration -> “var” VariableList“:”Type“;” .
      Type -> “int” | “bool” .

      VariableList -> Variable{“,” Variable} .

      CommandSeq -> “{”{Command“;”}“}” .
      Command -> Variable“:=”Expression |
                  “read” Variable |
                  “write” IntExpression |
                  “if” BoolExpression “then” CommandSeq
                  “else” CommandSeq
                  “endif” |
                  “while” BoolExpression “do” CommandSeq
                  “endwh” .

      Expression -> IntExpression | BoolExpression .

      IntExpression -> IntExpression AddOp IntTerm | IntTerm .

      IntTerm -> IntTerm MultOp IntFactor | IntFactor .

      IntFactor -> Numeral | Variable |
                  “(”IntExpression“)” | “˜”IntFactor .

      BoolExpression -> BoolExpression “||” BoolTerm | BoolTerm .

      BoolTerm -> BoolTerm “&&” BoolFactor | BoolFactor .

      BoolFactor -> “tt” | “ff” | Variable | Comparison |
                  “(”BoolExpression“)” | “!”BoolFactor .

      Comparison -> IntExpression RelOp IntExpression .

      Variable -> Identifier .

      RelOp -> “<” | “<=” | “=” | “>” | “>=” | “<>” .

      AddOp -> “+” | “−” .

      MultOp -> “∗” | “/” | “%” .

      Identifier -> Letter{Letter | Digit} .

      Numeral -> [“+” | “˜”]Digit{Digit} .

## AST datatype defination

## Syntax-directed translation

## Auxiliary functions and Data

## Other Design Decisions

## Other Implementation Decisions

## Acknowledgements