# Abstract Syntax Trees (AST) for WHILE

EBNF for WHILE programming language :https://gist.github.com/Amaniitd/357b62fb419c011f5c7e92c6a3e02f7e
(given by Prof. SAK)

## Context-free grammar

The grammar G = <N, T, P, Program> where,

      N = {Program, Block, DeclarationSeq, Declaration, IntDecl, BoolDecl,
      IntVariableList, BoolVariableList, IntVariable, BoolVariable,
      CommandSeq, Command, IntExpression, IntTerm, IntFactor,
      BoolExpression, BoolTerm, BoolFactor, Comparison, RelOp,
      AddOp, MultOp, Identifier, Numeral}

      T = {"program", "::", "var", ":", "int", "bool", ",", "{", "}", ":=",
      "read", "write", "if", "then", "else", "endif, "while", "do", "endwh",
      "(", ")", "&&", "||", "tt", "ff", "!", ">", "<", ">=", "<=", "=", "<>",
      "+", "-", "*", "/", "%", [a-zA-Z], [0-9]}

P consists of the following productions rules:

      Program -> "program" Identifier "::"Block .

      Block -> DeclarationSeq CommandSeq .

      DeclarationSeq -> {Declaration} .

      Declaration -> IntDecl | BoolDecl.

      IntDecl -> "var" IntVariableList ":" "int" .

      BoolDecl -> "var" BoolVariableList ":" "bool" .

      IntVariableList -> IntVariable {"," IntVariable} .

      BoolVariableList -> BoolVariable {"," BoolVariable} .


      CommandSeq -> "{"{Command";"}"}" .

      Command -> IntVariable":="IntExpression |
                  BoolVariable":="BoolExpression |
                  "read" IntVariable |
                  "read" BoolVariable |
                  "write" IntExpression |
                  "if" BoolExpression "then" CommandSeq
                  "else" CommandSeq
                  "endif" |
                  "while" BoolExpression "do" CommandSeq
                  "endwh" .

      IntExpression -> IntExpression AddOp IntTerm | IntTerm .

      IntTerm -> IntTerm MultOp IntFactor | IntFactor .

      IntFactor -> Numeral | IntVariable |
                  "("IntExpression")" .

      BoolExpression -> BoolExpression "||" BoolTerm | BoolTerm .

      BoolTerm -> BoolTerm "&&" BoolFactor | BoolFactor .

      BoolFactor -> "tt" | "ff" | BoolVariable | Comparison |
                  "("BoolExpression")" | "!"BoolFactor .

      Comparison -> IntExpression RelOp IntExpression |
                  BoolExpression RelOp BoolExpression .

      IntVariable -> Identifier .

      BoolVariable -> Identifier .

      RelOp -> "<" | "<=" | "=" | ">" | ">=" | "<>" .

      AddOp -> "+" | "-" .

      MultOp -> "*" | "/" | "%" .

      Identifier -> Letter{Letter | Digit} .

      Numeral -> ["+" | "~"]Digit{Digit} .

## AST datatype defination

## Syntax-directed translation

## Auxiliary functions and Data

## Other Design Decisions

## Other Implementation Decisions

## Acknowledgements
