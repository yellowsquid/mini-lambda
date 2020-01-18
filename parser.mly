/* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the grammar of the language, with
 * the productions builtin the Abstract Syntax Tree.
 */

%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token INVERT
%token PLUS MINUS
%token EQUAL NEQUAL
%token AND OR
%token LPAREN RPAREN LBRACE RBRACE
%token FUNC
%token RETURN
%token ARROW
%token LAMBDA
%token BIND
%token COMMA
%token SEMI
%token EOF

%start program
%type <Ast.program> program

%%

program:
  funcs = list(func) EOF { Array.of_list funcs }


func:
  | FUNC name = IDENT;
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    body = func_body
    { { name; params; body; loc = $startpos } }

func_body:
  | LBRACE statements RBRACE { Some($2) }
  | SEMI { None }

statements:
  | statement statements { $1 :: $2 }
  | { [] }

statement:
  | RETURN expr SEMI { ReturnStmt($startpos, $2) }
  | IDENT BIND expr SEMI { BindStmt($startpos, $1, $3) }
  | expr SEMI { ExprStmt($startpos, $1) }

expr:
  | unary_expr { $1 }
  | expr PLUS unary_expr { AddExpr($startpos, $1, $3) }
  | expr MINUS unary_expr { SubExpr($startpos, $1, $3) }
  | expr EQUAL unary_expr { EqualExpr($startpos, $1, $3) }
  | expr NEQUAL unary_expr { InvertExpr($startpos, EqualExpr($startpos, $1, $3))}
  | expr AND unary_expr { AndExpr($startpos, $1, $3) }
  | expr OR unary_expr { OrExpr($startpos, $1, $3) }

unary_expr:
  | LAMBDA
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    ARROW
    body = postfix_expr;
    { LambdaExpr($startpos, params, body) }
  | postfix_expr { $1 }
  | INVERT unary_expr { InvertExpr($startpos, $2) }

postfix_expr:
  | primary_expr { $1 }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr($startpos, callee, args) }

primary_expr:
  | LPAREN expr RPAREN { $2 }
  | IDENT { IdentExpr($startpos, $1) }
  | INT { IntExpr($startpos, $1) }
  | TRUE { BoolExpr($startpos, true) }
  | FALSE { BoolExpr($startpos, false) }
