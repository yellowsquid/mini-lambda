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
  | LBRACE body = statements; RBRACE { Some(body) }
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
  | lhs = expr; PLUS; rhs = unary_expr
    { AddExpr($startpos, lhs, rhs) }
  | lhs = expr; MINUS; rhs = unary_expr
    { SubExpr($startpos, lhs, rhs) }
  | lhs = expr; EQUAL; rhs = unary_expr
    { EqualExpr($startpos, lhs, rhs) }
  | lhs = expr; NEQUAL; rhs = unary_expr
    { InvertExpr($startpos, EqualExpr($startpos, lhs, rhs))}
  | lhs = expr; AND; rhs = unary_expr
    { AndExpr($startpos, lhs, rhs) }
  | lhs = expr; OR; rhs = unary_expr
    { OrExpr($startpos, lhs, rhs) }

unary_expr:
  | LAMBDA
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    ARROW
    body = postfix_expr;
    { LambdaExpr($startpos, params, body) }
  | postfix_expr { $1 }
  | INVERT arg = unary_expr; { InvertExpr($startpos, arg) }

postfix_expr:
  | primary_expr { $1 }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr($startpos, callee, args) }

primary_expr:
  | LPAREN e = expr; RPAREN { e }
  | name = IDENT { IdentExpr($startpos, name) }
  | decimal = INT { IntExpr($startpos, decimal) }
  | TRUE { BoolExpr($startpos, true) }
  | FALSE { BoolExpr($startpos, false) }
