/* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the grammar of the language, with
 * the productions builtin the Abstract Syntax Tree.
 */

%{
    open Ast
    open Ops
%}

%token <int> INT
%token <string> IDENT
%token <string> IGNORE
%token TRUE FALSE
%token INVERT
%token PLUS MINUS
%token EQUAL NEQUAL
%token AND OR
%token LPAREN RPAREN LBRACE RBRACE
%token FUNC
%token IF ELSE
%token WHILE CONTINUE BREAK
%token RETURN
%token ARROW
%token LAMBDA
%token BIND
%token COMMA
%token SEMI COLON
%token EOF

%start program
%type <Ast.program> program

%%

program:
  funcs = list(func) EOF { Array.of_list funcs }

func:
  | FUNC name = IDENT;
    LPAREN params = separated_list(COMMA, named); RPAREN
    body = func_body
    { { name; params; body; loc = $startpos } }

func_body:
  | LBRACE statements RBRACE { Some($2) }
  | SEMI { None }

statements:
  | statement statements { $1 :: $2 }
  | { [] }

statement:
  | if_statement { $1 }
  | while_statement { $1 }
  | loop_statement { $1 }
  | RETURN expr SEMI { ReturnStmt($startpos, $2) }
  | IDENT BIND expr SEMI { BindStmt($startpos, $1, $3) }
  | IGNORE BIND expr SEMI { IgnoreStmt($startpos, $3) }
  | expr SEMI { ExprStmt($startpos, $1) }

if_statement:
  | IF expr LBRACE statements RBRACE
    { IfStmt($startpos, $2, $4, []) }
  | IF expr LBRACE statements RBRACE ELSE if_statement
    { IfStmt($startpos, $2, $4, $7 :: []) }
  | IF expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { IfStmt($startpos, $2, $4, $8) }

while_statement:
  | WHILE expr LBRACE statements RBRACE
    { WhileStmt($startpos, $2, $4, [], None) }
  | WHILE expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { WhileStmt($startpos, $2, $4, $8, None) }
  | IDENT COLON WHILE expr LBRACE statements RBRACE
    { WhileStmt($startpos, $4, $6, [], Some($1)) }
  | IDENT COLON WHILE expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { WhileStmt($startpos, $4, $6, $10, Some($1)) }

loop_statement:
  | CONTINUE SEMI { ContinueStmt($startpos, None) }
  | CONTINUE IDENT SEMI { ContinueStmt($startpos, Some($2)) }
  | BREAK SEMI { BreakStmt($startpos, None) }
  | BREAK IDENT SEMI { BreakStmt($startpos, Some($2)) }

expr:
  | unary_expr { $1 }
  | expr NEQUAL unary_expr { UnaryExpr($startpos, Invert, BinExpr($startpos, Equal, $1, $3)) }
  | expr bin_op unary_expr { BinExpr($startpos, $2, $1, $3) }

unary_expr:
  | LAMBDA
    LPAREN params = separated_list(COMMA, named); RPAREN
    ARROW
    body = postfix_expr;
    { LambdaExpr($startpos, params, body) }
  | postfix_expr { $1 }
  | unary_op unary_expr { UnaryExpr($startpos, $1, $2) }

postfix_expr:
  | primary_expr { $1 }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr($startpos, callee, args) }

primary_expr:
  | LPAREN expr RPAREN { $2 }
  | named { IdentExpr($startpos, $1) }
  | INT { IntExpr($startpos, $1) }
  | TRUE { BoolExpr($startpos, true) }
  | FALSE { BoolExpr($startpos, false) }

named:
  | IDENT { $1 }
  | IGNORE { $1 }

bin_op:
  | PLUS { Add }
  | MINUS { Sub }
  | EQUAL { Equal }
  | AND { And }
  | OR { Or }

unary_op:
  | INVERT { Invert }
