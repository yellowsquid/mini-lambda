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
%token <string> TYPE
%token <bool> BOOL
%token INVERT
%token PLUS MINUS
%token EQUAL NEQUAL
%token AND OR
%token PIPE
%token LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE
%token ENUM
%token FUNC EXTERN
%token MATCH
%token IF ELSE
%token WHILE CONTINUE BREAK
%token RETURN
%token ARROW
%token BIND
%token COMMA
%token SEMI COLON
%token EOF

%start program
%type <Ast.program> program

%%

program:
  list(ty_decl) list(func) EOF { { tys = Array.of_list $1; funcs = Array.of_list $2 } }

ty_decl:
  | ENUM TYPE LBRACE constructors RBRACE
    { { loc = $startpos; ty_name = $2; generics = []; consts = $4 } }
  | ENUM TYPE LANGLE separated_nonempty_list(COMMA, TYPE) RANGLE LBRACE constructors RBRACE
    { { loc = $startpos; ty_name = $2; generics = $4; consts = $7 } }

constructors:
  | constructor constructors { $1 :: $2 }
  | { [] }

constructor:
  | TYPE SEMI
    { { loc = $startpos; const_name = $1; params = [] } }
  | TYPE LPAREN separated_nonempty_list(COMMA, ty) RPAREN SEMI
    { { loc = $startpos; const_name = $1; params = $3 } }

func:
  | EXTERN IDENT LPAREN params = separated_list(COMMA, ty); RPAREN COLON return_ty = ty; SEMI
    { { func_name = $2; loc = $startpos; rest = Extern (params, return_ty) } }
  | FUNC IDENT; LPAREN params = separated_list(COMMA, named); RPAREN LBRACE body = statements; RBRACE
    { { func_name = $2; loc = $startpos; rest = Definition (params, body) } }

statements:
  | statement statements { $1 :: $2 }
  | { [] }

statement:
  | match_statement { $1 }
  | if_statement { $1 }
  | while_statement { $1 }
  | loop_statement { $1 }
  | RETURN expr SEMI { ReturnStmt ($startpos, $2) }
  | IDENT BIND expr SEMI { BindStmt ($startpos, $1, $3) }
  | IGNORE BIND expr SEMI { IgnoreStmt ($startpos, $3) }
  | expr SEMI { ExprStmt ($startpos, $1) }

match_statement:
  | MATCH expr LBRACE list(case_stmt) RBRACE { MatchStmt ($startpos, $2, $4) }

case_stmt:
  | pattern ARROW statement { $startpos, $1, [$3] }
  | pattern ARROW LBRACE statements RBRACE { $startpos, $1, $4 }

pattern:
  | TYPE { Enum ($startpos, $1, []) }
  | TYPE LPAREN separated_nonempty_list(COMMA, pattern) RPAREN { Enum ($startpos, $1, $3) }
  | IDENT { Variable ($startpos, $1) }
  | IGNORE { Ignore ($startpos, $1) }
  | INT { Int ($startpos, $1) }
  | BOOL { Bool ($startpos, $1) }

if_statement:
  | IF expr LBRACE statements RBRACE
    { IfStmt ($startpos, $2, $4, []) }
  | IF expr LBRACE statements RBRACE ELSE if_statement
    { IfStmt ($startpos, $2, $4, $7 :: []) }
  | IF expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { IfStmt ($startpos, $2, $4, $8) }

while_statement:
  | WHILE expr LBRACE statements RBRACE
    { WhileStmt ($startpos, $2, $4, [], None) }
  | WHILE expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { WhileStmt ($startpos, $2, $4, $8, None) }
  | IDENT COLON WHILE expr LBRACE statements RBRACE
    { WhileStmt ($startpos, $4, $6, [], Some $1) }
  | IDENT COLON WHILE expr LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { WhileStmt ($startpos, $4, $6, $10, Some $1) }

loop_statement:
  | CONTINUE SEMI { ContinueStmt ($startpos, None) }
  | CONTINUE IDENT SEMI { ContinueStmt ($startpos, Some $2) }
  | BREAK SEMI { BreakStmt ($startpos, None) }
  | BREAK IDENT SEMI { BreakStmt ($startpos, Some $2) }

expr:
  | unary_expr { $1 }
  | expr NEQUAL unary_expr { UnaryExpr ($startpos, Invert, BinExpr ($startpos, Equal, $1, $3)) }
  | expr bin_op unary_expr { BinExpr ($startpos, $2, $1, $3) }

unary_expr:
  | PIPE params = separated_list(COMMA, named); PIPE ARROW body = postfix_expr;
    { LambdaExpr ($startpos, params, body) }
  | postfix_expr { $1 }
  | unary_op unary_expr { UnaryExpr ($startpos, $1, $2) }

postfix_expr:
  | primary_expr { $1 }
  | TYPE { ConstructorExpr ($startpos, $1, []) }
  | TYPE LPAREN separated_nonempty_list(COMMA, expr) RPAREN { ConstructorExpr ($startpos, $1, $3) }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr ($startpos, callee, args) }

primary_expr:
  | LPAREN expr RPAREN { $2 }
  | named { IdentExpr ($startpos, $1) }
  | INT { IntExpr ($startpos, $1) }
  | BOOL { BoolExpr ($startpos, $1) }

ty:
  | TYPE { { loc = $startpos; base = $1; params = [] } }
  | TYPE LANGLE separated_list(COMMA, ty) RANGLE { { loc = $startpos; base = $1; params = $3 } }

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
