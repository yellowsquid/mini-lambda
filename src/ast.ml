(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the Abstract Syntax Tree, produced
 * by the parser. In order to generate accurate error
 * messages further down the pipeline, source locations
 * are included in each node.
 *)

type loc = Lexing.position

type expr
  = IdentExpr of loc * string
  | IntExpr of loc * int
  | BoolExpr of loc * bool
  | BinExpr of loc * Ops.bin_op * expr * expr
  | UnaryExpr of loc * Ops.unary_op * expr
  | LambdaExpr of loc * string list * expr
  | CallExpr of loc * expr * expr list
  | ConstructorExpr of loc * string * expr list

type pattern
  = Variable of loc * string
  | Enum of loc * string * pattern list
  | Ignore of loc * string
  | Int of loc * int
  | Bool of loc * bool

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * string * expr
  | IgnoreStmt of loc * expr
  | MatchStmt of loc * expr * case list
  | IfStmt of loc * expr * statement list * statement list
  | WhileStmt of loc * expr * statement list * statement list * string option
  | ContinueStmt of loc * string option
  | BreakStmt of loc * string option
and case = loc * pattern * statement list

type ty = { loc: loc; base: string; params: ty list }

type func_type
  = Extern of ty list * ty
  | Definition of string list * statement list

type func = { loc: loc; func_name: string; rest: func_type }

type constructor = { loc: loc; const_name: string; params: ty list }

type ty_decl = { loc: loc; ty_name: string; generics: string list; consts: constructor list }

type program = { tys: ty_decl array; funcs: func array }
