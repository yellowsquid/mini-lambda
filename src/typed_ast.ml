(* Compiler Construction - Minimal Lambda Language
 *
 * This is a version of the AST produced by the type checker.
 * It does not actually include type information presently
 * since it is not needed by other passes. Lambda captures
 * are the targets of named references are made explicit here.
 *)

type loc = Lexing.position

type id = int

type expr
  = FuncExpr of loc * id
  | EnvExpr of loc * id
  | BoundExpr of loc * id
  | ArgExpr of loc * id
  | IntExpr of loc * int
  | BoolExpr of loc * bool
  | BinExpr of loc * Ops.bin_op * expr * expr
  | UnaryExpr of loc * Ops.unary_op * expr
  | LambdaExpr of loc * int * expr array * expr
  | CallExpr of loc * expr * expr array

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * id * expr
  | IfStmt of loc * expr * statement list * statement list
  | WhileStmt of loc * id * expr * statement list * statement list
  | ContinueStmt of loc * id
  | BreakStmt of loc * id

type func =
  { id: id
  ; name: string
  ; num_params: int
  ; num_locals: int
  ; body: (statement list) option
  ; loc: loc
  }

type program = func array array

let list_sep ppf () = Format.fprintf ppf ",@ "

let rec pp_expr ppf expr = match expr with
  | FuncExpr (_, id) -> Format.fprintf ppf "Func(%d)" id
  | EnvExpr (_, id) -> Format.fprintf ppf "Capture(%d)" id
  | BoundExpr (_, id) -> Format.fprintf ppf "Bound(%d)" id
  | ArgExpr (_, id) -> Format.fprintf ppf "Arg(%d)" id
  | IntExpr (_, i) -> Format.fprintf ppf "%d" i
  | BoolExpr (_, b) -> Format.fprintf ppf "%B" b
  | BinExpr (_, op, lhs, rhs) ->
     Format.fprintf ppf "@[<4>BinExpr(%s" (Ops.string_of_bin_op op);
     list_sep ppf ();
     pp_expr_list ppf [lhs; rhs];
     Format.fprintf ppf ")@]";
  | UnaryExpr (_, op, e) ->
     Format.fprintf ppf "@[<4>UnaryExpr(%s" (Ops.string_of_unary_op op);
     list_sep ppf ();
     pp_expr_list ppf [e];
     Format.fprintf ppf ")@]";
  | LambdaExpr (_, args, captures, body) ->
     Format.fprintf ppf "@[<4>LambdaExpr(%d " args;
     list_sep ppf ();
     pp_expr_list ppf (Array.to_list captures);
     list_sep ppf ();
     pp_expr ppf body;
     Format.fprintf ppf ")@]"
  | CallExpr (_, callee, args) ->
     Format.fprintf ppf "@[<4>CallExpr(";
     pp_expr ppf callee;
     list_sep ppf ();
     pp_expr_list ppf (Array.to_list args);
     Format.fprintf ppf ")@]"
and pp_expr_list ppf exprs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_expr ppf exprs;
  Format.fprintf ppf ")@]"

let rec pp_stmt ppf stmt = match stmt with
  | ReturnStmt (_, expr) ->
     Format.fprintf ppf "@[<4>ReturnStmt(";
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | ExprStmt (_, expr) ->
     Format.fprintf ppf "@[<4>ExprStmt(";
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | BindStmt (_, id, expr) ->
     Format.fprintf ppf "@[<4>BindStmt(%d" id;
     list_sep ppf ();
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | IfStmt (_, cond, tblock, fblock) ->
     Format.fprintf ppf "@[<4>IfStmt(";
     pp_expr ppf cond;
     list_sep ppf ();
     pp_stmt_list ppf tblock;
     list_sep ppf ();
     pp_stmt_list ppf fblock;
     Format.fprintf ppf ")@]"
  | WhileStmt (_, id, cond, lblock, eblock) ->
     Format.fprintf ppf "@[<4>WhileStmt(%d" id;
     list_sep ppf ();
     pp_expr ppf cond;
     list_sep ppf ();
     pp_stmt_list ppf lblock;
     list_sep ppf ();
     pp_stmt_list ppf eblock;
     Format.fprintf ppf ")@]"
  | ContinueStmt (_, id) -> Format.fprintf ppf "ContinueStmt(%d)" id
  | BreakStmt (_, id) -> Format.fprintf ppf "BreakStmt(%d)" id
and pp_stmt_list ppf stmts =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_stmt ppf stmts;
  Format.fprintf ppf ")@]"
