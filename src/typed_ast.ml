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

type func =
  { id: id
  ; name: string
  ; num_params: int
  ; num_locals: int
  ; body: (statement list) option
  ; loc: loc
  }

type program = func array array

let list_sep ppf _ = Format.fprintf ppf ",@ "

let rec pp_expr ppf expr = match expr with
  | FuncExpr(_, id) -> Format.fprintf ppf "Func(%d)" id
  | EnvExpr(_, id) -> Format.fprintf ppf "Capture(%d)" id
  | BoundExpr(_, id) -> Format.fprintf ppf "Bound(%d)" id
  | ArgExpr(_, id) -> Format.fprintf ppf "Arg(%d)" id
  | IntExpr(_, i) -> Format.fprintf ppf "%d" i
  | BoolExpr(_, b) -> Format.fprintf ppf "%B" b
  | BinExpr(_, op, lhs, rhs) ->
     Format.fprintf ppf "@[<4>BinExpr(%s,@ " (Ops.string_of_bin_op op);
     Format.pp_print_list ~pp_sep:list_sep pp_expr ppf [lhs; rhs];
     Format.fprintf ppf ")@]";
  | UnaryExpr(_, op, e) ->
     Format.fprintf ppf "@[<4>UnaryExpr(%s,@ " (Ops.string_of_unary_op op);
     Format.pp_print_list ~pp_sep:list_sep pp_expr ppf [e];
     Format.fprintf ppf ")@]";
  | LambdaExpr(_, args, captures, body) ->
     Format.fprintf ppf "@[<4>LambdaExpr(%d,@ @[<1>(@," args;
     Format.pp_print_list ~pp_sep:list_sep pp_expr ppf (Array.to_list captures);
     Format.fprintf ppf ")@],@ ";
     pp_expr ppf body;
     Format.fprintf ppf ")@]"
  | CallExpr(_, callee, args) ->
     Format.fprintf ppf "@[<4>CallExpr(";
     pp_expr ppf callee;
     Format.fprintf ppf ",@ @[<1>(";
     Format.pp_print_list ~pp_sep:list_sep pp_expr ppf (Array.to_list args);
     Format.fprintf ppf ")@])@]"

let rec pp_stmt ppf stmt = match stmt with
  | ReturnStmt(_, expr) ->
     Format.fprintf ppf "@[<4>ReturnStmt(";
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | ExprStmt(_, expr) ->
     Format.fprintf ppf "@[<4>ExprStmt(";
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | BindStmt(_, id, expr) ->
     Format.fprintf ppf "@[<4>BindStmt(%d,@ " id;
     pp_expr ppf expr;
     Format.fprintf ppf ")@]"
  | IfStmt(_, cond, then_block, else_block) ->
     Format.fprintf ppf "@[<4>IfStmt(";
     pp_expr ppf cond;
     Format.fprintf ppf ",@ @[<1>(";
     let sep = (fun ppf _ -> Format.fprintf ppf ")@],@ @[<1>(") in
     let sub_fmt ppf block = Format.pp_print_list ~pp_sep:list_sep pp_stmt ppf block in
     Format.pp_print_list ~pp_sep:sep sub_fmt ppf [then_block; else_block];
     Format.fprintf ppf ")@])@]"
