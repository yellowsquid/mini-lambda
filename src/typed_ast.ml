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
  | ConstructorExpr of loc * id * expr array

type pattern
  = Enum of loc * int * pattern list
  | Variable of loc * id

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * id * expr
  | MatchStmt of loc * expr * case list
  | IfStmt of loc * expr * statement list * statement list
  | WhileStmt of loc * id * expr * statement list * statement list
  | ContinueStmt of loc * id
  | BreakStmt of loc * id
and case = loc * pattern * statement list

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
let pp_list pp_func ppf list =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_func ppf list;
  Format.fprintf ppf ")@]"

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
     pp_list pp_expr ppf [lhs; rhs];
     Format.fprintf ppf ")@]";
  | UnaryExpr (_, op, e) ->
     Format.fprintf ppf "@[<4>UnaryExpr(%s" (Ops.string_of_unary_op op);
     list_sep ppf ();
     pp_list pp_expr ppf [e];
     Format.fprintf ppf ")@]";
  | LambdaExpr (_, args, captures, body) ->
     Format.fprintf ppf "@[<4>LambdaExpr(%d " args;
     list_sep ppf ();
     pp_list pp_expr ppf (Array.to_list captures);
     list_sep ppf ();
     pp_expr ppf body;
     Format.fprintf ppf ")@]"
  | CallExpr (_, callee, args) ->
     Format.fprintf ppf "@[<4>CallExpr(";
     pp_expr ppf callee;
     list_sep ppf ();
     pp_list pp_expr ppf (Array.to_list args);
     Format.fprintf ppf ")@]"
  | ConstructorExpr (_, variant, args) ->
     Format.fprintf ppf "@[<4>ConstructorExpr(%d" variant;
     list_sep ppf ();
     pp_list pp_expr ppf (Array.to_list args);
     Format.fprintf ppf ")@]"

let rec pp_pattern ppf pattern = match pattern with
  | Enum (_, variant, patterns) ->
     Format.fprintf ppf "@[<4>Enum(%d" variant;
     list_sep ppf ();
     pp_list pp_pattern ppf patterns;
     Format.fprintf ppf ")@]"
  | Variable (_, id) -> Format.fprintf ppf "*%d" id

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
  | MatchStmt (_, expr, cases) ->
     Format.fprintf ppf "@[<4>MatchStmt(";
     pp_expr ppf expr;
     list_sep ppf ();
     pp_list pp_case ppf cases;
     Format.fprintf ppf ")@]";
  | IfStmt (_, cond, tblock, fblock) ->
     Format.fprintf ppf "@[<4>IfStmt(";
     pp_expr ppf cond;
     list_sep ppf ();
     pp_list pp_stmt ppf tblock;
     list_sep ppf ();
     pp_list pp_stmt ppf fblock;
     Format.fprintf ppf ")@]"
  | WhileStmt (_, id, cond, lblock, eblock) ->
     Format.fprintf ppf "@[<4>WhileStmt(%d" id;
     list_sep ppf ();
     pp_expr ppf cond;
     list_sep ppf ();
     pp_list pp_stmt ppf lblock;
     list_sep ppf ();
     pp_list pp_stmt ppf eblock;
     Format.fprintf ppf ")@]"
  | ContinueStmt (_, id) -> Format.fprintf ppf "ContinueStmt(%d)" id
  | BreakStmt (_, id) -> Format.fprintf ppf "BreakStmt(%d)" id
and pp_case ppf (_, pattern, stmts) =
  Format.fprintf ppf "@[<1>";
  pp_pattern ppf pattern;
  list_sep ppf ();
  pp_list pp_stmt ppf stmts;
  Format.fprintf ppf ")@]"

let pp_func ppf func =
  Format.fprintf ppf "@[<1>%s(%d,@ %d,@ %d" func.name func.id func.num_params func.num_locals;
  if Option.is_some func.body then
    (list_sep ppf (); pp_list pp_stmt ppf (Option.get func.body));
  Format.fprintf ppf ")@]"

let pp_prog ppf prog =
  prog
  |> Array.iter (fun group ->
         group
         |> Array.iter (fun func ->
                pp_func ppf func;
                Format.pp_print_newline ppf ()))
