open Typed_ast
open Ops

type directive
  = Expr of expr
  | Stmt of statement
  | BinOp of bin_op
  | UnaryOp of unary_op
  | Pop
  | PopEnv
  | PushNone
  | Seq
  (* Params * Locals * Body *)
  | Capture of int * int * directive list
  | Builtin of (env -> value)
  | Call of int
  | Bind of int
  | If of directive list * directive list
and value
  = None
  | Unit
  | Bool of bool
  | Int of int
  (* Params * Locals * Body *)
  | Func of int * value list  * directive list
and env =
  { locals: int
  ; stack: value ref array
  }

let do_bin_op op lhs rhs = match op, lhs, rhs with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Equal, Int a, Int b -> Bool (a = b)
  | Equal, Bool a, Bool b -> Bool (a = b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _, _, _ -> failwith "runtime error"

let do_unary_op op e = match op, e with
  | Invert, Bool b -> Bool (not b)
  | _, _ -> failwith "runtime error"

let list_sep ppf _ = Format.fprintf ppf ",@ "

let rec pp_directive ppf directive = match directive with
  | Expr e -> Format.fprintf ppf "@[<4>Expr("; pp_expr ppf e; Format.fprintf ppf ")@]"
  | Stmt s -> Format.fprintf ppf "@[<4>Stmt("; pp_stmt ppf s; Format.fprintf ppf ")@]"
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (string_of_bin_op op);
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (string_of_unary_op op);
  | Pop -> Format.fprintf ppf "Pop"
  | PopEnv -> Format.fprintf ppf "PopEnv"
  | PushNone -> Format.fprintf ppf "PushNone"
  | Seq -> Format.fprintf ppf "Seq"
  | Capture (params, captures, body) ->
     Format.fprintf ppf "@[<4>Capture(%d,@ %d,@ " params captures;
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
  | Builtin _ -> Format.fprintf ppf "Builtin"
  | Call args -> Format.fprintf ppf "Call(%d)" args
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "@[<4>If(";
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [tblock; fblock];
     Format.fprintf ppf "@])@]"
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"

let rec pp_value ppf value = match value with
  | None -> Format.fprintf ppf "None"
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | Func (params, locals, body) ->
     Format.fprintf ppf "@[<4>Func(%d,@ " params;
     pp_value_list ppf locals;
     list_sep ppf ();
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
and pp_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_value ppf values;
  Format.fprintf ppf ")@]"

let pp_env ppf env =
  Format.fprintf ppf "@[<1>(%d,@ " env.locals;
  pp_value_list ppf (Array.to_list (Array.map (!) env.stack));
  Format.fprintf ppf ")@]"

let pp_env_list ppf envs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_env ppf envs;
  Format.fprintf ppf ")@]"

let funcs = ref (Array.of_list [])

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let make_block stmts = List.flatten (List.map (fun s -> [Stmt s; Seq]) stmts) @ [PushNone]

let step directives values envs = match directives, values, envs with
  (* Nothing to do so we're done *)
  | [], values, envs -> [], values, envs
  (* Push function to top *)
  | Expr (FuncExpr (_, id)) :: rest, _, _ ->
     rest, Array.get !funcs id :: values, envs
  (* Push local to top *)
  | Expr (EnvExpr (_, id)) :: rest, _, env :: _ ->
     rest, !(Array.get env.stack id) :: values, envs
  (* Push local to top *)
  | Expr (BoundExpr (_, id)) :: rest, _, env :: _->
     rest, !(Array.get env.stack id) :: values, envs
  (* Push arg to top *)
  | Expr (ArgExpr (_, id)) :: rest, _, env :: _ ->
     rest, !(Array.get env.stack (id + env.locals)) :: values, envs
  (* Push int to top *)
  | Expr (IntExpr (_, i)) :: rest, _, _ ->
     rest, Int i :: values, envs
  (* Push bool to top *)
  | Expr (BoolExpr (_, b)) :: rest, _, _ ->
     rest, Bool b :: values, envs
  (* Push first arg then second arg then compute *)
  | Expr (BinExpr (_, op, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: BinOp op :: rest, values, envs
  (* Push arg then compute *)
  | Expr (UnaryExpr (_, op, e)) :: rest, _, _ ->
     Expr e :: UnaryOp op :: rest, values, envs
  (* Evaluate args then capture as lambda *)
  | Expr (LambdaExpr (_, params, captures, body)) :: rest, _, _ ->
     let capturing = List.map (fun e -> Expr e) (Array.to_list captures) in
     capturing @ (Capture (params, Array.length captures, [Expr body]) :: rest), values, envs
  (* Evaluate callee then args then call *)
  | Expr (CallExpr (_, callee, args)) :: rest, _, _ ->
     let evaled_args = List.map (fun e -> Expr e) (Array.to_list args) in
     Expr callee :: evaled_args @ (Call (Array.length args) :: PopEnv :: rest), values, envs

  (* Calculate value and then continue *)
  | Stmt (ReturnStmt (_, e)) :: rest, _, _ ->
     Expr e :: rest, values, envs
  (* Eval expression pop then continue *)
  | Stmt (ExprStmt (_, e)) :: rest, _, _ ->
     Expr e :: Pop :: PushNone :: rest, values, envs
  (* Eval expression bind then continue *)
  | Stmt (BindStmt (_, id, e)) :: rest, _, _ ->
     Expr e :: Bind id :: rest, values, envs
  (* Eval condition then continue *)
  | Stmt (IfStmt (_, cond, tblock, fblock)) :: rest, _, _ ->
     Expr cond :: If (make_block tblock, make_block fblock) :: rest, values, envs

  (* Compute binary op then continue *)
  | BinOp op :: rest, rhs :: lhs :: values', _ ->
     rest, do_bin_op op lhs rhs :: values', envs
  (* Compute unary op then continue *)
  | UnaryOp op :: rest, v :: values', _ ->
     rest, do_unary_op op v :: values', envs

  (* Pop top value *)
  | Pop :: rest, _ :: values', _ ->
     rest, values', envs
  (* Pop top environment *)
  | PopEnv :: rest, _, _ :: envs' ->
     rest, values, envs'
  (* Push none to stack *)
  | PushNone :: rest, _, _ ->
     rest, None :: values, envs
  (* Statement didn't return so eval next one *)
  | Seq :: rest, None :: values', _ ->
     rest, values', envs
  (* Statement returned so skip next one *)
  | Seq :: _ :: rest, _, _ ->
     rest, values, envs

  (* Create lambda from args *)
  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Func (params, captures', body) :: values', envs
  (* Evaluate built-in function *)
  | Builtin f :: rest, _, env :: _ ->
     rest, f env :: values, envs

  (* Call function with args *)
  | Call args :: rest, _, _ ->
     let args', values' = take_rev args [] values in
     (match values' with
      | Func (params, locals, body) :: values'' when params = args ->
         let env' = { stack = Array.of_list (List.map ref (locals @ args'))
                    ; locals = List.length locals
                    } in
         body @ rest, values'', env' :: envs
      | _ -> failwith "type mismatch"
     )

  (* Bind value and push None *)
  | Bind id :: rest, v :: values', env :: _ ->
     (Array.get env.stack id) := v;
     rest, None :: values', envs
  (* If true then evaluate true block *)
  | If (tblock, _) :: rest, Bool true :: values', _ ->
     tblock @ rest, values', envs
  (* If false then evaluate false block *)
  | If (_, fblock) :: rest, Bool false :: values', _ ->
     fblock @ rest, values', envs

  (* Reaching here is a bug *)
  | _, _, _ -> failwith "runtime error"

let stage = ref 0

let driver debug env directives =
  let rec iter (directives, values, envs) =
    if debug then begin
        incr stage;
        Format.fprintf Format.std_formatter "@[<4>Step %d: (@," !stage;
        pp_directive_list Format.std_formatter directives;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_value_list Format.std_formatter values;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_env_list Format.std_formatter envs;
        Format.fprintf Format.std_formatter ")@]@\n" end;
    match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter (directives, [], [env])

module FuncMap = Map.Make(String)

let print_int env = match !(env.stack.(0)) with
  | Int x -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool env = match !(env.stack.(0)) with
  | Bool b -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _ = Int (read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Func (1, [], [Builtin print_int])
                                           ; "print_bool", Func (1, [], [Builtin print_bool])
                                           ; "input_int", Func (0, [], [Builtin input_int])])

let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    let locals = List.init func.num_locals (fun _ -> None) in
    Func (func.num_params, locals, make_block (Option.get func.body))

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  funcs := Array.map interpret_func program';
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { stack = Array.init main.num_locals (fun _ -> ref None)
            ; locals = main.num_locals
            } in
  match Array.get !funcs main.id with
  | Func(0, _, body) ->
     (match driver debug env body with
      | [None] -> ()
      | _ -> failwith "type mismatch")
  | _ -> failwith "main not a function"
