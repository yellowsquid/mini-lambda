open Typed_ast

type value
  = None
  | Unit
  | Bool of bool
  | Int of int
  | Func of int * int * statement list
  | Lambda of int * value list * expr
  | Builtin of (value list -> value)

type env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  }

type directive
  = Expr of expr
  | Stmt of statement
  | Add
  | Sub
  | Equal
  | And
  | Or
  | Invert
  | Pop
  | PopEnv
  | PushNone
  | Seq
  | Capture of int * int * expr
  | Call of int
  | Bind of int
  | If of statement list * statement list

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let make_block stmts = List.flatten (List.map (fun s -> [Stmt s; Seq]) stmts)

let step directives values envs = match directives, values, envs with
  | [], values, envs -> [], values, envs
  | Expr (FuncExpr (_, id)) :: rest, _, env :: _ ->
     rest, Array.get env.funcs id :: values, envs
  | Expr (EnvExpr (_, id)) :: rest, _, env :: _ ->
     rest, Array.get env.captures id :: values, envs
  | Expr (BoundExpr (_, id)) :: rest, _, env :: _->
     rest, !(Array.get env.binds id) :: values, envs
  | Expr (ArgExpr (_, id)) :: rest, _, env :: _ ->
     rest, Array.get env.args id :: values, envs
  | Expr (IntExpr (_, i)) :: rest, _, _ ->
     rest, Int i :: values, envs
  | Expr (BoolExpr (_, b)) :: rest, _, _ ->
     rest, Bool b :: values, envs
  | Expr (AddExpr (_, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: Add :: rest, values, envs
  | Expr (SubExpr (_, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: Sub :: rest, values, envs
  | Expr (EqualExpr (_, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: Equal :: rest, values, envs
  | Expr (AndExpr (_, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: And :: rest, values, envs
  | Expr (OrExpr (_, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: Or :: rest, values, envs
  | Expr (InvertExpr (_, e)) :: rest, _, _ ->
     Expr e :: Invert :: rest, values, envs
  | Expr (LambdaExpr (_, params, captures, body)) :: rest, _, _ ->
     let capturing = List.map (fun e -> Expr e) (Array.to_list captures) in
     capturing @ (Capture (params, Array.length captures, body) :: rest), values, envs
  | Expr (CallExpr (_, callee, args)) :: rest, _, _ ->
     let evaled_args = List.map (fun e -> Expr e) (Array.to_list args) in
     Expr callee :: evaled_args @ (Call (Array.length args) :: PopEnv :: rest), values, envs

  | Stmt (ReturnStmt (_, e)) :: rest, _, _ ->
     Expr e :: rest, values, envs
  | Stmt (ExprStmt (_, e)) :: rest, _, _ ->
     Expr e :: Pop :: PushNone :: rest, values, envs
  | Stmt (BindStmt (_, id, e)) :: rest, _, _ ->
     Expr e :: Bind id :: rest, values, envs
  | Stmt (IfStmt (_, cond, tblock, fblock)) :: rest, _, _ ->
     Expr cond :: If (tblock, fblock) :: rest, values, envs

  | Add :: rest, Int b :: Int a :: values', _ ->
     rest, Int (a + b) :: values', envs
  | Sub :: rest, Int b :: Int a :: values', _ ->
     rest, Int (a - b) :: values', envs
  | Equal :: rest, Int b :: Int a :: values', _ ->
     rest, Bool (a = b) :: values', envs
  | Equal :: rest, Bool b :: Bool a :: values', _ ->
     rest, Bool (a = b) :: values', envs
  | And :: rest, Bool b :: Bool a :: values', _ ->
     rest, Bool (a && b) :: values', envs
  | Or :: rest, Bool b :: Bool a :: values', _ ->
     rest, Bool (a || b) :: values', envs
  | Invert :: rest, Bool b :: values', _ ->
     rest, Bool (not b) :: values', envs

  | Pop :: rest, _ :: values', _ ->
     rest, values', envs
  | PopEnv :: rest, _, _ :: envs' ->
     rest, values, envs'
  | PushNone :: rest, _, _ ->
     rest, None :: values, envs
  | Seq :: rest, None :: values', _ ->
     rest, values', envs
  | Seq :: _ :: rest, _, _ ->
     rest, values, envs

  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Lambda (params, captures', body) :: values', envs

  | Call args :: rest, _, env :: _ ->
     let args', values' = take_rev args [] values in
     let callee = List.hd values' in
     let values'' = List.tl values' in
     (match callee with
      | Lambda (params, captures, body)
           when params = args ->
         let env' = { funcs = env.funcs
                    ; captures = Array.of_list captures
                    ; binds = Array.of_list []
                    ; args = Array.of_list args'
                    } in
         Expr body :: rest, values'', env' :: envs
      | Func (binds, params, body)
           when params = args ->
         let env' = { funcs = env.funcs
                    ; captures = Array.of_list []
                    ; binds = Array.init binds (fun _ -> ref None)
                    ; args = Array.of_list args'
                    } in
         make_block body @ (PushNone :: rest), values'', env' :: envs
      | Builtin f ->
         rest, f args' :: values'', env :: envs
      | _ -> failwith "type mismatch")

  | Bind id :: rest, v :: values', env :: _ ->
     (Array.get env.binds id) := v;
     rest, None :: values', envs
  | If (tblock, _) :: rest, Bool true :: values', _ ->
     make_block tblock @ (PushNone :: rest), values', envs
  | If (_, fblock) :: rest, Bool false :: values', _ ->
     make_block fblock @ (PushNone :: rest), values', envs
  | _, _, _ -> failwith "type mismatch"

let driver env stmts =
  let rec iter (directives, values, envs) = match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter ((make_block stmts @ [PushNone]), [], [env])

module FuncMap = Map.Make(String)

let print_int x_list = match x_list with
  | [Int x] -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool b_list = match b_list with
  | [Bool b] -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int empty_list = match empty_list with
  | [] -> Int (read_int ())
  | _ -> failwith "type mismatch"

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Builtin print_int
                                           ; "print_bool", Builtin print_bool
                                           ; "input_int", Builtin input_int])

let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    Func (func.num_locals, func.num_params, Option.get func.body)

let interpret program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  let funcs = Array.map interpret_func program' in
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { funcs = funcs
            ; captures = Array.of_list []
            ; binds = Array.of_list []
            ; args = Array.of_list []
            }
  in
  let func_expr = FuncExpr (Lexing.dummy_pos, main.id) in
  let args = Array.of_list [] in
  let call_expr = CallExpr (Lexing.dummy_pos, func_expr, args) in
  let call_stmt = ExprStmt (Lexing.dummy_pos, call_expr) in
  match driver env [call_stmt] with
  | [None] -> ()
  | _ -> failwith "type mismatch"
