(* OCaml interpretter for language *)

open Typed_ast

type value
  = Unit
  | None
  | Bool of bool
  | Int of int
  | Func of int * int * statement list
  | Lambda of int * value array * expr
  | Builtin of (value list -> value)
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  }

type cnt
  = AddOuterCnt of env * expr
  | AddCnt of int
  | SubOuterCnt of env * expr
  | SubCnt of int
  | EqualOuterCnt of env * expr
  | EqualIntCnt of int
  | EqualBoolCnt of bool
  | AndOuterCnt of env * expr
  | AndCnt of bool
  | OrOuterCnt of env * expr
  | OrCnt of bool
  | InvertCnt
  | LambdaCnt of env * expr list * value list * int * expr
  | CallCnt of env * expr list
  | ArgsCnt of env * expr list * value list * value
  | IgnoreCnt
  | BindCnt of env * id
  | IfCnt of env * statement list * statement list
  | SeqCnt of env * statement list

type step
  = Expr of cnt list * env * expr
  | Stmt of cnt list * env * statement
  | Apply of cnt list * value

let step config = match config with
  | Expr (cnts, env, FuncExpr (_, id)) -> Apply (cnts, Array.get env.funcs id)
  | Expr (cnts, env, EnvExpr (_, id)) -> Apply (cnts, Array.get env.captures id)
  | Expr (cnts, env, BoundExpr (_, id)) -> Apply (cnts, !(Array.get env.binds id))
  | Expr (cnts, env, ArgExpr (_, id)) -> Apply (cnts, Array.get env.args id)
  | Expr (cnts, _, IntExpr (_, i)) -> Apply (cnts, Int i)
  | Expr (cnts, _, BoolExpr (_, b)) -> Apply (cnts, Bool b)
  | Expr (cnts, env, AddExpr (_, lhs, rhs)) -> Expr (AddOuterCnt (env, rhs) :: cnts, env, lhs)
  | Expr (cnts, env, SubExpr (_, lhs, rhs)) -> Expr (SubOuterCnt (env, rhs) :: cnts, env, lhs)
  | Expr (cnts, env, EqualExpr (_, lhs, rhs)) -> Expr (EqualOuterCnt (env, rhs) :: cnts, env, lhs)
  | Expr (cnts, env, AndExpr (_, lhs, rhs)) -> Expr (AndOuterCnt (env, rhs) :: cnts, env, lhs)
  | Expr (cnts, env, OrExpr (_, lhs, rhs)) -> Expr (OrOuterCnt (env, rhs) :: cnts, env, lhs)
  | Expr (cnts, env, InvertExpr (_, e)) -> Expr (InvertCnt :: cnts, env, e)

  | Expr (cnts, env, LambdaExpr (_, params, captures, body)) ->
     Apply (LambdaCnt (env, Array.to_list captures, [], params, body) :: cnts, None)
  | Expr (cnts, env, CallExpr (_, callee, args)) ->
     Expr (CallCnt (env, Array.to_list args) :: cnts, env, callee)

  | Stmt (cnts, env, ReturnStmt (_, e)) -> Expr (cnts, env, e)
  | Stmt (cnts, env, ExprStmt (_, e)) -> Expr (IgnoreCnt :: cnts, env, e)
  | Stmt (cnts, env, BindStmt (_, id, e)) -> Expr (BindCnt (env, id) :: cnts, env, e)
  | Stmt (cnts, env, IfStmt (_, cond, tblock, fblock)) ->
     Expr (IfCnt (env, tblock, fblock) :: cnts, env, cond)

  | Apply ([], value) -> Apply ([], value)
  | Apply (AddOuterCnt (env, rhs) :: rest, Int a) -> Expr (AddCnt a :: rest, env, rhs)
  | Apply (AddCnt a :: rest, Int b) -> Apply (rest, Int (a + b))
  | Apply (SubOuterCnt (env, rhs) :: rest, Int a) -> Expr (SubCnt a :: rest, env, rhs)
  | Apply (SubCnt a :: rest, Int b) -> Apply (rest, Int (a - b))
  | Apply (EqualOuterCnt (env, rhs) :: rest, Int a) -> Expr (EqualIntCnt a :: rest, env, rhs)
  | Apply (EqualOuterCnt (env, rhs) :: rest, Bool a) -> Expr (EqualBoolCnt a :: rest, env, rhs)
  | Apply (EqualIntCnt a :: rest, Int b) -> Apply (rest, Bool (a = b))
  | Apply (EqualBoolCnt a :: rest, Bool b) -> Apply (rest, Bool (a = b))
  | Apply (AndOuterCnt (env, rhs) :: rest, Bool a) -> Expr (AndCnt a :: rest, env, rhs)
  | Apply (AndCnt a :: rest, Bool b) -> Apply (rest, Bool (a && b))
  | Apply (OrOuterCnt (env, rhs) :: rest, Bool a) -> Expr (OrCnt a :: rest, env, rhs)
  | Apply (OrCnt a :: rest, Bool b) -> Apply (rest, Bool (a || b))
  | Apply (InvertCnt :: rest, Bool a) -> Apply (rest, Bool (not a))

  | Apply (LambdaCnt (_, [], captured, p, body) :: rest, None) ->
     Apply (rest, Lambda (p, Array.of_list (List.rev captured), body))
  | Apply (LambdaCnt (env, e :: exprs, captured, p, body) :: rest, None) ->
     Expr (LambdaCnt (env, exprs, captured, p, body) :: rest, env, e)
  | Apply (LambdaCnt (env, to_capture, captured, p, body) :: rest, v) ->
     Apply (LambdaCnt (env, to_capture, v :: captured, p, body) :: rest, None)
  | Apply (CallCnt (env, args) :: rest, v) ->
     Apply (ArgsCnt (env, args, [], v) :: rest, None)
  | Apply (ArgsCnt (env, [], parsed, Lambda (params, captures, body)) :: rest, None)
       when params = List.length parsed ->
     let env' = { funcs = env.funcs
                ; captures = captures
                ; binds = Array.of_list []
                ; args = Array.of_list (List.rev parsed)
                }
     in Expr (rest, env', body)
  | Apply (ArgsCnt (env, [], parsed, Func (binds, params, body)) :: rest, None)
       when params = List.length parsed ->
     let env' = { funcs = env.funcs
                ; captures = Array.of_list []
                ; binds = Array.init binds (fun _ -> ref None)
                ; args = Array.of_list (List.rev parsed)
                }
     in Apply (SeqCnt (env', body) :: rest, None)
  | Apply (ArgsCnt (_, [], parsed, Builtin f) :: rest, None) ->
     Apply (rest, f (List.rev parsed))
  | Apply (ArgsCnt (env, e :: exprs, parsed, callee) :: rest, None) ->
     Expr (ArgsCnt (env, exprs, parsed, callee) :: rest, env, e)
  | Apply (ArgsCnt (env, exprs, parsed, callee) :: rest, v) ->
     Apply (ArgsCnt (env, exprs, v :: parsed, callee) :: rest, None)

  | Apply (IgnoreCnt :: rest, _) -> Apply (rest, None)
  | Apply (BindCnt (env, id) :: rest, x) -> Array.get env.binds id := x; Apply (rest, None)
  | Apply (IfCnt (env, tblock, _) :: rest, Bool true) -> Apply (SeqCnt (env, tblock) :: rest, None)
  | Apply (IfCnt (env, _, fblock) :: rest, Bool false) -> Apply (SeqCnt (env, fblock) :: rest, None)
  | Apply (SeqCnt (_, []) :: rest, None) -> Apply (rest, None)
  | Apply (SeqCnt (env, s :: stmts) :: rest, None) -> Stmt (SeqCnt (env, stmts) :: rest, env, s)
  | Apply (SeqCnt (_, _) :: rest, x) -> Apply (rest, x)
  | _ -> failwith "type mismatch"

let driver env stmts =
  let rec iter config = match config with
    | Apply ([], value) -> value
    | _ -> iter (step config)
  in iter (Apply ([SeqCnt(env, stmts)], None))

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
  | None -> ()
  | _ -> failwith "type mismatch"
