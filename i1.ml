(* OCaml interpretter for language *)

open Typed_ast

type value
  = Unit
  | Bool of bool
  | Int of int
  | Func of (env -> value list -> value)
  | Lambda of int * value array * expr
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  }

type cnt
  = ConsCnt of value
  | AddCnt of env * expr list
  | SubCnt of env * expr list
  | EqualCnt of env * expr list
  | AndCnt of env * expr list
  | OrCnt of env * expr list
  | InvertCnt of env * expr list
  | LambdaCnt of env * expr list * int * expr
  | CalleeCnt of env * expr list * expr list
  | CallCnt of env * expr list * value list
  | CallLambdaCnt of env * expr list
  | IgnoreCnt of env * statement list
  | BindCnt of env * statement list * id
  | IfCnt of env * statement list * statement list * statement list
  | IfBlockCnt of env * statement list

type step
  = Expr of cnt list * env * expr list
  | Stmt of cnt list * env * statement list
  | Apply of cnt list * value list

let step config = match config with
  | Expr (cnts, _env, []) -> Apply (cnts, [])
  | Expr (cnts, env, FuncExpr (_, id) :: rest) ->
     Expr (ConsCnt (Array.get env.funcs id) :: cnts, env, rest)
  | Expr (cnts, env, EnvExpr (_, id) :: rest) ->
     Expr (ConsCnt (Array.get env.captures id) :: cnts, env, rest)
  | Expr (cnts, env, BoundExpr (_, id) :: rest) ->
     Expr (ConsCnt !(Array.get env.binds id) :: cnts, env, rest)
  | Expr (cnts, env, ArgExpr (_, id) :: rest) ->
     Expr (ConsCnt (Array.get env.args id) :: cnts, env, rest)
  | Expr (cnts, env, IntExpr (_, i) :: rest) ->
     Expr (ConsCnt (Int i) :: cnts, env, rest)
  | Expr (cnts, env, BoolExpr (_, b) :: rest) ->
     Expr (ConsCnt (Bool b) :: cnts, env, rest)
  | Expr (cnts, env, AddExpr (_, lhs, rhs) :: rest) ->
     Expr (AddCnt (env, rest) :: cnts, env, [lhs; rhs])
  | Expr (cnts, env, SubExpr (_, lhs, rhs) :: rest) ->
     Expr (SubCnt (env, rest) :: cnts, env, [lhs; rhs])
  | Expr (cnts, env, EqualExpr (_, lhs, rhs) :: rest) ->
     Expr (EqualCnt (env, rest) :: cnts, env, [lhs; rhs])
  | Expr (cnts, env, AndExpr (_, lhs, rhs) :: rest) ->
     Expr (AndCnt (env, rest) :: cnts, env, [lhs; rhs])
  | Expr (cnts, env, OrExpr (_, lhs, rhs) :: rest) ->
     Expr (OrCnt (env, rest) :: cnts, env, [lhs; rhs])
  | Expr (cnts, env, InvertExpr (_, e) :: rest) ->
     Expr (InvertCnt (env, rest) :: cnts, env, [e])
  | Expr (cnts, env, LambdaExpr (_, params, captures, body) :: rest) ->
     Expr (LambdaCnt (env, rest, params, body) :: cnts, env, Array.to_list captures)
  | Expr (cnts, env, CallExpr (_, callee, args) :: rest) ->
     Expr (CalleeCnt (env, rest, Array.to_list args) :: cnts, env, [callee])
  | Stmt (cnts, _env, []) -> Apply (cnts, [])
  | Stmt (cnts, env, ReturnStmt (_, e) :: _rest) -> Expr (cnts, env, [e]) (* FIXME: what is cnts? *)
  | Stmt (cnts, env, ExprStmt (_, e) :: rest) ->
     Expr (IgnoreCnt (env, rest) :: cnts, env, [e])
  | Stmt (cnts, env, BindStmt (_, id, e) :: rest) ->
     Expr (BindCnt (env, rest, id) :: cnts, env, [e])
  | Stmt (cnts, env, IfStmt (_, cond, then_block, else_block) :: rest) ->
     Expr (IfCnt (env, rest, then_block, else_block) :: cnts, env, [cond])
  | Apply ([], values) -> Apply ([], values)
  | Apply (ConsCnt value :: rest, values) -> Apply (rest, value :: values)
  | Apply (AddCnt (env, stack) :: rest, [Int a; Int b]) ->
     Expr (ConsCnt (Int (a + b)) :: rest, env, stack)
  | Apply (SubCnt (env, stack) :: rest, [Int a; Int b]) ->
     Expr (ConsCnt (Int (a - b)) :: rest, env, stack)
  | Apply (EqualCnt (env, stack) :: rest, [Int a; Int b]) ->
     Expr (ConsCnt (Bool (a = b)) :: rest, env, stack)
  | Apply (EqualCnt (env, stack) :: rest, [Bool a; Bool b]) ->
     Expr (ConsCnt (Bool (a = b)) :: rest, env, stack)
  | Apply (AndCnt (env, stack) :: rest, [Bool a; Bool b]) ->
     Expr (ConsCnt (Bool (a && b)) :: rest, env, stack)
  | Apply (OrCnt (env, stack) :: rest, [Bool a; Bool b]) ->
     Expr (ConsCnt (Bool (a || b)) :: rest, env, stack)
  | Apply (InvertCnt (env, stack) :: rest, [Bool b]) ->
     Expr (ConsCnt (Bool (not b)) :: rest, env, stack)
  | Apply (LambdaCnt (env, stack, params, body) :: rest, values) ->
     Expr (ConsCnt (Lambda (params, Array.of_list values, body)) :: rest, env, stack)
  | Apply (CalleeCnt (env, stack, args) :: rest, values) ->
     Expr (CallCnt (env, stack, values) :: rest, env, args)
  | Apply (CallCnt (env, stack, [Lambda (params, captures, body)]) :: rest, values)
    when params = List.length values ->
     let env' = { funcs = env.funcs
                ; captures = captures
                ; binds = Array.of_list []
                ; args = Array.of_list values
                }
     in
     Expr (CallLambdaCnt (env, stack) :: rest, env', [body])
  | Apply (CallCnt (env, stack, [Func f]) :: rest, values) ->
     Expr (ConsCnt (f env values) :: rest, env, stack)
  | Apply (CallLambdaCnt (env, stack) :: rest, [e]) ->
     Expr (ConsCnt e :: rest, env, stack)
  | Apply (IgnoreCnt (env, stmts) :: rest, _values) ->
     Stmt (rest, env, stmts)
  | Apply (BindCnt (env, stmts, id) :: rest, [e]) ->
     (Array.get env.binds id) := e;
     Stmt (rest, env, stmts)
  | Apply (IfCnt (env, stmts, then_block, _else_block) :: rest, [Bool true]) ->
     Stmt (IfBlockCnt (env, stmts) :: rest, env, then_block)
  | Apply (IfCnt (env, stmts, _then_block, else_block) :: rest, [Bool false]) ->
     Stmt (IfBlockCnt (env, stmts) :: rest, env, else_block)
  | Apply (IfBlockCnt (_env, _stmts) :: rest, [x]) -> Apply (rest, [x]) (* FIXME: what is cnts? *)
  | Apply (IfBlockCnt (env, stmts) :: rest, []) -> Stmt (rest, env, stmts)
  | _ -> failwith "type mismatch"

  (* | LambdaCnt (env, stack, params, body) :: rest ->
   *    let eval env' args =
   *      if List.length args != params then
   *        failwith "wrong number of args"
   *      else
   *        let env'' = { funcs = env'.funcs
   *                    ; captures = (Array.of_list values)
   *                    ; binds = Array.of_list []
   *                    ; args = Array.of_list args
   *                    } in
   *        match interpret_exprs_cnt env'' [body] [] with
   *        | [x] -> x
   *        | _ -> failwith "type mismatch"
   *    in
   *    let value = Lambda eval in
   *    interpret_exprs_cnt env stack (ConsCnt value :: rest) *)
let driver env stmts =
  let rec iter config = match config with
    | Apply ([], values) -> values
    | _ -> iter (step config)
  in iter (Stmt ([], env, stmts))

module FuncMap = Map.Make(String)

let print_int _env x_list = match x_list with
  | [Int x] -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool _env b_list = match b_list with
  | [Bool b] -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _env empty_list = match empty_list with
  | [] -> Int (read_int ())
  | _ -> failwith "type mismatch"

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Func print_int
                                           ; "print_bool", Func print_bool
                                           ; "input_int", Func input_int])


let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    let eval env args =
      if List.length args != func.num_params then
        failwith "wrong number of args"
      else
        let env' = { funcs = env.funcs
                   ; captures = Array.of_list []
                   ; binds = Array.init func.num_locals (fun _ -> ref Unit)
                   ; args = Array.of_list args
                   }
        in
        match driver env' (Option.get func.body) with
        | [x] -> x
        | [] -> Unit
        | _ -> failwith "type mismatch"
    in Func eval

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
  | [] -> ()
  | _ -> failwith "type mismatch"
