(* OCaml interpretter for language *)

open Typed_ast
open Ops

type value
  = None
  | Unit
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
  = OuterBinCnt of env * bin_op * expr
  | InnerBinCnt of bin_op * value
  | UnaryCnt of unary_op
  | LambdaCnt of env * expr list * value list * int * expr
  | CallCnt of env * expr list
  | ArgsCnt of env * expr list * value list * value
  | IgnoreCnt
  | BindCnt of env * id
  | IfCnt of env * statement list * statement list
  | WhileCnt of env * id * expr * statement list * statement list
  | ContinueCnt of id
  | BreakCnt of id
  | SeqCnt of env * statement list

type step
  = Expr of cnt list * env * expr
  | Stmt of cnt list * env * statement
  | Apply of cnt list * value


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

let step config = match config with
  (* Push function to top before applying *)
  | Expr (cnts, env, FuncExpr (_, id)) -> Apply (cnts, Array.get env.funcs id)
  (* Push local to top before applying *)
  | Expr (cnts, env, EnvExpr (_, id)) -> Apply (cnts, Array.get env.captures id)
  (* Push local to top before applying *)
  | Expr (cnts, env, BoundExpr (_, id)) -> Apply (cnts, !(Array.get env.binds id))
  (* Push arg to top before applying *)
  | Expr (cnts, env, ArgExpr (_, id)) -> Apply (cnts, Array.get env.args id)
  (* Push int to top before applying *)
  | Expr (cnts, _, IntExpr (_, i)) -> Apply (cnts, Int i)
  (* Push bool to top before applying *)
  | Expr (cnts, _, BoolExpr (_, b)) -> Apply (cnts, Bool b)
  (* Eval the first arg before continuing *)
  | Expr (cnts, env, BinExpr (_, op, lhs, rhs)) ->
     Expr (OuterBinCnt (env, op, rhs) :: cnts, env, lhs)
  (* Eval the arg before continuing *)
  | Expr (cnts, env, UnaryExpr (_, op, e)) -> Expr (UnaryCnt op :: cnts, env, e)

  (* Prepare to capture args *)
  | Expr (cnts, env, LambdaExpr (_, params, captures, body)) ->
     Apply (LambdaCnt (env, Array.to_list captures, [], params, body) :: cnts, None)
  (* Evaluate callee before continuing *)
  | Expr (cnts, env, CallExpr (_, callee, args)) ->
     Expr (CallCnt (env, Array.to_list args) :: cnts, env, callee)

  (* Calculate value then apply *)
  | Stmt (cnts, env, ReturnStmt (_, e)) -> Expr (cnts, env, e)
  (* Eval expression pop then continue *)
  | Stmt (cnts, env, ExprStmt (_, e)) -> Expr (IgnoreCnt :: cnts, env, e)
  (* Eval bind value then continue *)
  | Stmt (cnts, env, BindStmt (_, id, e)) -> Expr (BindCnt (env, id) :: cnts, env, e)
  | Stmt (_, _, MatchStmt _) -> failwith "todo"
  (* Eval condition then continue *)
  | Stmt (cnts, env, IfStmt (_, cond, tblock, fblock)) ->
     Expr (IfCnt (env, tblock, fblock) :: cnts, env, cond)
  (* Eval condition then continue *)
  | Stmt (cnts, env, WhileStmt (_, id, cond, lblock, eblock)) ->
     Expr (WhileCnt (env, id, cond, lblock, eblock) :: cnts, env, cond)
  (* Push continue control statement then continue *)
  | Stmt (cnts, _, ContinueStmt (_, id)) -> Apply (ContinueCnt id :: cnts, None)
  (* Push break control statement then continue *)
  | Stmt (cnts, _, BreakStmt (_, id)) -> Apply (BreakCnt id :: cnts, None)

  (* Noting to do so done *)
  | Apply ([], value) -> Apply ([], value)
  (* Eval the second arg before continuing *)
  | Apply (OuterBinCnt (env, op, rhs) :: rest, lhs) ->
     Expr (InnerBinCnt (op, lhs) :: rest, env, rhs)
  (* Compute and apply *)
  | Apply (InnerBinCnt (op, lhs) :: rest, rhs) -> Apply (rest, do_bin_op op lhs rhs)
  (* Compute and apply *)
  | Apply (UnaryCnt op :: rest, v) -> Apply (rest, do_unary_op op v)

  (* All args captured so push and apply *)
  | Apply (LambdaCnt (_, [], captured, p, body) :: rest, None) ->
     Apply (rest, Lambda (p, Array.of_list (List.rev captured), body))
  (* Evaluate the next arg *)
  | Apply (LambdaCnt (env, e :: exprs, captured, p, body) :: rest, None) ->
     Expr (LambdaCnt (env, exprs, captured, p, body) :: rest, env, e)
  (* Move arg from stack to lambda *)
  | Apply (LambdaCnt (env, to_capture, captured, p, body) :: rest, v) ->
     Apply (LambdaCnt (env, to_capture, v :: captured, p, body) :: rest, None)

  (* Prepare to evaluate args *)
  | Apply (CallCnt (env, args) :: rest, v) ->
     Apply (ArgsCnt (env, args, [], v) :: rest, None)

  (* All args evaluated so evaluate lambda body *)
  | Apply (ArgsCnt (env, [], parsed, Lambda (params, captures, body)) :: rest, None)
       when params = List.length parsed ->
     let env' = { funcs = env.funcs
                ; captures = captures
                ; binds = Array.of_list []
                ; args = Array.of_list (List.rev parsed)
                }
     in Expr (rest, env', body)
  (* All args evaluated so evaluate function body *)
  | Apply (ArgsCnt (env, [], parsed, Func (binds, params, body)) :: rest, None)
       when params = List.length parsed ->
     let env' = { funcs = env.funcs
                ; captures = Array.of_list []
                ; binds = Array.init binds (fun _ -> ref None)
                ; args = Array.of_list (List.rev parsed)
                }
     in Apply (SeqCnt (env', body) :: rest, None)
  (* All args evaluated so evaluate builtin *)
  | Apply (ArgsCnt (_, [], parsed, Builtin f) :: rest, None) ->
     Apply (rest, f (List.rev parsed))

  (* Evaluate the next arg *)
  | Apply (ArgsCnt (env, e :: exprs, parsed, callee) :: rest, None) ->
     Expr (ArgsCnt (env, exprs, parsed, callee) :: rest, env, e)
  (* Move arg from stack to call *)
  | Apply (ArgsCnt (env, exprs, parsed, callee) :: rest, v) ->
     Apply (ArgsCnt (env, exprs, v :: parsed, callee) :: rest, None)

  (* Ignore value and apply to None *)
  | Apply (IgnoreCnt :: rest, _) -> Apply (rest, None)
  (* Bind value and apply to None *)
  | Apply (BindCnt (env, id) :: rest, x) -> Array.get env.binds id := x; Apply (rest, None)
  (* If condition was true then evaluate tblock *)
  | Apply (IfCnt (env, tblock, _) :: rest, Bool true) -> Apply (SeqCnt (env, tblock) :: rest, None)
  (* If condition was false then evaluate fblock *)
  | Apply (IfCnt (env, _, fblock) :: rest, Bool false) -> Apply (SeqCnt (env, fblock) :: rest, None)

  (* While condition was true then evaluate loop block *)
  | Apply (WhileCnt (env, id, cond, lblock, eblock) :: rest, Bool true) ->
     Apply (SeqCnt (env, lblock) :: (WhileCnt (env, id, cond, lblock, eblock)) :: rest, None)
  (* While condition was false then evalue else block *)
  | Apply (WhileCnt (env, _, _, _, eblock) :: rest, Bool false) ->
     Apply (SeqCnt (env, eblock) :: rest, None)
  (* While loop finished so re-evaluate condition *)
  | Apply (WhileCnt (env, id, cond, lblock, eblock) :: rest, None) ->
     Expr (WhileCnt (env, id, cond, lblock, eblock) :: rest, env, cond)

  (* Continue followed by correct while so drop it *)
  | Apply (ContinueCnt id :: WhileCnt (env, id', cond, lblock, eblock) :: rest, None)
       when id' = id ->
     Expr (WhileCnt (env, id', cond, lblock, eblock) :: rest, env, cond)
  (* Continue followed by something else so march on *)
  | Apply (ContinueCnt id :: _ :: rest, None) -> Apply (ContinueCnt id :: rest, None)

  (* Break followed by correct while so drop it *)
  | Apply (BreakCnt id :: WhileCnt (_, id', _, _, _) :: rest, None) when id' = id ->
     Apply (rest, None)
  (* Break followed by something else so march on *)
  | Apply (BreakCnt id :: _ :: rest, None) -> Apply (BreakCnt id :: rest, None)

  (* No more statements so apply to None *)
  | Apply (SeqCnt (_, []) :: rest, None) -> Apply (rest, None)
  (* Returned none so eval next statement *)
  | Apply (SeqCnt (env, s :: stmts) :: rest, None) -> Stmt (SeqCnt (env, stmts) :: rest, env, s)
  (* Returned something so pass it up *)
  | Apply (SeqCnt (_, _) :: rest, x) -> Apply (rest, x)

  (* Reaching here is a bug *)
  | _ -> failwith "runtime error"

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

let interpret _bool program =
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
