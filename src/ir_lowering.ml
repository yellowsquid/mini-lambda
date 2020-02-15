(* Compiler Construction - Minimal Lambda Language *)

open Ir
open Typed_ast

module FuncMap = Map.Make(String)

type env =
  { locals: int
  ; args: int
  }

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", 1 ; "print_bool", 1 ; "input_int", 0])

let rec cleanup directives cnt = match directives with
  | [] -> cnt []
  | Call (args, block) :: _ -> cnt [Call (args, block)]
  | Return (locals, args) :: _ -> cnt [Return (locals, args)]
  | Jump block :: _ -> cnt [Jump block]
  | If (tblock, fblock) :: _ -> cnt [If (tblock, fblock)]
  | x :: rest -> cleanup rest (fun rest' -> cnt (x :: rest'))

let add_block, flatten_blocks, reserve_loop, get_loop, get_continue, set_loop =
  let block_id = ref 0 in
  let next_block () =
    let block = !block_id in
    incr block_id; block in
  let blocks = ref BlockMap.empty in
  let loops = ref BlockMap.empty in
  let eval_add block =
    let this_block = next_block () in
    cleanup block (fun block' ->
        blocks := BlockMap.add this_block block' !blocks;
        this_block) in
  let eval_flattens () = !blocks |> BlockMap.to_seq |> Seq.map snd |> Array.of_seq in
  let reserve_loop id continue =
    let this_block = next_block () in
    loops := BlockMap.add id (this_block, continue) !loops in
  let get_loop id = fst (BlockMap.find id !loops) in
  let get_continue id = snd (BlockMap.find id !loops) in
  let set_loop id block =
    cleanup block (fun block' -> blocks := BlockMap.add (get_loop id) block' !blocks) in
  eval_add, eval_flattens, reserve_loop, get_loop, get_continue, set_loop

(* Stack is args, return address, locals, temp, ... *)
(* Depth points to return address *)
let rec flatten_expr env acc depth expr = match expr with
  | FuncExpr (_, id) -> PushFunc id :: acc
  | EnvExpr (_, id) -> PushStack (depth - id - 1) :: acc
  | BoundExpr (_, id) -> PushStack (depth - id - 1) :: acc
  | ArgExpr (_, id) -> PushStack (depth + env.args - id) :: acc
  | IntExpr (_, i) -> Push (Int i) :: acc
  | BoolExpr (_, b) -> Push (Bool b) :: acc
  | BinExpr (_, op, lhs, rhs) ->
     flatten_expr env (flatten_expr env (BinOp op :: acc) (depth + 1) rhs) depth lhs
  | UnaryExpr (_, op, e) -> flatten_expr env (UnaryOp op :: acc) depth e
  | LambdaExpr (_, params, captures, body) ->
     let capturec = Array.length captures in
     let env' = { locals = capturec; args = params } in
     let body' = flatten_expr env' [Return (capturec, params)] capturec body in
     let block = add_block (CopyLocals (capturec, params) :: body') in
     let acc' = AllocHeap (capturec, Func (Array.length captures, block)) :: acc in
     let depth_acc = (depth + capturec - 1, acc') in
     snd (List.fold_left (flatten_exprs env) depth_acc (List.rev (Array.to_list captures)))
  | CallExpr (_, callee, args) ->
     let return = add_block acc in
     let argc = Array.length args in
     let call' = Call (argc, return) in
     let depth_acc = (depth + argc, [call']) in
     let acc' = List.fold_left (flatten_exprs env) depth_acc (List.rev (Array.to_list args)) in
     flatten_expr env (snd acc') depth callee
and flatten_exprs env (depth, acc) expr = depth - 1, flatten_expr env acc depth expr

let rec flatten_stmt env acc stmt =
  let depth = env.locals in
  match stmt with
  | ReturnStmt (_, e) -> flatten_expr env (Return (env.locals, env.args) :: acc) depth e
  | ExprStmt (_, e) -> flatten_expr env (Pop :: acc) depth e
  | BindStmt (_, id, e) -> flatten_expr env (Bind (depth - id) :: acc) depth e
  | IfStmt (_, cond, tblock, fblock) ->
     let continue = [Jump (add_block acc)] in
     let tblock' = add_block (make_block env tblock continue) in
     let fblock' = add_block (make_block env fblock continue) in
     let acc' = [If (tblock', fblock')] in
     flatten_expr env acc' depth cond
  | WhileStmt (_, id, cond, lblock, eblock) ->
     let continue = add_block acc in
     reserve_loop id continue;
     let lblock' = add_block (make_block env lblock [Jump (get_loop id)]) in
     let eblock' = add_block (make_block env eblock [Jump continue]) in
     set_loop id (flatten_expr env [If (lblock', eblock')] depth cond);
     [Jump (get_loop id)]
  | ContinueStmt (_, id) -> Jump (get_loop id) :: acc
  | BreakStmt (_, id) -> Jump (get_continue id) :: acc
and make_block env stmts acc =
  List.fold_left (flatten_stmt env) acc (List.rev stmts)

let lower_func _debug func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let params = FuncMap.find func.name builtins in
      0, add_block [Builtin func.name; Return (0, params)]
    else
      failwith "built-in has no definition"
  else
    let locals, args = func.num_locals, func.num_params in
    let env = { locals = locals; args = args } in
    let block = make_block env (Option.get func.body) [Push Unit; Return (locals, args)] in
    let block' = add_block (CopyLocals (locals, args) :: block) in
    locals, block'

let lower debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  let funcs = Array.map (lower_func debug) program' in
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  { blocks = flatten_blocks (); funcs = funcs; main = main.id }
