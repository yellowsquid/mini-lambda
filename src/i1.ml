(* OCaml interpretter for language *)

open Typed_ast
open Ops

exception Error of loc * string

type value
  = None
  | Unit
  | Bool of bool
  | Int of int
  | Lambda of (env -> value list -> (value -> value) -> value)
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  ; debug: bool
  }

let value_to_string value = match value with
  | None -> "!"
  | Unit -> "()"
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Lambda(_) -> "λ"

let step = ref 0

let bin_type_mismatch op lhs rhs =
  let val_lhs = value_to_string lhs in
  let val_rhs = value_to_string rhs in
  let val_op = string_of_bin_op op in
  Printf.sprintf "type mismatch for %s: got %s and %s" val_op val_lhs val_rhs

let unary_type_mismatch op e =
  let val_e = value_to_string e in
  let val_op = string_of_unary_op op in
  Printf.sprintf "type mismatch for %s: got %s" val_op val_e

let do_bin_op pos op lhs rhs = match op, lhs, rhs with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Equal, Int a, Int b -> Bool (a = b)
  | Equal, Bool a, Bool b -> Bool (a = b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _, _, _ -> raise (Error (pos, bin_type_mismatch op lhs rhs))

let do_unary_op pos op e = match op, e with
  | Invert, Bool b -> Bool (not b)
  | _, _ -> raise (Error (pos, unary_type_mismatch op e))

let rec fold_left_cnt f acc list cnt = match list with
  | [] -> cnt acc
  | x :: rest -> f acc x (fun acc' -> fold_left_cnt f acc' rest cnt)

let print_cnt expr debug =
  if debug then
    let eval cnt value =
      pp_expr Format.std_formatter expr;
      Format.printf " -> %s" (value_to_string value);
      Format.print_newline ();
      cnt value in
    eval
  else
    let eval cnt = cnt in
    eval

let rec eval_expr env expr cnt =
  let cnt = print_cnt expr env.debug cnt in
  match expr with
  (* Push the func to stack top before applying *)
  | FuncExpr(_, id) -> cnt (Array.get env.funcs id)
  (* Push the local to stack top before applying *)
  | EnvExpr(_, id) -> cnt (Array.get env.captures id)
  (* Push the local to stack top before applying *)
  | BoundExpr(_, id) -> cnt !(Array.get env.binds id)
  (* Push the arg to stack top before applying *)
  | ArgExpr(_, id) -> cnt (Array.get env.args id)
  (* Push the int to stack top before applying *)
  | IntExpr(_, i) -> cnt (Int i)
  (* Push the bool to stack top before applying *)
  | BoolExpr(_, b) -> cnt (Bool b)
  (* Push first then second arg then do op before applying *)
  | BinExpr(pos, op, lhs, rhs) ->
     eval_expr env lhs (fun lhs' ->
         eval_expr env rhs (fun rhs' -> cnt (do_bin_op pos op lhs' rhs')))
  (* Push first then second arg then invert before applying *)
  | UnaryExpr(pos, op, e) ->
     eval_expr env e (fun e' -> cnt (do_unary_op pos op e'))
  (* Capture args then construct function before applying *)
  | LambdaExpr(_, _, captures, body) ->
     fold_left_cnt (eval_exprs env) [] (List.rev (Array.to_list captures)) (fun captures' ->
         let eval env' args cnt' =
           let env'' = { funcs = env'.funcs
                       ; captures = (Array.of_list captures')
                       ; binds = Array.of_list []
                       ; args = Array.of_list args
                       ; debug = env'.debug
                       } in
           eval_expr env'' body cnt' in
         cnt (Lambda eval)
       )
  (* Push function then args before calling and then applying *)
  | CallExpr(_, callee, args) ->
     eval_expr env callee (fun callee' ->
         fold_left_cnt (eval_exprs env) [] (Array.to_list args) (fun args' ->
             match callee' with
             | Lambda f -> f env (List.rev args') cnt
             | _ -> failwith "expected lambda"))
and eval_exprs env acc expr cnt = eval_expr env expr (fun value -> cnt (value :: acc))

let rec eval_stmt env stmt cnt =
  if env.debug then begin
      incr step;
      Format.printf "Step %d" !step;
      Format.print_newline ();
      pp_stmt Format.std_formatter stmt;
      Format.print_newline ();
      Format.print_newline ()
    end;
  match stmt with
  (* Calculate return value then apply *)
  | ReturnStmt(_, e) ->
     eval_expr env e cnt
  (* Eval expression then continue on *)
  | ExprStmt(_, e) ->
     eval_expr env e (fun _ -> cnt None)
  (* Bind expression then continue on *)
  | BindStmt(_, id, e) ->
     eval_expr env e (fun e' -> Array.get env.binds id := e'; cnt None)
  (* Evaluate condition then block. If block return then return, else continue on *)
  | IfStmt(_, cond, tblock, fblock) ->
     eval_expr env cond (fun cond' ->
         let block = match cond' with
           | Bool true -> tblock
           | Bool false -> fblock
           | _ -> failwith "expected bool" in
         fold_left_cnt (eval_stmts env cnt) None block cnt)
and eval_stmts env escape _ stmt cnt =
  eval_stmt env stmt (fun value ->
      match value with
      | None -> cnt None
      | x -> escape x)

module FuncMap = Map.Make(String)

let print_int _env x_list cnt = match x_list with
  | [Int x] -> print_int x; print_newline(); cnt Unit
  | _ -> failwith "type mismatch"

let print_bool _env b_list cnt = match b_list with
  | [Bool b] -> print_string (string_of_bool b); print_newline (); cnt Unit
  | _ -> failwith "type mismatch"

let input_int _env empty_list cnt = match empty_list with
  | [] -> cnt (Int (read_int ()))
  | _ -> failwith "type mismatch"

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Lambda(print_int)
                                           ; "print_bool", Lambda(print_bool)
                                           ; "input_int", Lambda(input_int)])

let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    let eval env args cnt =
      if List.length args != func.num_params then
        failwith "wrong number of args"
      else
        let env' = { funcs = env.funcs
                   ; captures = Array.of_list []
                   ; binds = Array.init func.num_locals (fun _ -> ref Unit)
                   ; args = Array.of_list args
                   ; debug = env.debug
                   } in
        fold_left_cnt (eval_stmts env' cnt) None (Option.get func.body) cnt in
    Lambda eval

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun (a: Typed_ast.func) b -> compare a.id b.id) program';
  let funcs = Array.map interpret_func program' in
  let main = List.find (fun (f: Typed_ast.func) -> f.name = "main") (Array.to_list program') in
  let env = { funcs = funcs
            ; captures = Array.of_list []
            ; binds = Array.of_list []
            ; args = Array.of_list []
            ; debug = debug
            }
  in
  let func_expr = Typed_ast.FuncExpr(Lexing.dummy_pos, main.id) in
  let args = Array.of_list [] in
  let call_expr = Typed_ast.CallExpr(Lexing.dummy_pos, func_expr, args) in
  eval_expr env call_expr (fun x ->
      match x with
      | None -> None
      | _ -> failwith "type mismatch")
|> ignore
