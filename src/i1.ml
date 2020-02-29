(* CPS interpretter. Everything is a continuation. *)

open Typed_ast
open Ops

exception Error of loc * string

type value
  = None
  | Break of int
  | Continue of int
  | Unit
  | Bool of bool
  | Int of int
  | Lambda of (env -> value list -> (value -> value) -> value)
  | Enum of int * value list
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value array
  ; args: value array
  ; debug: bool
  }

let rec value_to_string value = match value with
  | None -> "!"
  | Break id -> Printf.sprintf "Break(%d)" id
  | Continue id -> Printf.sprintf "Continue(%d)" id
  | Unit -> "()"
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Lambda(_) -> "λ"
  | Enum (var, params) ->
     let params' = String.concat ", " ((string_of_int var) :: List.map value_to_string params) in
     Printf.sprintf "Enum(%s)" params'

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
  | x :: rest -> f cnt acc x (fun acc' -> fold_left_cnt f acc' rest cnt)

let print_cnt expr debug =
  if debug
  then
    (fun cnt value ->
      pp_expr Format.std_formatter expr;
      Format.printf " -> %s" (value_to_string value);
      Format.print_newline ();
      cnt value)
  else
    (fun cnt -> cnt)

let rec eval_expr env expr cnt =
  let cnt = print_cnt expr env.debug cnt in
  match expr with
  (* Push the func to stack top before applying *)
  | FuncExpr (_, id) -> cnt env.funcs.(id)
  (* Push the local to stack top before applying *)
  | EnvExpr (_, id) -> cnt env.captures.(id)
  (* Push the local to stack top before applying *)
  | BoundExpr (_, id) -> cnt env.binds.(id)
  (* Push the arg to stack top before applying *)
  | ArgExpr (_, id) -> cnt env.args.(id)
  (* Push the int to stack top before applying *)
  | IntExpr (_, i) -> cnt (Int i)
  (* Push the bool to stack top before applying *)
  | BoolExpr (_, b) -> cnt (Bool b)
  (* Push first then second arg then do op before applying *)
  | BinExpr (pos, op, lhs, rhs) ->
     eval_expr env lhs (fun lhs' ->
         eval_expr env rhs (fun rhs' -> cnt (do_bin_op pos op lhs' rhs')))
  (* Push first then second arg then invert before applying *)
  | UnaryExpr (pos, op, e) ->
     eval_expr env e (fun e' -> cnt (do_unary_op pos op e'))
  (* Capture args then construct function before applying *)
  | LambdaExpr (_, _, captures, body) ->
     fold_left_cnt (eval_exprs env) [] (captures |> Array.to_list |> List.rev) (fun captures' ->
         let eval env' args cnt' =
           let env'' = { funcs = env'.funcs
                       ; captures = Array.of_list captures'
                       ; binds = Array.of_list []
                       ; args = Array.of_list args
                       ; debug = env'.debug
                       } in
           eval_expr env'' body cnt' in
         cnt (Lambda eval))
  (* Push function then args before calling and then applying *)
  | CallExpr(_, callee, args) ->
     eval_expr env callee (fun callee' ->
         fold_left_cnt (eval_exprs env) [] (Array.to_list args) (fun args' ->
             match callee' with
             | Lambda f -> f env (List.rev args') cnt
             | _ -> failwith "expected lambda"))
  (* Capture values before wrapping up *)
  | ConstructorExpr (_, variant, args) ->
     fold_left_cnt (eval_exprs env) [] (args |> Array.to_list |> List.rev) (fun args' ->
         cnt (Enum (variant, args')))
and eval_exprs env _ acc expr cnt = eval_expr env expr (fun value -> cnt (value :: acc))

(* Evaluate patterns depth-first, left-to-right *)
let rec eval_pattern env v pattern cnt =
  if env.debug
  then
    (Format.printf "@[";
     pp_pattern Format.std_formatter pattern;
     Format.printf "@ <~>@ %s@]" (value_to_string v);
     Format.print_newline ());
  match v, pattern with
  | x, Variable (_, id) ->
     let binds = Array.copy env.binds in
     binds.(id) <- x;
     let env' = { funcs = env.funcs
                ; captures = env.captures
                ; binds
                ; args = env.args
                ; debug = env.debug
                } in
     cnt (true, env')
  | Enum (var, params), Enum (_, variant, patterns) when var = variant ->
     fold_left_cnt eval_patterns (true, env) (List.combine params patterns) cnt
  | Enum _, Enum _ -> cnt (false, env)
  | _, Enum _ -> failwith "typechecker failed with pattern matching"
  | _, Ignore _ -> cnt (true, env)
  | Int i, Int (_, j) -> cnt (i = j, env)
  | _, Int _ -> failwith "typechecker failed with pattern matching"
  | Bool b, Bool (_, b') -> cnt (b = b', env)
  | _, Bool _ -> failwith "typechecker failed with pattern matching"
and eval_patterns escape (_, env) (v, pattern) cnt =
  eval_pattern env v pattern (fun ((matched, _) as ret) -> if matched then cnt ret else escape ret)

let rec eval_stmt env stmt cnt =
  if env.debug
  then
    (incr step;
     Format.printf "Step %d" !step;
     Format.print_newline ();
     pp_stmt Format.std_formatter stmt;
     Format.print_newline ();
     Format.print_newline ());
  match stmt with
  (* Calculate return value then apply *)
  | ReturnStmt (_, e) ->
     eval_expr env e cnt
  (* Eval expression then continue on *)
  | ExprStmt (_, e) ->
     eval_expr env e (fun _ -> cnt None)
  (* Bind expression then continue on *)
  | BindStmt (_, id, e) ->
     eval_expr env e (fun e' -> env.binds.(id) <- e'; cnt None)
  (* Eval expression then start matching cases *)
  | MatchStmt (_, e, cases) ->
     eval_expr env e (fun e' -> fold_left_cnt (eval_case env e') None cases cnt)
  (* Evaluate condition then block. If block return then return, else continue on *)
  | IfStmt (_, cond, tblock, fblock) ->
     eval_expr env cond (fun cond' ->
         let block = match cond' with
           | Bool true -> tblock
           | Bool false -> fblock
           | _ -> failwith "expected bool" in
         fold_left_cnt (eval_stmts env) None block cnt)
  (* Evaluate condition then either first block or second. May have to loop *)
  | WhileStmt (pos, id, cond, lblock, eblock) ->
     eval_expr env cond (fun cond' ->
         match cond' with
         | Bool false -> fold_left_cnt (eval_stmts env) None eblock cnt
         | Bool true ->
            fold_left_cnt (eval_stmts env) None lblock (fun loop' ->
                match loop' with
                | None -> eval_stmt env (WhileStmt (pos, id, cond, lblock, eblock)) cnt
                | Break id' when id' = id -> cnt None
                | Continue id' when id' = id ->
                   eval_stmt env (WhileStmt (pos, id, cond, lblock, eblock)) cnt
                | x -> cnt x)
         | _ -> failwith "expected bool")
  | ContinueStmt (_, id) -> cnt (Continue id)
  | BreakStmt (_, id) -> cnt (Break id)
and eval_stmts env escape _ stmt cnt =
  eval_stmt env stmt (fun value ->
      match value with
      | None -> cnt None
      | x -> escape x)
(* If a pattern matches, eval block and stop. Else continue *)
and eval_case env e escape _ (_, pattern, block) cnt =
  eval_pattern env e pattern (fun (matched, env') ->
      if matched
      then
        (Array.iteri (Array.set env.binds) env'.binds;
         fold_left_cnt (eval_stmts env) None block escape)
      else cnt None)

module FuncMap = Map.Make(String)

let print_int _env list cnt = match list with
  | [Int x] -> print_int x; print_newline(); cnt Unit
  | _ ->
     let list = String.concat ", " (List.map value_to_string list) in
     let msg = Format.sprintf "type mismatch for print_int: expected [int] got [%s]" list in
     failwith msg

let print_bool _env list cnt = match list with
  | [Bool b] -> print_string (string_of_bool b); print_newline (); cnt Unit
  | _ ->
     let list = String.concat ", " (List.map value_to_string list) in
     let msg = Format.sprintf "type mismatch for print_bool: expected [bool] got [%s]" list in
     failwith msg

let input_int _env list cnt = match list with
  | [] -> cnt (Int (read_int ()))
  | _ ->
     let list = String.concat ", " (List.map value_to_string list) in
     let msg = Format.sprintf "type mismatch for input_int: expected [] got [%s]" list in
     failwith msg

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
                   ; binds = Array.make func.num_locals Unit
                   ; args = Array.of_list args
                   ; debug = env.debug
                   } in
        fold_left_cnt (eval_stmts env') None (Option.get func.body) cnt in
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
      | _ -> failwith "type mismatch on program termination")
  |> ignore
