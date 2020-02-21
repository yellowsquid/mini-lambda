(* OCaml interpretter for language *)

open Typed_ast
open Ops

exception Error of Typed_ast.loc * string

type value
  = None
  | Break of int
  | Continue of int
  | Unit
  | Bool of bool
  | Int of int
  | Lambda of (env * value array -> value)
  | Enum of int * value array
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value array
  ; args: value array
  ; debug: bool
  }

let value_to_string value = match value with
  | None -> "!"
  | Break id -> Printf.sprintf "Break(%d)" id
  | Continue id -> Printf.sprintf "Continue(%d)" id
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Lambda _ -> "λ"
  | Enum (var, _) -> Printf.sprintf "Enum(%d)" var

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
  | Equal, Unit, Unit -> Bool true
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _, _, _ -> raise (Error (pos, bin_type_mismatch op lhs rhs))

let do_unary_op pos op e = match op, e with
  | Invert, Bool b -> Bool (not b)
  | _, _ -> raise (Error (pos, unary_type_mismatch op e))

(* Proof: Observation *)
let rec interpret_expr env expr =
  let result = match expr with
    | FuncExpr (_, id) -> env.funcs.(id)
    | EnvExpr (_, id) -> env.captures.(id)
    | BoundExpr (_, id) -> env.binds.(id)
    | ArgExpr (_, id) -> env.args.(id)
    | IntExpr (_, i) -> Int i
    | BoolExpr (_, b) -> Bool b
    | BinExpr (pos, op, lhs, rhs) ->
       let lhs' = interpret_expr env lhs in
       let rhs' = interpret_expr env rhs in
       do_bin_op pos op lhs' rhs'
    | UnaryExpr (pos, op, e) ->
       let e' = interpret_expr env e in
       do_unary_op pos op e'
    | LambdaExpr (_, params, captures, body) -> Lambda (eval_lambda env params captures body)
    | CallExpr (pos, callee, args) ->
       let callee' = interpret_expr env callee in
       let args' = Array.map (interpret_expr env) args in
       (match callee' with
        | Lambda f -> f(env, args')
        | _ ->
           let ty_callee = value_to_string callee' in
           let msg = Printf.sprintf "type mismatch for call: got %s" ty_callee in
           raise (Error (pos, msg)))
    | ConstructorExpr (_, variant, exprs) ->
       let exprs' = Array.map (interpret_expr env) exprs in
       Enum (variant, exprs') in
  if env.debug
  then
      (Typed_ast.pp_expr Format.std_formatter expr;
       Format.printf " -> %s@\n" (value_to_string result));
  result
and eval_lambda env params captures body  =
  let capture expr =
    let expr' = interpret_expr env expr in
    (if env.debug then
       Format.printf "Capture <- %s@\n" (value_to_string expr'));
    expr'
  in
  let captured = Array.map capture captures in
  let eval (env, args) =
    if Array.length args != params then
      failwith "wrong number of args"
    else
      let env' = { funcs = env.funcs
                 ; captures = captured
                 ; binds = env.binds
                 ; args
                 ; debug = env.debug
                 } in
      interpret_expr env' body
  in
  eval

let rec pattern_matches env v pattern = match v, pattern with
  | Enum (var, params), Typed_ast.Enum (_, variant, patterns) when var = variant ->
     List.fold_left2 (fun (matched, env) v pattern ->
         if matched
         then pattern_matches env v pattern
         else false, env) (true, env) (Array.to_list params) patterns
  | _, Enum _ -> false, env
  | x, Variable (_, id) ->
     let binds = Array.copy env.binds in
     binds.(id) <- x;
     let env' = { funcs = env.funcs
                ; captures = env.captures
                ; binds
                ; args = env.args
                ; debug = env.debug
                } in
     true, env'

let rec interpret_stmt env stmt =
  if env.debug then begin
      Format.printf "Step %d@\n" !step;
      Typed_ast.pp_stmt Format.std_formatter stmt;
      Format.printf "@\n";
      ignore (read_line ());
      incr step
    end;

  match stmt with
  | ReturnStmt (_, e) -> interpret_expr env e
  | ExprStmt (_, e) -> ignore (interpret_expr env e); None
  | BindStmt (_, id, e) ->
     let e' = interpret_expr env e in
     env.binds.(id) <- e';
     (if env.debug then
        Format.printf "Bound(%d) <- %s@\n@\n" id (value_to_string e')
     );
     None
  | MatchStmt (_, e, cases) ->
     let e' = interpret_expr env e in
     let matcher = pattern_matches env e' in
     cases
     |> List.fold_left (fun (v, matched) (_, pattern, block) ->
            if matched
            then v, true
            else
              let was_match, env' = matcher pattern in
              if was_match
              then
                (Array.iteri (Array.set env.binds) env'.binds;
                 apply_block env block, true)
              else None, false) (None, false)
     |> fst
  | IfStmt (pos, cond, then_block, else_block) ->
     (match interpret_expr env cond with
      | Bool true -> apply_block env then_block
      | Bool false -> apply_block env else_block
      | _ -> raise(Error(pos, "condition with not a bool")))
  | WhileStmt (pos, id, cond, lblock, eblock) ->
     (match interpret_expr env cond with
      | Bool true ->
         (match apply_block env lblock with
          | None -> interpret_stmt env (WhileStmt (pos, id, cond, lblock, eblock))
          | Break id' when id' = id -> None
          | Continue id' when id' = id ->
             interpret_stmt env (WhileStmt (pos, id, cond, lblock, eblock))
          | x -> x)
      | Bool false -> apply_block env eblock
      | _ -> raise (Error (pos, "condition with not a bool")))
  | ContinueStmt (_, id) -> Continue id
  | BreakStmt (_, id) -> Break id
and apply_block env stmts =
  match stmts with
  | [] -> None
  | stmt :: rest ->
     (match interpret_stmt env stmt with
      | None -> apply_block env rest
      | x -> x)

module FuncMap = Map.Make(String)

let print_int (_env, x_array) =
  if Array.length x_array != 1 then
    failwith "wrong number of args"
  else match Array.get x_array 0 with
       | Int(x) -> print_int x; print_newline (); Unit
       | _ -> failwith "type mismatch"

let print_bool (_env, b_array) =
  if Array.length b_array != 1 then
    failwith "wrong number of args"
  else match Array.get b_array 0 with
       | Bool(b) -> print_string (string_of_bool b); print_newline (); Unit
       | _ -> failwith "type mismatch"

let input_int (_env, empty_array) =
  if Array.length empty_array != 0 then
    failwith "wrong number of args"
  else
    Int(read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Lambda(print_int)
                                           ; "print_bool", Lambda(print_bool)
                                           ; "input_int", Lambda(input_int)])


let interpret_func (func: Typed_ast.func) =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    let eval (env, args) =
      if Array.length args != func.num_params then
        failwith "wrong number of args"
      else
        let env' = { funcs = env.funcs
                   ; captures = Array.of_list []
                   ; binds = Array.init func.num_locals (fun _ -> Unit)
                   ; args = args
                   ; debug = env.debug
                   }
        in
        apply_block env' (Option.get func.body)
    in Lambda(eval)

let interpret debug program  =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  let funcs = Array.map interpret_func  program' in
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { funcs = funcs
            ; captures = Array.of_list []
            ; binds = Array.of_list []
            ; args = Array.of_list []
            ; debug = debug
            } in
  let func_expr = Typed_ast.FuncExpr(Lexing.dummy_pos, main.id) in
  let args = Array.of_list [] in
  let call_expr = Typed_ast.CallExpr(Lexing.dummy_pos, func_expr, args) in
  (ignore (interpret_expr env call_expr))
