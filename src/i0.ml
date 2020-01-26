(* OCaml interpretter for language *)

open Typed_ast

exception Error of Typed_ast.loc * string

type value
  = Unit
  | Bool of bool
  | Int of int
  | Lambda of (env * value array -> value)
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  ; debug: bool
  }

let value_to_string value = match value with
  | Unit -> "()"
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Lambda(_) -> "λ"

let step = ref 0

let type_mismatch op lhs rhs =
  let val_lhs = value_to_string lhs in
  let val_rhs = value_to_string rhs in
  Printf.sprintf "type mismatch for %s: got %s and %s" op val_lhs val_rhs

let rec interpret_expr env expr =
  let result = match expr with
    | FuncExpr(_, id) -> Array.get env.funcs id
    | EnvExpr(_, id) -> Array.get env.captures id
    | BoundExpr(_, id) -> !(Array.get env.binds id)
    | ArgExpr(_, id) -> Array.get env.args id
    | IntExpr(_, i) -> Int(i)
    | BoolExpr(_, b) -> Bool(b)
    | AddExpr(pos, lhs, rhs) ->
       (let lhs' = interpret_expr env lhs in
        let rhs' = interpret_expr env rhs in
        match lhs', rhs' with
        | Int(a), Int(b) -> Int(a + b)
        | _ ->
           let msg = type_mismatch "add" lhs' rhs' in
           raise(Error(pos, msg)))
    | SubExpr(pos, lhs, rhs) ->
       (let lhs' = interpret_expr env lhs in
        let rhs' = interpret_expr env rhs in
        match lhs', rhs' with
        | Int(a), Int(b) -> Int(a - b)
        | _ ->
           let msg = type_mismatch "sub" lhs' rhs' in
           raise(Error(pos, msg)))
    | EqualExpr(pos, lhs, rhs) ->
       (let lhs' = interpret_expr env lhs in
        let rhs' = interpret_expr env rhs in
        match lhs', rhs' with
        | Int(a), Int(b) -> Bool(a = b)
        | Bool(a), Bool(b) -> Bool(a = b)
        | _ ->
           let msg = type_mismatch "equal" lhs' rhs' in
           raise(Error(pos, msg)))
    | AndExpr(pos, lhs, rhs) ->
       (let lhs' = interpret_expr env lhs in
        let rhs' = interpret_expr env rhs in
        match lhs', rhs' with
        | Bool(a), Bool(b) -> Bool(a && b)
        | _ ->
           let msg = type_mismatch "and" lhs' rhs' in
           raise(Error(pos, msg)))
    | OrExpr(pos, lhs, rhs) ->
       (let lhs' = interpret_expr env lhs in
        let rhs' = interpret_expr env rhs in
        match lhs', rhs' with
        | Bool(a), Bool(b) -> Bool(a || b)
        | _ ->
           let msg = type_mismatch "or" lhs' rhs' in
           raise(Error(pos, msg)))
    | InvertExpr(pos, e) ->
       (let e' = interpret_expr env e in
        match e' with
        | Bool(b) -> Bool(not b)
        | _ ->
           let ty_e = value_to_string e' in
           let msg = Printf.sprintf "type mismatch for invert: got %s" ty_e in
           raise(Error(pos, msg)))
    | LambdaExpr(_, params, captures, body) -> Lambda (eval_lambda env params captures body)
    | CallExpr(pos, callee, args) ->
       let callee' = interpret_expr env callee in
       let args' = Array.map (interpret_expr env) args in
       match callee' with
       | Lambda(f) -> f(env, args')
       | _ ->
          let ty_callee = value_to_string callee' in
          let msg = Printf.sprintf "type mismatch for call: got %s" ty_callee in
          raise(Error(pos, msg))
  in
  if env.debug then begin
      Typed_ast.pp_expr Format.std_formatter expr;
      Format.printf " -> %s@\n" (value_to_string result)
    end;
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
                 ; args = args
                 ; debug = env.debug
                 } in
      interpret_expr env' body
  in
  eval

let rec interpret_stmt env stmt =
  if env.debug then begin
      Format.printf "Step %d@\n" !step;
      Typed_ast.pp_stmt Format.std_formatter stmt;
      Format.printf "@\n@\n";
      incr step
    end;

  match stmt with
  | ReturnStmt(_, e) -> interpret_expr env e
  | ExprStmt(_, e) -> ignore (interpret_expr env e); Unit
  | BindStmt(_, id, e) ->
     let e' = interpret_expr env e in
     (Array.get env.binds id) := e';
     (if env.debug then
        Format.printf "Bound(%d) <- %s@\n@\n" id (value_to_string e')
     );
     Unit
  | IfStmt(pos, cond, then_block, else_block) ->
     match interpret_expr env cond with
     | Bool(true) -> apply_block env then_block
     | Bool(false) -> apply_block env else_block
     | _ -> raise(Error(pos, "condition with not a bool"))
and apply_block env stmts =
  List.fold_left (fun _ stmt -> interpret_stmt env stmt) Unit stmts

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
                   ; binds = Array.init func.num_locals (fun _ -> ref Unit)
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
            }
  in
  let func_expr = Typed_ast.FuncExpr(Lexing.dummy_pos, main.id) in
  let args = Array.of_list [] in
  let call_expr = Typed_ast.CallExpr(Lexing.dummy_pos, func_expr, args) in
  (ignore (interpret_expr env call_expr))
