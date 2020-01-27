(* OCaml interpretter for language *)

open Typed_ast
open Ops

type value
  = None
  | Unit
  | Bool of bool
  | Int of int
  | Lambda of (env -> value list -> value)
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
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

let rec interpret_exprs_cnt env exprs cnt = match exprs with
  (* No expressions so start applying *)
  | [] -> cnt []
  (* Push the func to stack top before applying *)
  | FuncExpr(_, id) :: rest ->
     let value = Array.get env.funcs id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push the local to stack top before applying *)
  | EnvExpr(_, id) :: rest ->
     let value = Array.get env.captures id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push the local to stack top before applying *)
  | BoundExpr(_, id) :: rest ->
     let value = !(Array.get env.binds id) in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push the arg to stack top before applying *)
  | ArgExpr(_, id) :: rest ->
     let value = Array.get env.args id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push the int to stack top before applying *)
  | IntExpr(_, i) :: rest ->
     let value = Int i in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push the bool to stack top before applying *)
  | BoolExpr(_, b) :: rest ->
     let value = Bool b in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  (* Push first then second arg then do op before applying *)
  | BinExpr(_, op, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [lhs'; rhs'] -> do_bin_op op lhs' rhs'
           | _ -> failwith "runtime error" in
         interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  (* Push first then second arg then invert before applying *)
  | UnaryExpr(_, op, e) :: rest ->
     interpret_exprs_cnt env [e] (fun x ->
         let value = match x with
           | [e'] -> do_unary_op op e'
           | _ -> failwith "runtime error" in
         interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  (* Capture args then construct function before applying *)
  | LambdaExpr(_, params, captures, body) :: rest ->
     interpret_exprs_cnt env (Array.to_list captures) (fun captures' ->
         let eval env' args =
           if List.length args != params then
             failwith "wrong number of args"
           else
             let env'' = { funcs = env'.funcs
                         ; captures = (Array.of_list captures')
                         ; binds = Array.of_list []
                         ; args = Array.of_list args
                         } in
             interpret_exprs_cnt env'' [body] List.hd
         in
         let value = Lambda eval in
         interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  (* Push function then args before calling and then applying *)
  | CallExpr(_, callee, args) :: rest ->
     interpret_exprs_cnt env [callee] (fun x ->
         interpret_exprs_cnt env (Array.to_list args) (fun args' ->
             let value = match x with
               | [Lambda f] -> f env args'
               | _ -> failwith "type mismatch"
             in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))))

let rec interpret_stmts_cnt env stmts cnt = match stmts with
  (* No statements so apply None *)
  | [] -> cnt None
  (* Calculate return value then apply *)
  | ReturnStmt(_, e) :: _ ->
     interpret_exprs_cnt env [e] (fun x ->
         let value = match x with
           | [e'] -> e'
           | _ -> failwith "type mismatch"
         in cnt value)
  (* Eval expression then continue on *)
  | ExprStmt(_, e) :: rest ->
     interpret_exprs_cnt env [e] (fun _ ->
         interpret_stmts_cnt env rest cnt)
  (* Bind expression then continue on *)
  | BindStmt(_, id, e) :: rest ->
     interpret_exprs_cnt env [e] (fun x ->
         (match x with
          | [e'] -> (Array.get env.binds id):= e'
          | _ -> failwith "type mismatch");
         interpret_stmts_cnt env rest cnt)
  (* Evaluate condition then block. If block return then return, else continue on *)
  | IfStmt(_, cond, then_block, else_block) :: rest ->
     interpret_exprs_cnt env [cond] (fun x ->
         let block = match x with
           | [Bool true] -> then_block
           | [Bool false] -> else_block
           | _ -> failwith "type mismatch"
         in
         interpret_stmts_cnt env block (fun block' ->
             match block' with
             | None -> interpret_stmts_cnt env rest cnt
             | x -> x))

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

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Lambda(print_int)
                                           ; "print_bool", Lambda(print_bool)
                                           ; "input_int", Lambda(input_int)])

let identity x = x

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
         interpret_stmts_cnt env' (Option.get func.body) identity
    in Lambda eval

let interpret _debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun (a: Typed_ast.func) b -> compare a.id b.id) program';
  let funcs = Array.map interpret_func program' in
  let main = List.find (fun (f: Typed_ast.func) -> f.name = "main") (Array.to_list program') in
  let env = { funcs = funcs
            ; captures = Array.of_list []
            ; binds = Array.of_list []
            ; args = Array.of_list []
            }
  in
  let func_expr = Typed_ast.FuncExpr(Lexing.dummy_pos, main.id) in
  let args = Array.of_list [] in
  let call_expr = Typed_ast.CallExpr(Lexing.dummy_pos, func_expr, args) in
  interpret_exprs_cnt env [call_expr] (fun x ->
      match x with
      | [None] -> None
      | _ -> failwith "type mismatch")
|> ignore
