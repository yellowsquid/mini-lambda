(* OCaml interpretter for language *)

open Typed_ast

type value
  = Unit
  | Bool of bool
  | Int of int
  | Lambda of (env -> value list -> value)
and env =
  { funcs: value array
  ; captures: value array
  ; binds: value ref array
  ; args: value array
  }

let rec interpret_exprs_cnt env exprs cnt = match exprs with
  | [] -> cnt []
  | FuncExpr(_, id) :: rest ->
     let value = Array.get env.funcs id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | EnvExpr(_, id) :: rest ->
     let value = Array.get env.captures id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | BoundExpr(_, id) :: rest ->
     let value = !(Array.get env.binds id) in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | ArgExpr(_, id) :: rest ->
     let value = Array.get env.args id in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | IntExpr(_, i) :: rest ->
     let value = Int i in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | BoolExpr(_, b) :: rest ->
     let value = Bool b in
     interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))
  | AddExpr(_, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [Int a; Int b] -> Int (a + b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  | SubExpr(_, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [Int a; Int b] -> Int (a - b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  | EqualExpr(_, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [Int a; Int b] -> Bool (a = b)
           | [Bool a; Bool b] -> Bool (a = b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  | AndExpr(_, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [Bool a; Bool b] -> Bool (a && b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  | OrExpr(_, lhs, rhs) :: rest ->
     interpret_exprs_cnt env [lhs; rhs] (fun x ->
         let value = match x with
           | [Bool a; Bool b] -> Bool (a || b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
  | InvertExpr(_, e) :: rest ->
     interpret_exprs_cnt env [e] (fun x ->
         let value = match x with
           | [Bool b] -> Bool (not b)
           | _ -> failwith "type mismatch"
         in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest')))
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
  | CallExpr(_, callee, args) :: rest ->
     interpret_exprs_cnt env [callee] (fun x ->
         interpret_exprs_cnt env (Array.to_list args) (fun args' ->
             let value = match x with
               | [Lambda f] -> f env args'
               | _ -> failwith "type mismatch"
             in interpret_exprs_cnt env rest (fun rest' -> cnt (value :: rest'))))

let rec get_value values = match values with
  | [] -> Unit
  | Unit :: rest -> get_value rest
  | x :: _ -> x

let rec interpret_stmts_cnt env stmts cnt = match stmts with
  | [] -> cnt []
  | ReturnStmt(_, e) :: rest ->
     interpret_exprs_cnt env [e] (fun x ->
         let value = match x with
           | [e'] -> e'
           | _ -> failwith "type mismatch"
         in interpret_stmts_cnt env rest (fun rest' -> cnt (value :: rest')))
  | ExprStmt(_, e) :: rest ->
     interpret_exprs_cnt env [e] (fun _ ->
         interpret_stmts_cnt env rest (fun rest' -> cnt (Unit :: rest')))
  | BindStmt(_, id, e) :: rest ->
     interpret_exprs_cnt env [e] (fun x ->
         (match x with
          | [e'] -> (Array.get env.binds id):= e'
          | _ -> failwith "type mismatch");
         interpret_stmts_cnt env rest (fun rest' -> cnt (Unit :: rest')))
  | IfStmt(_, cond, then_block, else_block) :: rest ->
     interpret_exprs_cnt env [cond] (fun x ->
         let block = match x with
           | [Bool true] -> then_block
           | [Bool false] -> else_block
           | _ -> failwith "type mismatch"
         in
         interpret_stmts_cnt env block (fun block' ->
             let value = get_value block' in
             interpret_stmts_cnt env rest (fun rest' -> cnt (value :: rest'))))

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
         interpret_stmts_cnt env' (Option.get func.body) get_value
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
  ignore (interpret_exprs_cnt env [call_expr] get_value)
