(* Compiler Construction - Minimal Lambda Language *)

open Typed_ast

let lower program =
  let next_func_id = ref 0 in
  let new_func_id () = let id = !next_func_id in next_func_id := id + 1; id in

  (* Map function IDs to functions. *)
  let funcs_by_id = Hashtbl.create 10 in
  Array.iter
    (Array.iter
       (fun func -> Hashtbl.add funcs_by_id func.id (func, new_func_id ()))
    )
    program;

  (* Pass through each function, lowering all functions and closures. *)
  let closures = ref [] in

  let lower_func func =
    match func.body with
    | None -> ()
    | Some body ->
       let next_label_id = ref 0 in
       let new_label_id () = let id = !next_label_id in next_label_id := id + 1; id in
       let rec lower_expr acc e =
         match e with
         | FuncExpr(_, id) ->
            (* Decide whether the symbol is a function or a builtin. *)
            let { body; name; _ }, closure_id = Hashtbl.find funcs_by_id id in
            (match body with
             | None ->
                Ir.GetBuiltin name :: acc
             | Some _ ->
                Ir.GetClosure closure_id :: acc
            )
         | EnvExpr(_, id) ->
            Ir.GetEnv id :: acc
         | BoundExpr(_, id) ->
            Ir.GetLocal id :: acc
         | ArgExpr(_, id) ->
            Ir.GetArg id :: acc
         | IntExpr(_, i) ->
            Ir.ConstInt i :: acc
         | BoolExpr(_, b) ->
            (if b then Ir.ConstInt 1 else Ir.ConstInt 0) :: acc
         | AddExpr(_, lhs, rhs) ->
            Ir.Add :: lower_expr (lower_expr acc lhs) rhs
         | SubExpr(_, lhs, rhs) ->
            Ir.Sub :: lower_expr (lower_expr acc lhs) rhs
         | EqualExpr(_, lhs, rhs) ->
            Ir.Equal :: lower_expr (lower_expr acc lhs) rhs
         | AndExpr(_, lhs, rhs) ->
            Ir.And :: lower_expr (lower_expr acc lhs) rhs
         | OrExpr(_, lhs, rhs) ->
            Ir.Or :: lower_expr (lower_expr acc lhs) rhs
         | InvertExpr(_, expr) ->
            Ir.Not :: lower_expr acc expr
         | LambdaExpr(_, num_params, env, body) ->
            (* Create a new closure from the body. *)
            let id = new_func_id() in
            let ir_body = Ir.Return :: lower_expr [] body in
            let num_captures = Array.length env in
            let closure =
              { Ir.id
              ; Ir.name = None
              ; Ir.num_params
              ; Ir.num_captures
              ; Ir.num_locals = 0
              ; Ir.insts =  Array.of_list (List.rev ir_body)
              }
            in
            closures := closure :: !closures;
            (* Instantiate the closure with the captures. *)
            Ir.Closure(id, num_captures) :: Array.fold_left lower_expr acc env
         | CallExpr(_, callee, args) ->
            Ir.Call :: lower_expr (Array.fold_left lower_expr acc args) callee
       in
       (* The return instruction, same across the function. *)
       let rec lower_body acc stmts =
         match stmts with
         | ReturnStmt(_, e) :: rest ->
            lower_body (Ir.Return :: lower_expr acc e) rest
         | ExprStmt(_, e) :: rest ->
            lower_body (Ir.Pop :: lower_expr acc e) rest
         | BindStmt(_, id, e) :: rest ->
            lower_body (Ir.SetLocal id :: lower_expr acc e) rest
         | IfStmt(_, cond, first_block, second_block) :: rest ->
            let _, id = Hashtbl.find funcs_by_id func.id in
            let else_id = new_label_id() in
            let end_id = new_label_id() in
            let second_block' = Ir.Label(id, end_id) :: lower_body [] (List.rev second_block) in
            let first_block' = Ir.Label(id, else_id)
                               :: Ir.Jump(id, end_id)
                               :: lower_body [] (List.rev first_block) in
            let nested = second_block' @ first_block' @ (Ir.If(id, else_id) :: lower_expr acc cond) in
            lower_body nested rest
         | [] ->
            acc
       in
       let _, id = Hashtbl.find funcs_by_id func.id in
       let insts = lower_body [] body in
       let body = Ir.Return :: Ir.ConstInt 0 :: insts in
       let closure =
         { Ir.id
         ; Ir.name = Some(func.name)
         ; Ir.num_params = func.num_params
         ; Ir.num_captures = 0
         ; Ir.num_locals = func.num_locals
         ; Ir.insts = Array.of_list (List.rev body)
         }
       in
       closures := closure :: !closures
  in
  Array.iter (Array.iter lower_func) program;
  Array.of_list (List.rev !closures)
