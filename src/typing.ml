(* Compiler Construction - Minimal Lambda Language
 *
 * The type checker throws an exception if it encounter
 * a type error. This file defines all the types supported
 * by the language, along with an implementation of the
 * Hindley-Milner algorithm which infers the most general
 * polymorphic type for each function in the program.
 *
 * Hindley-Milner is based on unification: if the type of
 * an expression is known to be of a certain kind, or if
 * the types of expressions are known to be identical, they
 * are unified using the method defined below.
 *)

open Ast
open Ops

exception Error of Ast.loc * string

module IdentMap = Map.Make(String)

(* Enumeration of types *)
type ty
  (* Basic integer type *)
  = TyInt
  (* Basic boolean type *)
  | TyBool
  (* Unit type for functions with no return *)
  | TyUnit
  (* Function type *)
  | TyArr of ty array * ty
  (* Type variable *)
  | TyVar of var_ty ref
  (* Forall qualified type - must be substituted *)
  | TyAbs of int
and var_ty
  = Unbound of int
  | Bound of ty

(* Todo: link types together. *)
let rec name_type ty = match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyArr(params, ret) ->
     let args = "(" ^ (String.concat ", " (Array.to_list (Array.map name_type params))) ^ ")" in
     let ret = name_type ret in
     args ^ " -> " ^ ret
  | TyVar(ty) -> (match !ty with
                  | Unbound(n) -> "'" ^ string_of_int n
                  | Bound(ty) -> name_type ty)
  | TyAbs(n) -> "''" ^ string_of_int n

(* Scope to find types in. *)
type lambda_capture = int * Typed_ast.expr * ty
type type_scope
  = GlobalScope of (int * ty) IdentMap.t
  | GroupScope of (int * ty) IdentMap.t
  | FuncScope of (int * ty) IdentMap.t * ty
  | BindScope of (int * ty) IdentMap.t
  | LambdaScope of (int * ty) IdentMap.t * (lambda_capture IdentMap.t) ref
  | LabelScope of int IdentMap.t * int option

(* Occurs check *)
let rec occurs loc r ty = match ty with
  | TyInt -> ()
  | TyBool -> ()
  | TyUnit -> ()
  | TyArr(params, ret) ->
     occurs loc r ret;
     Array.iter (occurs loc r) params
  | TyAbs _ ->
     failwith "should have been instantiated"
  | TyVar ({ contents = Bound ty }) ->
     occurs loc r ty
  | TyVar r' ->
     if r = r' then raise(Error(loc, "recursive type"))

(* Implementation of unification *)
let rec unify loc a b
  = if a = b then ()
    else
      match a, b with
      | TyVar ({ contents = Bound ty }), ty'
        | ty, TyVar ({ contents = Bound ty' }) ->
         unify loc ty ty'
      | TyVar ({ contents = Unbound _ } as r), ty
        | ty, TyVar ({ contents = Unbound _ } as r) ->
         occurs loc r ty;
         r := Bound ty
      | TyArr(pa, ra), TyArr(pb, rb) ->
         let len_a = Array.length pa in
         let len_b = Array.length pb in
         if len_a != len_b then
           raise(Error(loc, "mismatched function types: " ^ name_type a ^ " and " ^ name_type b));
         for i = 0 to len_a - 1 do
           unify loc pa.(i) pb.(i);
         done;
         unify loc ra rb
      | _, _ ->
         raise(Error(loc, "mismatched types: " ^ name_type a ^ " and " ^ name_type b))

(* Helper to generate a type variable. *)
let ty_idx = ref 0
let new_ty_var () =
  let idx = !ty_idx in
  ty_idx := idx + 1;
  TyVar (ref (Unbound idx))

(* Helper to generate while labels. *)
let add_label, get_label =
  let label_idx = ref 0 in
  let rec eval_add label scope = match scope with
    | [] -> None
    | LabelScope (map, _) :: rest ->
       let new_id = !label_idx in
       incr label_idx;
       Some (new_id, LabelScope (IdentMap.add label new_id map, Some new_id) :: rest)
    | v :: rest ->
       Option.map (fun (id, rest') -> id, v :: rest') (eval_add label rest) in
  let rec do_get label scope = match scope with
    | [] -> None
    | LabelScope (map, _) :: _ -> IdentMap.find_opt label map
    | _ :: rest -> do_get label rest in
  let rec do_current scope = match scope with
    | [] -> None
    | LabelScope (_, last) :: _ -> last
    | _ :: rest -> do_current rest in
  let eval_get label scope = match label with
    | Some l -> do_get l scope
    | None -> do_current scope in
  eval_add, eval_get

(* Generalises a type *)
let rec generalise ty
  = match ty with
  | TyInt -> ty
  | TyBool -> ty
  | TyUnit -> ty
  | TyArr(params, ret) -> TyArr(Array.map generalise params, generalise ret)
  | TyAbs _ -> failwith "should have been instantiated"
  | TyVar ({ contents = Bound ty }) -> generalise ty
  | TyVar ({ contents = Unbound id }) -> TyAbs id

(* Instantiates a type *)
let instantiate ty =
  let abs_context = Hashtbl.create 5 in
  let rec loop ty = match ty with
    | TyInt -> ty
    | TyBool -> ty
    | TyUnit -> ty
    | TyArr(params, ret) -> TyArr(Array.map loop params, loop ret)
    | TyAbs id ->
       begin
         try Hashtbl.find abs_context id
         with Not_found ->
           let ty = new_ty_var () in
           Hashtbl.add abs_context id ty;
           ty
       end
    | TyVar _ -> failwith "should have been generalised"
  in loop ty

let get_bin_type op = match op with
  | Add -> TyInt, TyInt, TyInt
  | Sub -> TyInt, TyInt, TyInt
  | Equal -> let ty = new_ty_var () in ty, ty, TyBool
  | And -> TyBool, TyBool, TyBool
  | Or -> TyBool, TyBool, TyBool

let get_unary_type op = match op with
  | Invert -> TyBool, TyBool

(* Checks the type of an expression *)
let rec check_expr scope expr = match expr with
  | IdentExpr(loc, name) ->
     if name.[0] == '_' then
       raise(Error(loc, "attempt to use ignored name " ^ name))
     else
       let rec find_name ss
         = match ss with
         | GlobalScope map :: _ when IdentMap.mem name map ->
            (* Type schemes are instantiated here. *)
            let id, ty = IdentMap.find name map in
            Typed_ast.FuncExpr(loc, id), instantiate ty
         | GroupScope map :: _ when IdentMap.mem name map ->
            (* Polymorphic recursion is not allowed, no generalisation here. *)
            let id, ty = IdentMap.find name map in
            Typed_ast.FuncExpr(loc, id), ty
         | FuncScope(map, _) :: _ when IdentMap.mem name map ->
            let id, ty = IdentMap.find name map in
            Typed_ast.ArgExpr(loc, id), ty
         | BindScope map :: _ when IdentMap.mem name map ->
            let id, ty = IdentMap.find name map in
            Typed_ast.BoundExpr(loc, id), ty
         | LambdaScope(map, captures) :: rest ->
            (* In a lambda scope, see what needs to be captured. Arguments are *)
            (* handled as expected, while captures are cached. If a captured name *)
            (* is to be foud, the outside scope is searched, but an env reference *)
            (* is returned in its place, unless the name is a global. *)
            if IdentMap.mem name map then
              let id, ty = IdentMap.find name map in
              Typed_ast.ArgExpr(loc, id), ty
            else if IdentMap.mem name !captures then
              let id, _, ty = IdentMap.find name !captures in
              Typed_ast.EnvExpr(loc, id), ty
            else begin
                let expr, ty = find_name rest in
                match expr with
                | Typed_ast.FuncExpr(_, _) -> expr, ty
                | _ ->
                   let id = IdentMap.cardinal !captures in
                   captures := IdentMap.add name (id, expr, ty) !captures;
                   Typed_ast.EnvExpr(loc, id), ty
              end
         | _ :: rest ->
            find_name rest
         | [] ->
            raise(Error(loc, "unbound variable " ^ name))
       in find_name scope
  | IntExpr(loc, i) ->
     Typed_ast.IntExpr(loc, i), TyInt
  | BoolExpr(loc, b) ->
     Typed_ast.BoolExpr(loc, b), TyBool
  | BinExpr(loc, op, lhs, rhs) ->
     let ty_one, ty_two, ty_ret = get_bin_type op in
     let lhs', ty_lhs = check_expr scope lhs in
     unify loc ty_lhs ty_one;
     let rhs', ty_rhs = check_expr scope rhs in
     unify loc ty_rhs ty_two;
     Typed_ast.BinExpr(loc, op, lhs', rhs'), ty_ret
  | UnaryExpr(loc, op, arg) ->
     let ty_arg, ty_ret = get_unary_type op in
     let arg', ty = check_expr scope arg in
     unify loc ty ty_arg;
     Typed_ast.UnaryExpr(loc, op, arg'), ty_ret
  | LambdaExpr(loc, params, body) ->
     let args, ty_args =
       List.fold_left
         (fun (map, ty_args) param ->
           let id = IdentMap.cardinal map in
           let ty_arg = new_ty_var () in
           IdentMap.add param (id, ty_arg) map, ty_arg :: ty_args
         ) (IdentMap.empty, []) params
     in
     let captures = ref IdentMap.empty in
     let lambda_scope = LambdaScope(args, captures) in
     let body, ty_body = check_expr (lambda_scope :: scope) body in
     let lambda_ty = TyArr(Array.of_list (List.rev ty_args), ty_body) in
     let capture_list =
       Array.init (IdentMap.cardinal !captures)
         (fun i ->
           let _, (_, capture, _) =
             List.find (fun (_, (id, _, _)) -> id == i) (IdentMap.bindings !captures)
           in
           capture
         )
     in
     Typed_ast.LambdaExpr(loc, List.length params, capture_list, body), lambda_ty
  | CallExpr(loc, callee, args) ->
     (* When checking the type of a call, a dummy type is created: *)
     (* (ty_arg0, ty_arg1, ...) -> ty_return *)
     (* The type is then unified with the calle's type - during unification *)
     (* ty_return is unified with the function's return type, yielding the *)
     (* type of the call expression. *)
     let callee', ty_callee = check_expr scope callee in
     let args' = List.map (check_expr scope) args in
     let arg_tys = List.map snd args' in
     let ret_ty = new_ty_var () in
     let ty_func = TyArr(Array.of_list arg_tys, ret_ty) in
     unify loc ty_func ty_callee;
     Typed_ast.CallExpr(loc, callee', Array.of_list (List.map fst args')), ret_ty

let find_in_scope(scope, name, nb) = match scope with
  | BindScope(map) :: rest ->
     if IdentMap.mem name map then
       let id, ty = IdentMap.find name map in
       (id, nb, ty, scope)
     else
       let ty = new_ty_var () in
       let map = IdentMap.add name (nb, ty) map in
       (nb, nb + 1, ty, (BindScope map) :: rest)
  | x ->
     let ty = new_ty_var () in
     let map = IdentMap.add name (nb, ty) IdentMap.empty in
     (nb, nb + 1, ty, (BindScope map) :: x)

(* Checks the type of a statement. *)
let rec check_statements ret_ty acc scope stats
  = let rec iter (nb, acc) scope stats = match stats with
      | ReturnStmt(loc, e) :: rest ->
         let e', ty = check_expr scope e in
         unify loc ty ret_ty;
         let node = Typed_ast.ReturnStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | ExprStmt(loc, e) :: rest ->
         (* It is a funky design choice to unify everything with unit. *)
         let e', ty = check_expr scope e in
         unify loc ty TyUnit;
         let node = Typed_ast.ExprStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | BindStmt(loc, name, e) :: rest ->
         let e', ty = check_expr scope e in
         let (nb, next_nb, bind_ty, scope') = find_in_scope(scope, name, nb) in
         unify loc ty bind_ty;
         let node = Typed_ast.BindStmt(loc, nb, e') in
         iter (next_nb, node :: acc) scope' rest
      | IgnoreStmt(loc, e) :: rest ->
         (* Extend funky design by having names starting _ pretend to bind. *)
         let e', _ = check_expr scope e in
         (* TODO: suggest e; if type is unit *)
         let node = Typed_ast.ExprStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | IfStmt(loc, cond, tblock, fblock) :: rest ->
         let cond', cond_ty = check_expr scope cond in
         unify loc cond_ty TyBool;
         let ret_ty = new_ty_var () in
         let nb', true_acc' = check_statements ret_ty (nb, []) scope tblock in
         let nb'', false_acc' = check_statements ret_ty (nb, []) scope fblock in
         let node = Typed_ast.IfStmt(loc, cond', List.rev true_acc', List.rev false_acc') in
         iter (max nb' nb'', node :: acc) scope rest
      | WhileStmt(loc, cond, lblock, eblock, label) :: rest ->
         let cond', cond_ty = check_expr scope cond in
         unify loc cond_ty TyBool;
         let ret_ty  = new_ty_var () in
         let id, scope' = match add_label (Option.value label ~default:"_") scope with
           | None -> raise (Error (loc, "not in function."))
           | Some (x, y) -> x, y in
         let nb', lacc = check_statements ret_ty (nb, []) scope' lblock in
         let nb'', eacc = check_statements ret_ty (nb, []) scope eblock in
         let node = Typed_ast.WhileStmt (loc, id, cond', List.rev lacc, List.rev eacc) in
         iter (max nb' nb'', node :: acc) scope rest
      | ContinueStmt(loc, label) :: rest ->
         let id = match get_label label scope with
           | None -> raise (Error (loc, "failed to find loop."))
           | Some x -> x in
         let node = Typed_ast.ContinueStmt (loc, id) in
         iter (nb, node :: acc) scope rest
      | BreakStmt(loc, label) :: rest ->
         let id = match get_label label scope with
           | None -> raise (Error (loc, "failed to find loop."))
           | Some x -> x in
         let node = Typed_ast.BreakStmt (loc, id) in
         iter (nb, node :: acc) scope rest
      | [] ->
         (nb, acc)
    in iter acc scope stats

let check_func prog externs group_scope id =
  let func = prog.(id) in
  match func.rest with
  | Extern _ ->
     let _, (params, ret) = IdentMap.find func.name externs in
     let new_func =
       { Typed_ast.id
       ; name = func.name
       ; num_params = Array.length params
       ; num_locals = 0
       ; body = None
       ; loc = func.loc
       } in
     TyArr (params, ret), new_func
  | Definition (params, body) ->
     (* Set up argument / return types. *)
     let args =
       List.fold_left (fun map name ->
           IdentMap.add name (IdentMap.cardinal map, new_ty_var ()) map)
         IdentMap.empty params in
     let ret = new_ty_var () in
     let label_scope = LabelScope (IdentMap.empty, None) in
     let scope = label_scope :: FuncScope (args, ret) :: group_scope in

     (* Recursively check the function. *)
     let nb, body = check_statements ret (0, []) scope body in
     let new_body, num_locals = List.rev body, nb in
     let new_func =
       { Typed_ast.id
       ; name = func.name
       ; num_params = IdentMap.cardinal args
       ; num_locals
       ; body = Some (new_body)
       ; loc = func.loc
       } in

     let arg_types =
       Array.init (List.length params)
         (fun i -> snd (IdentMap.find (List.nth params i) args)) in

     (* Construct a function type. *)
     TyArr (arg_types, ret), new_func

let check_extern types func = match func.rest with
  | Definition _ -> failwith "not an extern"
  | Extern (params, return) ->
     let get_type name = match IdentMap.find_opt name types with
       | Some ty -> ty
       | None -> raise (Error (func.loc, Printf.sprintf "unknown type '%s'" name)) in
     params |> List.map get_type |> Array.of_list, get_type return

(* Finds the free variables in an expression. *)
let rec find_refs_expr bound acc expr = match expr with
  | IdentExpr(loc, name) ->
     if List.mem name bound then acc else (loc, name) :: acc
  | IntExpr(_, _) ->
     acc
  | BoolExpr(_, _) ->
     acc
  | BinExpr(_, _, lhs, rhs) ->
     find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | UnaryExpr(_, _, rhs) -> find_refs_expr bound acc rhs
  | LambdaExpr(_, params, body) ->
     find_refs_expr (List.append params bound) acc body
  | CallExpr(_, callee, args) ->
     List.fold_left (find_refs_expr bound) (find_refs_expr bound acc callee) args

(* Finds the free variables in a function body. *)
let rec find_refs_stat bound stats
  = let _, acc =
      List.fold_left
        (fun (bound, acc) stat ->
          match stat with
          | ReturnStmt (_, e) ->
             (bound, find_refs_expr bound acc e)
          | ExprStmt (_, e) ->
             (bound, find_refs_expr bound acc e)
          | BindStmt (_, name, e) ->
             (* The expression can refer to previous instances of 'name'. *)
             (name :: bound, find_refs_expr bound acc e)
          | IgnoreStmt (_, e) ->
             (bound, find_refs_expr bound acc e)
          | IfStmt (_, cond, tblock, fblock) ->
             (bound, (find_refs_expr bound acc cond)
                     @ (find_refs_stat bound tblock)
                     @ (find_refs_stat bound fblock))
          | WhileStmt (_, cond, lblock, eblock, _) ->
             (bound, (find_refs_expr bound acc cond)
                     @ (find_refs_stat bound lblock)
                     @ (find_refs_stat bound eblock))
          | ContinueStmt _ -> (bound, acc)
          | BreakStmt _ -> (bound, acc)
        ) (bound, []) stats
    in acc

(* Helper structure for the very hacky and imperative SCC implementation. *)
type dfs_info =
  { index: int
  ; mutable link: int
  ; mutable on_stack: bool
  }

let get_references funcs externs i func =
  if Hashtbl.mem funcs func.name || Hashtbl.mem externs func.name then
    raise (Error (func.loc, "duplicate name"))
  else match func.rest with
       | Definition (params, body) ->
          Hashtbl.add funcs func.name i;
          find_refs_stat params body
       | Extern _ ->
          Hashtbl.add externs func.name i;
          []

let graph funcs externs =
  let get_id (loc, name) =
    if Hashtbl.mem funcs name
    then Hashtbl.find funcs name
    else if Hashtbl.mem externs name
    then Hashtbl.find externs name
    else raise (Error (loc, "undefined function"))
  in Array.map (List.map get_id)

(* Using the directed graph of references, find the strongly connected components.
 * Inside a component, i.e a recursive or mutually recursive context, the type is not polymorphic.
 * This is due to the fact that the problem of inferring polymorphically recursive types is
 * undecidable *)
let build_sccs graph =
  let count = Array.length graph in
  let scc_info = Array.init count (fun _ ->
                     { index = -1
                     ; link = -1
                     ; on_stack = false
                   }) in
  let dfs_stack = ref [] in
  let index = ref 0 in
  let sccs_rev = ref [] in

  let rec scc_dfs node_from =
    let idx = !index in
    incr index;
    scc_info.(node_from) <- { index = idx; link = idx; on_stack = true };
    dfs_stack := node_from :: !dfs_stack;

    let update_link node_to =
      let link = min scc_info.(node_from).link scc_info.(node_to).link in
      scc_info.(node_from).link <- link in

    graph.(node_from) |>
      List.iter (fun node_to ->
          let { index = to_index; on_stack = to_on_stack; _ } = scc_info.(node_to) in
          if to_index < 0 then begin
              scc_dfs node_to;
              update_link node_to
            end else if to_on_stack then begin
              update_link node_to
            end
        );

    if scc_info.(node_from).index = scc_info.(node_from).link then begin
        let rec build_component acc stack = match stack with
          | node :: rest when node != node_from ->
             scc_info.(node).on_stack <- false;
             build_component (node :: acc) rest
          | node :: rest ->
             scc_info.(node).on_stack <- false;
             (node :: acc, rest)
          | [] ->
             (acc, []) in
        let (scc, stack) = build_component [] !dfs_stack in
        dfs_stack := stack;
        sccs_rev := Array.of_list scc :: !sccs_rev;
      end in

  for i = 0 to count - 1 do
    if scc_info.(i).index < 0 then scc_dfs i;
  done;
  Array.of_list (List.rev !sccs_rev)

let group_map prog base group =
  Array.fold_left (fun scope id ->
      if id >= 0 then
        let { name; _ } = prog.(id) in
        IdentMap.add name (id, new_ty_var ()) scope
      else scope) base group

let is_definition func = match func.rest with
  | Extern _ -> false
  | Definition _ -> true

let check prog =
  (* For each toplevel definition, collect the list of references. *)
  let num_funcs = prog |> Array.to_list |> (List.filter is_definition) |> List.length in
  let name_table = Hashtbl.create num_funcs in
  let extern_table = Hashtbl.create (Array.length prog - num_funcs) in

  let references = Array.mapi (get_references name_table extern_table) prog in

  (* Build a directed graph of function-to-function references. *)
  let graph = references |> graph name_table extern_table in
  let sccs = build_sccs graph in

  (* Type check all externs first *)
  let types =
    [("Int", TyInt); ("Bool", TyBool); ("Unit", TyUnit)] |> List.to_seq |> IdentMap.of_seq in
  let externs =
    extern_table
    |> Hashtbl.to_seq
    |> Seq.map (fun (name, id) -> name, (id, check_extern types prog.(id)))
    |> IdentMap.of_seq in

  (* Typecheck each method group. Types are polymorphic only outside of SCCs. *)
  let typed_prog, _ =
    Array.fold_left (fun (typed_prog, root_scope) group ->
        let group_map = group_map prog IdentMap.empty group in
        let group_scope = [GroupScope group_map; GlobalScope root_scope] in

        (* Typecheck individual methods *)
        let types = Array.map (check_func prog externs group_scope) group in

        (* Unify the types with their tvars. *)
        let funcs =
          Array.mapi (fun i id ->
              let ty, func = types.(i) in
              let _, fn_ty = IdentMap.find (prog.(id).name) group_map in
              unify func.Typed_ast.loc fn_ty ty; func)
            group in

        (* Generalise the types. *)
        let new_root_scope =
          Array.fold_left (fun map id ->
              let { name; _ } = prog.(id) in
              let _, fn_ty = IdentMap.find name group_map in
              let func_ty = generalise fn_ty in
              IdentMap.add name (id, func_ty) map)
            root_scope group in
        (funcs :: typed_prog, new_root_scope))
      ([], IdentMap.map (fun (id, (params, ret)) -> id, TyArr(params, ret)) externs) sccs in
  Array.of_list (List.rev typed_prog)
