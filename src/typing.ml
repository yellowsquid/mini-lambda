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
  | TyVar of ty array option * var_ty ref
  (* Forall qualified type - must be substituted *)
  | TyAbs of int * ty array option
  (* Enum type: name and generic parameters *)
  | TyEnum of string * ty array
and var_ty
  = Unbound of int
  | Bound of ty

let rec name_type ty = match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyArr (params, ret) ->
     let params' = String.concat ", " (Array.to_list (Array.map name_type params)) in
     Printf.sprintf "(%s) -> %s" params' (name_type ret)
  | TyVar (_, { contents = Bound ty }) -> Printf.sprintf "*(%s)" (name_type ty)
  | TyVar (None, { contents = Unbound id }) -> Printf.sprintf "'%d" id
  | TyVar (Some options, { contents = Unbound id }) ->
     let options' = String.concat " | " (options |> Array.map name_type |> Array.to_list) in
     Printf.sprintf "'%d(%s)" id options'
  | TyAbs (n, Some options) ->
     let options' = String.concat " | " (options |> Array.map name_type |> Array.to_list) in
     Printf.sprintf "&%d(%s)" n options'
  | TyAbs (n, None) -> Printf.sprintf "&%d" n
  | TyEnum (name, params) ->
     let params' = String.concat ", " (Array.to_list (Array.map name_type params)) in
     Printf.sprintf "%s<%s>" name params'

(* Helper to generate a type variable. *)
let new_unbound_var, new_option_var =
  let ty_idx = ref 0 in
  let eval_unbound () =
    let idx = !ty_idx in
    incr ty_idx;
    TyVar (None, ref (Unbound idx)) in
  let eval_option options =
    let idx = !ty_idx in
    incr ty_idx;
    TyVar (Some options, ref (Unbound idx)) in
  eval_unbound, eval_option

(* Scope to find types in. *)
type lambda_capture = int * Typed_ast.expr * ty
type type_scope
  = GlobalScope of (int * ty) IdentMap.t
  | GroupScope of (int * ty) IdentMap.t
  | FuncScope of (int * ty) IdentMap.t * ty
  | BindScope of (int * ty) IdentMap.t
  | LambdaScope of (int * ty) IdentMap.t * (lambda_capture IdentMap.t) ref
  | LabelScope of int IdentMap.t * int option

(* Generalises a type *)
let rec generalise ty = match ty with
  | TyInt -> ty
  | TyBool -> ty
  | TyUnit -> ty
  | TyArr (params, ret) -> TyArr (Array.map generalise params, generalise ret)
  | TyAbs _ -> failwith "should have been instantiated"
  | TyVar (_, { contents = Bound ty }) -> generalise ty
  | TyVar (options, { contents = Unbound id }) -> TyAbs (id, options)
  | TyEnum (name, params) -> TyEnum (name, Array.map generalise params)

(* Instantiates a type *)
let rec instantiate context ty =
  let loop = instantiate context in
  match ty with
  | TyInt -> ty
  | TyBool -> ty
  | TyUnit -> ty
  | TyArr (params, ret) -> TyArr (Array.map loop params, loop ret)
  | TyAbs (id, Some options) ->
     (match Hashtbl.find_opt context id with
      | Some v -> v
      | None ->
         let ty = new_option_var options in
         Hashtbl.add context id ty;
         ty)
  | TyAbs (id, None) ->
     (match Hashtbl.find_opt context id with
      | Some v -> v
      | None ->
         let ty = new_unbound_var () in
         Hashtbl.add context id ty;
         ty)
  | TyVar _ -> failwith "should have been generalised"
  | TyEnum (name, params) -> TyEnum (name, Array.map loop params)

(* Occurs check *)
let rec occurs loc r ty = match ty with
  | TyInt -> None
  | TyBool -> None
  | TyUnit -> None
  | TyArr(params, ret) ->
     let iter acc ty = if Option.is_some acc then acc else occurs loc r ty in
     Array.fold_left iter (occurs loc r ret) params
  | TyAbs _ -> failwith "should have been instantiated"
  | TyVar (_, { contents = Bound ty }) -> occurs loc r ty
  | TyVar (_, r') when r = r' -> Some (loc, "recursive type")
  | TyVar (None, _) -> None
  | TyVar (Some options, _) ->
     let iter acc ty = if Option.is_some acc then acc else occurs loc r ty in
     Array.fold_left iter None options
  | TyEnum (_, params) ->
     let iter acc ty = if Option.is_some acc then acc else occurs loc r ty in
     Array.fold_left iter None params

(* Implementation of unification *)
let unify loc a b =
  let rec loop loc first a b = match a, b, first with
    | x, y, _ when x = y -> None
    | TyVar (_, { contents = Bound ty }), ty', _ -> loop loc true ty ty'
    | ty, TyVar (_, { contents = Bound ty' }), _ -> loop loc true ty ty'
    | TyVar (None, r), ty, _ ->
       (match occurs loc r ty with
        | Some v -> Some v
        | None -> (r := Bound ty; None))
    | ty, TyVar (None, r), _ ->
       (match occurs loc r ty with
        | Some v -> Some v
        | None -> (r := Bound ty; None))
    | TyVar (Some options, r), ty, _ ->
       let iter (errs, last) option = match last with
         | Some _ -> last :: errs, loop loc true (instantiate (Hashtbl.create 5) option) ty
         | None -> errs, last in
       (match options |> Array.fold_left iter ([], Some (loc, "failed to match union")) |> snd with
        | Some v -> Some v
        | None -> (r := Bound ty; None))
    | ty, TyVar (Some options, r), _ ->
       let iter (errs, last) option = match last with
         | Some _ -> last :: errs, loop loc true (instantiate (Hashtbl.create 5) option) ty
         | None -> errs, last in
       (match options |> Array.fold_left iter ([], Some (loc, "failed to match union")) |> snd with
        | Some v -> Some v
        | None -> (r := Bound ty; None))
    | TyArr(pa, ra), TyArr(pb, rb), _ when Array.length pa = Array.length pb ->
       List.fold_left2 iter (loop loc true ra rb) (Array.to_list pa) (Array.to_list pb)
    | TyEnum ("Int", [||]), TyInt, _ -> None
    | TyEnum ("Bool", [||]), TyBool, _ -> None
    | TyEnum ("Unit", [||]), TyUnit, _ -> None
    | TyEnum (na, pa), TyEnum (nb, pb), _ when na = nb ->
       List.fold_left2 iter None (Array.to_list pa) (Array.to_list pb)
    | x, y, true -> loop loc false y x
    | _, _, false ->
       Some (loc, Printf.sprintf "mismatched types: %s and %s" (name_type a) (name_type b))
  and iter acc a b = match acc with
    | Some v -> Some v
    | None -> loop loc true a b in
  match iter None a b with
  | Some (loc, msg) -> raise (Error (loc, msg))
  | None -> ()

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

let get_bin_type op = match op with
  | Add -> TyInt, TyInt, TyInt
  | Sub -> TyInt, TyInt, TyInt
  | Equal ->
     let ty = new_option_var [|TyInt; TyBool; TyUnit|] in
     ty, ty, TyBool
  | And -> TyBool, TyBool, TyBool
  | Or -> TyBool, TyBool, TyBool

let get_unary_type op = match op with
  | Invert -> TyBool, TyBool

(* Checks the type of an expression *)
let rec check_expr consts scope expr = match expr with
  | IdentExpr(loc, name) ->
     if name.[0] == '_' then
       raise(Error(loc, "attempt to use ignored name " ^ name))
     else
       let rec find_name ss
         = match ss with
         | GlobalScope map :: _ when IdentMap.mem name map ->
            (* Type schemes are instantiated here. *)
            let id, ty = IdentMap.find name map in
            Typed_ast.FuncExpr(loc, id), instantiate (Hashtbl.create 5) ty
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
     let lhs', ty_lhs = check_expr consts scope lhs in
     unify loc ty_lhs ty_one;
     let rhs', ty_rhs = check_expr consts scope rhs in
     unify loc ty_rhs ty_two;
     Typed_ast.BinExpr(loc, op, lhs', rhs'), ty_ret
  | UnaryExpr(loc, op, arg) ->
     let ty_arg, ty_ret = get_unary_type op in
     let arg', ty = check_expr consts scope arg in
     unify loc ty ty_arg;
     Typed_ast.UnaryExpr(loc, op, arg'), ty_ret
  | LambdaExpr(loc, params, body) ->
     let args, ty_args =
       List.fold_left
         (fun (map, ty_args) param ->
           let id = IdentMap.cardinal map in
           let ty_arg = new_unbound_var () in
           IdentMap.add param (id, ty_arg) map, ty_arg :: ty_args
         ) (IdentMap.empty, []) params
     in
     let captures = ref IdentMap.empty in
     let lambda_scope = LambdaScope(args, captures) in
     let body, ty_body = check_expr consts (lambda_scope :: scope) body in
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
     let callee', ty_callee = check_expr consts scope callee in
     let args', arg_tys = args |> List.map (check_expr consts scope) |> List.split in
     let ret_ty = new_unbound_var () in
     let ty_func = TyArr (Array.of_list arg_tys, ret_ty) in
     unify loc ty_func ty_callee;
     Typed_ast.CallExpr (loc, callee', Array.of_list args'), ret_ty
  | ConstructorExpr (loc, name, params) ->
     let ty, variant, param_tys = match IdentMap.find_opt name consts with
       | Some (ty, variant, param_tys) ->
          let context = Hashtbl.create (List.length params) in
          instantiate context ty, variant, List.map (instantiate context) param_tys
       | None -> raise (Error (loc, Printf.sprintf "unknown constructor '%s'" name)) in
     let params', ty_param = params |> List.map (check_expr consts scope) |> List.split in
     List.iter2 (unify loc) param_tys ty_param;
     Typed_ast.ConstructorExpr (loc, variant, Array.of_list params'), ty

let find_in_scope scope name nb = match scope with
  | BindScope(map) :: rest ->
     if IdentMap.mem name map then
       let id, ty = IdentMap.find name map in
       (id, nb, ty, scope)
     else
       let ty = new_unbound_var () in
       let map = IdentMap.add name (nb, ty) map in
       (nb, nb + 1, ty, (BindScope map) :: rest)
  | x ->
     let ty = new_unbound_var () in
     let map = IdentMap.add name (nb, ty) IdentMap.empty in
     (nb, nb + 1, ty, (BindScope map) :: x)

let rec check_pattern_names_unique acc pattern = match pattern with
  | Variable (loc, name) ->
     if List.mem name acc
     then raise (Error (loc, Printf.sprintf "duplicate pattern name '%s'" name))
     else name :: acc
  | Enum (_, _, params) -> List.fold_left check_pattern_names_unique acc params
  | Ignore (loc, name) ->
     if name != "_" && List.mem name acc
     then raise (Error (loc, Printf.sprintf "duplicate pattern name '%s'" name))
     else name :: acc
  | Int _ -> acc
  | Bool _ -> acc

let rec check_pattern consts nb scope pattern = match pattern with
  | Variable (loc, name) ->
     let id, nb', ty, scope' = find_in_scope scope name nb in
     ty, Typed_ast.Variable (loc, id), nb', scope'
  | Enum (loc, name, params) ->
     let ty, variant, param_tys = match IdentMap.find_opt name consts with
     | None -> raise (Error (loc, Printf.sprintf "bad constructor name '%s'" name))
     | Some (ty, variant, param_tys) ->
        let context = Hashtbl.create (List.length params) in
        instantiate context ty, variant, List.map (instantiate context) param_tys in
     let params', nb', scope' =
       List.fold_left2 (fun (params, nb, scope) param param_ty ->
           let ty, param', nb', scope' = check_pattern consts nb scope param in
           let loc = match param with
             | Variable (loc, _) -> loc
             | Enum (loc, _, _) -> loc
             | Ignore (loc, _) -> loc
             | Int (loc, _) -> loc
             | Bool (loc, _) -> loc in
           unify loc ty param_ty;
           param' :: params, nb', scope')
         ([], nb, scope) (List.rev params) (List.rev param_tys) in
     ty, Typed_ast.Enum (loc, variant, params'), nb', scope'
  | Ignore (loc, _) -> new_unbound_var (), Typed_ast.Ignore loc, nb, scope
  | Int (loc, i) -> TyInt, Typed_ast.Int (loc, i), nb, scope
  | Bool (loc, i) -> TyBool, Typed_ast.Bool (loc, i), nb, scope

(* Checks the type of a statement. *)
let rec check_statements consts ret_ty acc scope stats
  = let rec iter (nb, acc) scope stats = match stats with
      | ReturnStmt(loc, e) :: rest ->
         let e', ty = check_expr consts scope e in
         unify loc ty ret_ty;
         let node = Typed_ast.ReturnStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | ExprStmt(loc, e) :: rest ->
         (* It is a funky design choice to unify everything with unit. *)
         let e', ty = check_expr consts scope e in
         unify loc ty TyUnit;
         let node = Typed_ast.ExprStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | BindStmt(loc, name, e) :: rest ->
         let e', ty = check_expr consts scope e in
         let (nb, next_nb, bind_ty, scope') = find_in_scope scope name nb in
         unify loc ty bind_ty;
         let node = Typed_ast.BindStmt(loc, nb, e') in
         iter (next_nb, node :: acc) scope' rest
      | IgnoreStmt(loc, e) :: rest ->
         (* Extend funky design by having names starting _ pretend to bind. *)
         let e', _ = check_expr consts scope e in
         (* TODO: suggest e; if type is unit *)
         let node = Typed_ast.ExprStmt(loc, e') in
         iter (nb, node :: acc) scope rest
      | MatchStmt(loc, e, cases) :: rest ->
         let e', ty = check_expr consts scope e in
         (* Check case does unification *)
         let nb', cases' =
           List.fold_left (fun (max_nb, acc) (loc, pattern, stmts) ->
               ignore (check_pattern_names_unique [] pattern);
               let pattern_ty, pattern', nb', scope' = check_pattern consts nb scope pattern in
               unify loc pattern_ty ty;
               let nb'', stmts' = check_statements consts ret_ty (nb', []) scope' stmts in
               let node = (loc, pattern', stmts') in
               max max_nb nb'', node :: acc) (nb, []) (List.rev cases) in
         let node = Typed_ast.MatchStmt (loc, e', cases') in
         iter (nb', node :: acc) scope rest
      | IfStmt(loc, cond, tblock, fblock) :: rest ->
         let cond', cond_ty = check_expr consts scope cond in
         unify loc cond_ty TyBool;
         let ret_ty = new_unbound_var () in
         let nb', true_acc' = check_statements consts ret_ty (nb, []) scope tblock in
         let nb'', false_acc' = check_statements consts ret_ty (nb, []) scope fblock in
         let node = Typed_ast.IfStmt(loc, cond', List.rev true_acc', List.rev false_acc') in
         iter (max nb' nb'', node :: acc) scope rest
      | WhileStmt(loc, cond, lblock, eblock, label) :: rest ->
         let cond', cond_ty = check_expr consts scope cond in
         unify loc cond_ty TyBool;
         let ret_ty  = new_unbound_var () in
         let id, scope' = match add_label (Option.value label ~default:"_") scope with
           | None -> raise (Error (loc, "not in function."))
           | Some (x, y) -> x, y in
         let nb', lacc = check_statements consts ret_ty (nb, []) scope' lblock in
         let nb'', eacc = check_statements consts ret_ty (nb, []) scope eblock in
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

let check_func consts externs funcs group_scope id =
  let func = funcs.(id) in
  match func.rest with
  | Extern _ ->
     let _, (params, ret) = IdentMap.find func.func_name externs in
     let new_func =
       { Typed_ast.id
       ; name = func.func_name
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
           IdentMap.add name (IdentMap.cardinal map, new_unbound_var ()) map)
         IdentMap.empty params in
     let ret = new_unbound_var () in
     let label_scope = LabelScope (IdentMap.empty, None) in
     let scope = label_scope :: FuncScope (args, ret) :: group_scope in

     (* Recursively check the function. *)
     let nb, body = check_statements consts ret (0, []) scope body in
     let new_body, num_locals = List.rev body, nb in
     let new_func =
       { Typed_ast.id
       ; name = func.func_name
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

let rec get_type types generics { loc; base; params } =
  if IdentMap.mem base generics
  then IdentMap.find base generics
  else match IdentMap.find_opt base types with
       | Some n when n = (List.length params) ->
          TyEnum (base, params |> List.map (get_type types generics) |> Array.of_list)
       | Some _ ->
          raise (Error (loc, Printf.sprintf "wrong number of generic parameters"))
       | None -> raise (Error (loc, Printf.sprintf "unknown type '%s'" base))

let check_extern types func = match func.rest with
  | Definition _ -> failwith "not an extern"
  | Extern (params, return) ->
     let params' = params |> List.map (get_type types IdentMap.empty) |> Array.of_list in
     params', get_type types IdentMap.empty return

let group_map funcs base group =
  Array.fold_left (fun scope id ->
      if id >= 0 then
        let { func_name; _ } = funcs.(id) in
        IdentMap.add func_name (id, new_unbound_var ()) scope
      else scope) base group

let check_group check funcs (acc, scope) group =
  let group_map = group_map funcs IdentMap.empty group in
  let group_scope = [GroupScope group_map; GlobalScope scope] in
  (* Type check individual methods *)
  let func_types = Array.map (check funcs group_scope) group in

  (* Unify the types with their tvars. *)
  let typed_funcs =
    Array.mapi (fun i id ->
        let ty, func = func_types.(i) in
        let _, fn_ty = IdentMap.find (funcs.(id).func_name) group_map in
        unify func.Typed_ast.loc fn_ty ty; func)
      group in

  (* Generalise the types. *)
  let new_root_scope =
    Array.fold_left (fun map id ->
        let { func_name; _ } = funcs.(id) in
        let _, fn_ty = IdentMap.find func_name group_map in
        let func_ty = generalise fn_ty in
        IdentMap.add func_name (id, func_ty) map)
      scope group in
  (typed_funcs :: acc, new_root_scope)

(* Type check a constructor*)
let check_const types generics const =
  const.const_name, List.map (fun ty -> ty |> get_type types generics |> generalise) const.params

(* Type check a group (not necessary?) *)
let check_ty_decl types acc ty_decl =
  let ty_generics = ty_decl.generics in
  let generics =
    ty_generics |> List.map (fun name -> name, new_unbound_var ()) |> List.to_seq |> IdentMap.of_seq in
  let ty =
    TyEnum (ty_decl.ty_name, ty_generics
                        |> List.map (fun name -> IdentMap.find name generics)
                        |> Array.of_list) in
  let consts = List.map (check_const types generics) ty_decl.consts in
  let add_const ty (i, acc) (name, params) =
    i + 1, IdentMap.add name (generalise ty, i, params) acc in
  consts |> List.fold_left (add_const ty) (0, acc) |> snd

(* Finds the free variables in an expression. *)
let rec find_refs_expr bound acc expr = match expr with
  | IdentExpr (loc, name) ->
     if List.mem name bound then acc else (loc, name) :: acc
  | IntExpr _ -> acc
  | BoolExpr _ -> acc
  | BinExpr (_, _, lhs, rhs) -> find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | UnaryExpr (_, _, rhs) -> find_refs_expr bound acc rhs
  | LambdaExpr (_, params, body) -> find_refs_expr (List.append params bound) acc body
  | CallExpr (_, callee, args) ->
     List.fold_left (find_refs_expr bound) (find_refs_expr bound acc callee) args
  | ConstructorExpr (_, _, args) -> List.fold_left (find_refs_expr bound) [] args

let rec bind_pattern acc pattern = match pattern with
  | Variable (_, name) -> name :: acc
  | Enum (_, _, patterns) -> List.fold_left bind_pattern acc patterns
  | Ignore _ -> acc
  | Int _ -> acc
  | Bool _ -> acc

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
          | MatchStmt (_, expr, cases) ->
             bound, List.fold_left (find_refs_case bound) (find_refs_expr bound acc expr) cases
          | IfStmt (_, cond, tblock, fblock) ->
             (bound, (find_refs_expr bound acc cond)
                     @ (find_refs_stat bound tblock)
                     @ (find_refs_stat bound fblock))
          | WhileStmt (_, cond, lblock, eblock, _) ->
             (bound, (find_refs_expr bound acc cond)
                     @ (find_refs_stat bound lblock)
                     @ (find_refs_stat bound eblock))
          | ContinueStmt _ -> (bound, acc)
          | BreakStmt _ -> (bound, acc)) (bound, []) stats in
    acc
and find_refs_case bound acc (_, pattern, stmts) =
  find_refs_stat (bind_pattern bound pattern) stmts @ acc

let get_references funcs externs i func =
  if Hashtbl.mem funcs func.func_name || Hashtbl.mem externs func.func_name then
    raise (Error (func.loc, "duplicate name"))
  else match func.rest with
       | Definition (params, body) ->
          Hashtbl.add funcs func.func_name i;
          find_refs_stat params body
       | Extern _ ->
          Hashtbl.add externs func.func_name i;
          []

let graph_funcs funcs externs =
  let get_id (loc, name) =
    if Hashtbl.mem funcs name
    then Hashtbl.find funcs name
    else if Hashtbl.mem externs name
    then Hashtbl.find externs name
    else raise (Error (loc, "undefined function"))
  in Array.map (List.map get_id)

let is_definition func = match func.rest with
  | Extern _ -> false
  | Definition _ -> true

let base_types = [ "Int", 0; "Bool", 0; "Unit", 0 ] |> List.to_seq |> IdentMap.of_seq

let check { funcs; tys } =
  let new_types =
    tys
    |> Array.map (fun { ty_name; generics; _ } -> ty_name, List.length generics)
    |> Array.to_seq in
  let types = IdentMap.add_seq new_types base_types in
  let constructors = Array.fold_left (check_ty_decl types) IdentMap.empty tys in

  (* For each toplevel definition, collect the list of references. *)
  let num_funcs = funcs |> Array.to_list |> (List.filter is_definition) |> List.length in
  let name_table = Hashtbl.create num_funcs in
  let extern_table = Hashtbl.create (Array.length funcs - num_funcs) in

  let references = Array.mapi (get_references name_table extern_table) funcs in

  (* Build a directed graph of function-to-function references. *)
  let func_sccs = references |> graph_funcs name_table extern_table |> Graph.sccs in

  (* Type check all externs first *)
  let externs =
    extern_table
    |> Hashtbl.to_seq
    |> IdentMap.of_seq
    |> IdentMap.map (fun id -> id, check_extern types funcs.(id)) in

  (* Typecheck each method group. Types are polymorphic only outside of SCCs. *)
  let root_scope = IdentMap.map (fun (id, (params, ret)) -> id, TyArr(params, ret)) externs in
  let check_fun = (check_func constructors externs) in
  let typed_prog, _ = Array.fold_left (check_group check_fun funcs) ([], root_scope) func_sccs in
  Array.of_list (List.rev typed_prog)
