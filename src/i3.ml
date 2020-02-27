(* Three-stack interpretter.
 *
 * A stack for directives, values and environments. *)

open Typed_ast
open Ops

type directive
  = Expr of expr
  | Stmt of statement
  | BinOp of bin_op
  | UnaryOp of unary_op
  (* Params * Locals * Body *)
  | Capture of int * int * directive list
  | Builtin of (env -> value)
  | Call of int
  | PopEnv
  (* Variant * Params *)
  | Constructor of int * int
  | Pop
  | Bind of int
  | Match of id
  (* Match id * Pattern * Block *)
  | Case of id * directive list * directive list
  | PatternVar of int
  | PatternEnum of id
  | SkipCase
  | SkipMatch of id
  | EndMatch of id
  | If of directive list * directive list
  (* Id * Condition * Loop Block * Else Block *)
  | While of id * directive list * directive list * directive list
  | Continue of id
  | Break of id
  | Seq
  | PushNone
and value
  = None
  | Unit
  | Bool of bool
  | Int of int
  (* Variant * Params *)
  | Enum of int * value list
  (* Params * Locals * Body *)
  | Func of int * value list  * directive list
  | MatchTag of id
and env =
  { locals: int
  ; stack: value array
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

let list_sep ppf _ = Format.fprintf ppf ",@ "

let rec pp_directive ppf directive = match directive with
  | Expr e -> Format.fprintf ppf "@[<4>Expr("; pp_expr ppf e; Format.fprintf ppf ")@]"
  | Stmt s -> Format.fprintf ppf "@[<4>Stmt("; pp_stmt ppf s; Format.fprintf ppf ")@]"
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (string_of_bin_op op);
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (string_of_unary_op op);
  | Capture (params, captures, body) ->
     Format.fprintf ppf "@[<4>Capture(%d,@ %d,@ " params captures;
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
  | Builtin _ -> Format.fprintf ppf "Builtin"
  | Call args -> Format.fprintf ppf "Call(%d)" args
  | PopEnv -> Format.fprintf ppf "PopEnv"
  | Constructor (variant, params) -> Format.fprintf ppf "Constructor(%d, %d)" variant params
  | Pop -> Format.fprintf ppf "Pop"
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | Match id -> Format.fprintf ppf "Match(%d)" id
  | Case (id, pattern, block) ->
     Format.fprintf ppf "@[<4>Case(%d" id;
     list_sep ppf ();
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [pattern; block];
     Format.fprintf ppf ")@]"
  | PatternVar id -> Format.fprintf ppf "PatternVar(%d)" id
  | PatternEnum var -> Format.fprintf ppf "PatternEnum(%d)" var
  | SkipCase -> Format.fprintf ppf "SkipCase"
  | SkipMatch id -> Format.fprintf ppf "SkipMatch(%d)" id
  | EndMatch id -> Format.fprintf ppf "EndMatch(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "@[<4>If(";
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [tblock; fblock];
     Format.fprintf ppf ")@]"
  | While (id, cond, lblock, eblock) ->
     Format.fprintf ppf "@[<4>While(%d" id;
     list_sep ppf ();
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [cond; lblock; eblock];
     Format.fprintf ppf ")@]"
  | Continue id -> Format.fprintf ppf "Continue(%d)" id
  | Break id -> Format.fprintf ppf "Break(%d)" id
  | Seq -> Format.fprintf ppf "Seq"
  | PushNone -> Format.fprintf ppf "PushNone"
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"

let rec pp_value ppf value = match value with
  | None -> Format.fprintf ppf "None"
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | Enum (variant, params) ->
     Format.fprintf ppf "@[<4>Enum(%d,@ " variant;
     pp_value_list ppf params;
     Format.fprintf ppf ")@]"
  | MatchTag id -> Format.fprintf ppf "MatchTag(%d)" id
  | Func (params, locals, body) ->
     Format.fprintf ppf "@[<4>Func(%d,@ " params;
     pp_value_list ppf locals;
     list_sep ppf ();
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
and pp_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_value ppf values;
  Format.fprintf ppf ")@]"

let pp_env ppf env =
  Format.fprintf ppf "@[<1>(%d,@ " env.locals;
  pp_value_list ppf (Array.to_list env.stack);
  Format.fprintf ppf ")@]"

let pp_env_list ppf envs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_env ppf envs;
  Format.fprintf ppf ")@]"

(* Gets unique id for each match statement *)
let new_match_id =
  let match_idx = ref 0 in
  let eval_new () =
    let new_idx = !match_idx in
    incr match_idx;
    new_idx in
  eval_new

let funcs = ref (Array.of_list [])

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let make_block stmts = List.flatten (List.map (fun s -> [Stmt s; Seq]) stmts) @ [PushNone]

let rec make_pattern pattern = match pattern with
  | Variable (_, id) -> [PatternVar id]
  | Enum (_, var, patterns) ->
     PatternEnum var :: (patterns |> List.map make_pattern |> List.flatten)

let make_case id (_, pattern, block) = Case (id, make_pattern pattern, make_block block)

let step directives values envs = match directives, values, envs with
  (* Nothing to do so we're done *)
  | [], values, envs -> [], values, envs
  (* Push function to top *)
  | Expr (FuncExpr (_, id)) :: rest, _, _ -> rest, (!funcs).(id) :: values, envs
  (* Push local to top *)
  | Expr (EnvExpr (_, id)) :: rest, _, env :: _ -> rest, env.stack.(id) :: values, envs
  (* Push local to top *)
  | Expr (BoundExpr (_, id)) :: rest, _, env :: _-> rest, env.stack.(id) :: values, envs
  (* Push arg to top *)
  | Expr (ArgExpr (_, id)) :: rest, _, env :: _ -> rest, env.stack.(id + env.locals) :: values, envs
  (* Push int to top *)
  | Expr (IntExpr (_, i)) :: rest, _, _ -> rest, Int i :: values, envs
  (* Push bool to top *)
  | Expr (BoolExpr (_, b)) :: rest, _, _ -> rest, Bool b :: values, envs
  (* Push first arg then second arg then compute *)
  | Expr (BinExpr (_, op, lhs, rhs)) :: rest, _, _ ->
     Expr lhs :: Expr rhs :: BinOp op :: rest, values, envs
  (* Push arg then compute *)
  | Expr (UnaryExpr (_, op, e)) :: rest, _, _ -> Expr e :: UnaryOp op :: rest, values, envs
  (* Evaluate args then capture as lambda *)
  | Expr (LambdaExpr (_, params, captures, body)) :: rest, _, _ ->
     let capturing = List.map (fun e -> Expr e) (Array.to_list captures) in
     capturing @ (Capture (params, Array.length captures, [Expr body]) :: rest), values, envs
  (* Evaluate callee then args then call *)
  | Expr (CallExpr (_, callee, args)) :: rest, _, _ ->
     let evaled_args = List.map (fun e -> Expr e) (Array.to_list args) in
     Expr callee :: evaled_args @ (Call (Array.length args) :: PopEnv :: rest), values, envs
  (* Evaluate parameters then make the constructor *)
  | Expr (ConstructorExpr (_, variant, params)) :: rest, _, _ ->
     let evaled_params = List.map (fun e -> Expr e) (Array.to_list params) in
     evaled_params @ (Constructor (variant, Array.length params) :: rest), values, envs

  (* Calculate value and then continue *)
  | Stmt (ReturnStmt (_, e)) :: rest, _, _ -> Expr e :: rest, values, envs
  (* Eval expression pop then continue *)
  | Stmt (ExprStmt (_, e)) :: rest, _, _ -> Expr e :: Pop :: PushNone :: rest, values, envs
  (* Eval expression bind then continue *)
  | Stmt (BindStmt (_, id, e)) :: rest, _, _ -> Expr e :: Bind id :: rest, values, envs
  (* Eval expression then continue *)
  | Stmt (MatchStmt (_, e, cases)) :: rest, _, _ ->
     let id = new_match_id () in
     let cases' = List.map (make_case id) cases in
     Expr e :: Match id :: cases' @ (EndMatch id :: rest), values, envs
  (* Eval condition then continue *)
  | Stmt (IfStmt (_, cond, tblock, fblock)) :: rest, _, _ ->
     Expr cond :: If (make_block tblock, make_block fblock) :: rest, values, envs
  (* Eval condition then continue *)
  | Stmt (WhileStmt (_, id, cond, lblock, eblock)) :: rest, _, _ ->
     let lblock' = make_block (lblock @ [ContinueStmt (Lexing.dummy_pos, id)]) in
     let eblock' = make_block eblock in
     Expr cond :: While (id, [Expr cond], lblock', eblock') :: rest, values, envs
  (* Start continuing *)
  | Stmt (ContinueStmt (_, id)) :: rest, _, _ -> Continue id :: rest, values, envs
  (* Start breaking *)
  | Stmt (BreakStmt (_, id)) :: rest, _, _ -> Break id :: rest, values, envs

  (* Compute binary op then continue *)
  | BinOp op :: rest, rhs :: lhs :: values', _ -> rest, do_bin_op op lhs rhs :: values', envs
  (* Compute unary op then continue *)
  | UnaryOp op :: rest, v :: values', _ -> rest, do_unary_op op v :: values', envs

  (* Create lambda from args *)
  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Func (params, captures', body) :: values', envs
  (* Evaluate built-in function *)
  | Builtin f :: rest, _, env :: _ -> rest, f env :: values, envs

  (* Call function with args *)
  | Call args :: rest, _, _ ->
     let args', values' = take_rev args [] values in
     (match values' with
      | Func (params, locals, body) :: values'' when params = args ->
         let env' = { stack = Array.of_list (locals @ args')
                    ; locals = List.length locals
                    } in
         body @ rest, values'', env' :: envs
      | _ -> failwith "type mismatch")

  (* Pop top environment *)
  | PopEnv :: rest, _, _ :: envs' -> rest, values, envs'

  (* Create enum from parameters *)
  | Constructor (variant, params) :: rest, _, _ ->
     let params', values' = take_rev params [] values in
     rest, Enum (variant, params') :: values', envs

  (* Pop top value *)
  | Pop :: rest, _ :: values', _ -> rest, values', envs
  (* Bind value and push None *)
  | Bind id :: rest, v :: values', env :: _ ->
     env.stack.(id) <- v;
     rest, None :: values', envs

  (* If true then evaluate true block *)
  | If (tblock, _) :: rest, Bool true :: values', _ -> tblock @ rest, values', envs
  (* If false then evaluate false block *)
  | If (_, fblock) :: rest, Bool false :: values', _ -> fblock @ rest, values', envs

  (* Found a case *)
  | Match mid :: Case (cid, pat, block) :: rest, v :: _, env :: _ when mid = cid ->
     let stack = Array.copy env.stack in
     let env' = { stack
                ; locals = env.locals
                } in
     pat @ (Case (cid, pat, block) :: rest), v :: MatchTag mid :: values, env' :: envs
  (* No more cases so push None *)
  | Match mid :: EndMatch eid :: rest, _ :: values', _ when mid = eid -> rest, None :: values', envs
  (* Find case or end *)
  | Match id :: _ :: rest, _, _ -> Match id :: rest, values, envs
  (* Case successful so run block and skip match *)
  | Case (_, _, block) :: rest, MatchTag tid :: _ :: values', env :: env' :: envs' ->
     Array.iteri (Array.set env'.stack) env.stack;
     block @ (SkipMatch tid :: rest), values', env' :: envs'
  (* Variable pattern matches with anything. Do bind and continue *)
  | PatternVar id :: rest, v :: values', env :: _ ->
     env.stack.(id) <- v;
     rest, values', envs
  (* Enum pattern matches with correct variant. Push params to check *)
  | PatternEnum pvar :: rest, Enum (evar, params) :: values', _ when pvar = evar ->
     rest, params @ values', envs
  (* Enum pattern doesn't match bad variant. Drop stack past tag and look for next step *)
  | PatternEnum _ :: rest, Enum _ :: values', _ :: envs' ->
     let rec drop values = match values with
       | [] -> failwith "No MatchTag on stack"
       | MatchTag _ :: rest -> rest
       | _ :: rest -> drop rest in
     let values'' = drop values' in
     SkipCase :: rest, values'', envs'
  (* Reached the case that failed so skip over it. *)
  | SkipCase :: Case (id, _, _) :: rest, _, _ -> Match id :: rest, values, envs
  (* Not reached case to skip so continue jumping. *)
  | SkipCase :: _ :: rest, _, _ -> SkipCase :: rest, values, envs
  (* Finished match that we completed so continue. *)
  | SkipMatch sid :: EndMatch eid :: rest, _, _ when sid = eid -> rest, values, envs
  (* Not reached end of match so continue jumping. *)
  | SkipMatch id :: _ :: rest, _, _ -> SkipMatch id :: rest, values, envs


  (* While condition was true so start loop *)
  | While (_, _, lblock, _) :: _, Bool true :: values', _ -> lblock @ directives, values', envs
  (* While condition was false so enter end block *)
  | While (_, _, _, eblock) :: rest, Bool false :: values', _ -> eblock @ rest, values', envs
  (* Continue reached correct while so evaluate condition *)
  | Continue id :: While (id', cond, lblock, eblock) :: rest, _, _ when id = id' ->
     cond @ (While (id', cond, lblock, eblock) :: rest), values, envs
  (* Continue not reached while so skip *)
  | Continue id :: _ :: rest, _, _ -> Continue id :: rest, values, envs
  (* Break reached correct while so skip over *)
  | Break id :: While (id', _, _, _) :: rest, _, _ when id = id' -> rest, None :: values, envs
  (* Break not reached while so skip *)
  | Break id :: _ :: rest, _, _ -> Break id :: rest, values, envs

  (* Statement didn't return so eval next one *)
  | Seq :: rest, None :: values', _ -> rest, values', envs
  (* Statement returned so skip next one *)
  | Seq :: _ :: rest, _, _ -> rest, values, envs
  (* Push none to stack *)
  | PushNone :: rest, _, _ -> rest, None :: values, envs

  (* Reaching here is a bug *)
  | _, _, _ -> failwith "runtime error"

let stage = ref 0

let driver debug env directives =
  let rec iter (directives, values, envs) =
    if debug then begin
        incr stage;
        Format.fprintf Format.std_formatter "@[<4>Step %d: (@," !stage;
        pp_directive_list Format.std_formatter directives;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_value_list Format.std_formatter values;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_env_list Format.std_formatter envs;
        Format.fprintf Format.std_formatter ")@]@\n" end;
    match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter (directives, [], [env])

module FuncMap = Map.Make(String)

let print_int env = match env.stack.(0) with
  | Int x -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool env = match env.stack.(0) with
  | Bool b -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _ = Int (read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", Func (1, [], [Builtin print_int])
                                           ; "print_bool", Func (1, [], [Builtin print_bool])
                                           ; "input_int", Func (0, [], [Builtin input_int])])

let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      FuncMap.find func.name builtins
    else
      failwith "built-in has no definition"
  else
    let locals = List.init func.num_locals (fun _ -> None) in
    Func (func.num_params, locals, make_block (Option.get func.body))

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  funcs := Array.map interpret_func program';
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { stack = Array.make main.num_locals None
            ; locals = main.num_locals
            } in
  match Array.get !funcs main.id with
  | Func(0, _, body) ->
     (match driver debug env body with
      | [None] -> ()
      | _ -> failwith "type mismatch")
  | _ -> failwith "main not a function"
