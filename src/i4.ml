open Typed_ast
open Ops

type directive
  = BinOp of bin_op
  | UnaryOp of unary_op
  | PushFunc of int
  | PushLocal of int
  | PushArg of int
  | Push of value
  | Pop
  | PopEnv
  | Seq
  | Continue of int
  | Break of int
  | BlockEnd
  (* Params * Locals * Body *)
  | Capture of int * int * directive list
  | Builtin of string
  | Call of int
  | Bind of int
  | If of directive list * directive list
  | While of id * directive list * directive list * directive list
and value
  = None
  | Unit
  | Bool of bool
  | Int of int
  (* Params * Locals * Body *)
  | Func of int * value list  * directive list
and env =
  { locals: int
  ; stack: value ref array
  }

module FuncMap = Map.Make(String)

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
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (string_of_bin_op op)
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (string_of_unary_op op)
  | PushFunc i -> Format.fprintf ppf "PushFunc(%d)" i
  | PushLocal i -> Format.fprintf ppf "PushLocal(%d)" i
  | PushArg i -> Format.fprintf ppf "PushArg(%d)" i
  | Push v ->
     Format.fprintf ppf "@[<4>Push(";
     pp_value ppf v;
     Format.fprintf ppf ")@]"
  | Pop -> Format.fprintf ppf "Pop"
  | PopEnv -> Format.fprintf ppf "PopEnv"
  | Seq -> Format.fprintf ppf "Seq"
  | Continue id -> Format.fprintf ppf "Continue(%d)" id
  | Break id -> Format.fprintf ppf "Break(%d)" id
  | BlockEnd -> Format.fprintf ppf "BlockEnd"
  | Capture (params, captures, body) ->
     Format.fprintf ppf "@[<4>Capture(%d,@ %d,@ " params captures;
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call args -> Format.fprintf ppf "Call(%d)" args
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "@[<4>If(";
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [tblock; fblock];
     Format.fprintf ppf ")@]"
  | While (id, cond, lblock, eblock) ->
     Format.fprintf ppf "@[<4>While(%d" id;
     list_sep ppf ();
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [cond; lblock; eblock];
     Format.fprintf ppf ")@]"
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"
and pp_value ppf value = match value with
  | None -> Format.fprintf ppf "None"
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
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
  pp_value_list ppf (Array.to_list (Array.map (!) env.stack));
  Format.fprintf ppf ")@]"

let pp_env_list ppf envs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_env ppf envs;
  Format.fprintf ppf ")@]"

let print_int env = match !(env.stack.(0)) with
  | Int x -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool env = match !(env.stack.(0)) with
  | Bool b -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _ = Int (read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", (print_int, 1)
                                           ; "print_bool", (print_bool, 1)
                                           ; "input_int", (input_int, 0)])

let funcs = ref (Array.of_list [])

let rec flatten_expr acc expr = match expr with
  (* Push function to top *)
  | FuncExpr (_, id) -> PushFunc id :: acc
  (* Push local to top *)
  | EnvExpr (_, id) -> PushLocal id :: acc
  (* Push local to top *)
  | BoundExpr (_, id) -> PushLocal id :: acc
  (* Push arg to top *)
  | ArgExpr (_, id) -> PushArg id :: acc
  (* Push int to top*)
  | IntExpr (_, i) -> Push (Int i) :: acc
  (* Push bool to top *)
  | BoolExpr (_, b) -> Push (Bool b) :: acc
  (* Push first arg then second then compute *)
  | BinExpr (_, op, lhs, rhs) -> flatten_expr (flatten_expr (BinOp op :: acc) rhs) lhs
  (* Push arg then compute *)
  | UnaryExpr (_, op, e) -> flatten_expr (UnaryOp op :: acc) e
  (* Evaluate args then capture as lambda *)
  | LambdaExpr (_, params, captures, body) ->
     let acc' = Capture (params, Array.length captures, flatten_expr [] body) :: acc in
     List.fold_left flatten_expr acc' (List.rev (Array.to_list captures))
  (* Evaluate callee then args then call *)
  | CallExpr (_, callee, args) ->
     let acc' = Call (Array.length args) :: PopEnv :: acc in
     flatten_expr (List.fold_left flatten_expr acc' (List.rev (Array.to_list args))) callee

let rec flatten_stmt acc stmt = match stmt with
  (* Calculate value then continue *)
  | ReturnStmt (_, e) -> flatten_expr acc e
  (* Evaluate expression pop then continue *)
  | ExprStmt (_, e) -> flatten_expr (Pop :: Push None :: acc) e
  (* Evaluate expression bind then conntinue *)
  | BindStmt (_, id, e) -> flatten_expr (Bind id :: acc) e
  (* Evaluate condiition branch then continue *)
  | IfStmt (_, cond, tblock, fblock) ->
     flatten_expr (If (make_if_block tblock, make_if_block fblock) :: acc) cond
  | WhileStmt (_, id, cond, lblock, eblock) ->
     let lblock' = make_while_block id lblock in
     let eblock' = make_if_block eblock in
     let acc' = (While (id, flatten_expr [] cond, lblock', eblock') :: acc) in
     flatten_expr acc' cond
  | ContinueStmt (_, id) -> [Continue id]
  | BreakStmt (_, id) -> [Break id]
and sequence acc stmt = flatten_stmt (Seq :: acc) stmt
and make_if_block stmts = List.fold_left sequence [Push None] (List.rev stmts)
and make_while_block id stmts = List.fold_left sequence [Continue id] (List.rev stmts)

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let step directives values envs = match directives, values, envs with
  (* Nothing to do so we're done *)
  | [], values, envs -> [], values, envs

  (* Compute binary op then continue *)
  | BinOp op :: rest, rhs :: lhs :: values', _ -> rest, do_bin_op op lhs rhs :: values', envs
  (* Compute unary op then continue *)
  | UnaryOp op :: rest, v :: values', _ -> rest, do_unary_op op v :: values', envs

  (* Push a function *)
  | PushFunc id :: rest, _, _ -> rest, Array.get !funcs id :: values, envs
  (* Push a local *)
  | PushLocal id :: rest, _, env :: _ -> rest, !(Array.get env.stack id) :: values, envs
  (* Push an arg *)
  | PushArg id :: rest, _, env :: _ ->
     rest, !(Array.get env.stack (id + env.locals)) :: values, envs
  (* Push a constant *)
  | Push v :: rest, _, _ -> rest, v :: values, envs
  (* Pop a value *)
  | Pop :: rest, _ :: values', _ -> rest, values', envs
  (* Pop an environment *)
  | PopEnv :: rest, _, _ :: envs' -> rest, values, envs'

  (* Evaluate next statement if no return *)
  | Seq :: rest, None :: values', _ -> rest, values', envs
  (* Skip until reached EndBlock *)
  | Seq :: rest, _, _ ->
     let rec iter stack = match stack with
       | BlockEnd :: rest -> rest
       | _ :: rest -> iter rest
       | _ -> failwith "Seq without BlockEnd" in
     iter rest, values, envs
  (* No-op (function didn't return) *)
  | BlockEnd :: rest, _, _ ->
     rest, values, envs

  (* Build function from captures on stack *)
  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Func (params, captures', body) :: values', envs
  (* Eval builtin function *)
  | Builtin name :: rest, _, env :: _ ->
     rest, (fst (FuncMap.find name builtins)) env :: values, envs

  (* Call function with args *)
  | Call args :: rest, _, _ ->
     let args', values' = take_rev args [] values in
     (match values' with
      | Func (params, locals, body) :: values'' when params = args ->
         let env' = { stack = Array.of_list (List.map ref (locals @ args'))
                    ; locals = List.length locals
                    } in
         body @ rest, values'', env' :: envs
      | _ -> failwith "type mismatch"
     )

  (* Bind value and push none *)
  | Bind id :: rest, v :: values', env :: _ ->
     (Array.get env.stack id) := v;
     rest, None :: values', envs
  (* If true then evaluate true block *)
  | If (tblock, _) :: rest, Bool true :: values', _ -> tblock @ rest, values', envs
  (* If false then evaluate false block *)
  | If (_, fblock) :: rest, Bool false :: values', _ -> fblock @ rest, values', envs

  (* While condition was true so start loop *)
  | While (_, _, lblock, _) :: _, Bool true :: values', _ -> lblock @ directives, values', envs
  (* While condition was false so enter end block *)
  | While (_, _, _, eblock) :: rest, Bool false :: values', _ -> eblock @ rest, values', envs
  (* Skip until reach correct while *)
  | Continue id :: rest, _, _ ->
     let rec iter stack = match stack with
       | While (id', cond, _, _) :: _ when id = id' -> cond @ stack
       | _ :: rest -> iter rest
       | _ -> failwith "Continue without While" in
     iter rest, values, envs
  (* Skip until reach correct while *)
  | Break id :: rest, _, _ ->
     let rec iter stack = match stack with
       | While (id', _, _, _) :: rest when id = id' -> rest
       | _ :: rest -> iter rest
       | _ -> failwith "Break without While" in
     iter rest, None :: values, envs

  (* Reaching here is a bug *)
  | _, _, _ -> failwith "type mismatch"

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
        Format.fprintf Format.std_formatter ")@]";
        Format.pp_print_newline Format.std_formatter ();
      end;
    match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter (directives, [], [env])


let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let _, params = FuncMap.find func.name builtins in
      Func (params, [], [Builtin func.name; Seq; Push None; BlockEnd])
    else
      failwith "built-in has no definition"
  else
    let locals = List.init func.num_locals (fun _ -> None) in
    let block = List.fold_left sequence [Push None; BlockEnd] (List.rev (Option.get func.body)) in
    Func (func.num_params, locals, block)

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  funcs := Array.map interpret_func program';
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { stack = Array.of_list []; locals = 0 } in
  match Array.get !funcs main.id with
  | Func(0, binds, body) ->
     (match driver debug env [Push (Func (0, binds, body)); Call 0] with
      | [None] -> ()
      | _ -> failwith "type mismatch")
  | _ -> failwith "main not a function"
