(* Linear code interpretter.
 *
 * Code is no longer a stack and is instead split into blocks. *)

open Typed_ast
open Ops

type directive
  (* Binary op of two values *)
  = BinOp of bin_op
  (* Unary of of a value *)
  | UnaryOp of unary_op
  (* Pushes function *)
  | PushFunc of int
  (* Pushes local value *)
  | PushLocal of int
  (* Pushes argument *)
  | PushArg of int
  (* Pushes constant value *)
  | Push of value
  (* Lambda capture: #params * #locals * block *)
  | Capture of int * int * int
  (* Calls builtin function *)
  | Builtin of string
  (* Calls function: #args * return block *)
  | Call of int * int
  (* Jumps to return block in env *)
  | Return
  (* Creates a new enum type: variant * #params *)
  | Constructor of int * int
  (* Pops value *)
  | Pop
  (* Binds local to value *)
  | Bind of int
  (* Creates environment for pattern matching *)
  | Case
  (* Pattern matching enum: variant * next case *)
  | PatternEnum of int * int option
  (* Pattern matching int: value * next case *)
  | PatternInt of int * int option
  (* Pattern matching bool: value * next case *)
  | PatternBool of bool * int option
  (* Indicates end of pattern matching *)
  | PatternEnd
  (* Conditional jump: true block * false block *)
  | If of int * int
  (* Jumps to constant block *)
  | Jump of int
and value
  = Unit
  | Bool of bool
  | Int of int
  (* Variant * Parameters *)
  | Enum of int * value list
  | MatchTag
  (* Params * Locals * Block *)
  | Func of int * value list  * int
and env =
  { return: int
  ; locals: int
  ; stack: value array
  }

module BlockMap = Map.Make(Int)
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

(*** Printing ***)

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
  | Capture (params, captures, body) ->
     Format.fprintf ppf "Capture(%d, %d, %d)" params captures body;
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call (args, return) -> Format.fprintf ppf "Call(%d, %d)" args return
  | Return -> Format.fprintf ppf "Return"
  | Constructor (variant, params) -> Format.fprintf ppf "Constructor(%d, %d)" variant params
  | Pop -> Format.fprintf ppf "Pop"
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | Case -> Format.fprintf ppf "Case"
  | PatternEnum (var, None) -> Format.fprintf ppf "PatternEnum(%d)" var
  | PatternEnum (var, Some block) -> Format.fprintf ppf "PatternEnum(%d, %d)" var block
  | PatternInt (i, None) -> Format.fprintf ppf "PatternInt(%d)" i
  | PatternInt (i, Some block) -> Format.fprintf ppf "PatternInt(%d, %d)" i block
  | PatternBool (b, None) -> Format.fprintf ppf "PatternBool(%B)" b
  | PatternBool (b, Some block) -> Format.fprintf ppf "PatternBool(%B, %d)" b block
  | PatternEnd -> Format.fprintf ppf "PatternEnd"
  | If (tblock, fblock) ->
     Format.fprintf ppf "If(%d, %d)" tblock fblock
  | Jump block -> Format.fprintf ppf "Jump(%d)" block
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"
and pp_value ppf value = match value with
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | Enum (variant, params) ->
     Format.fprintf ppf "@[<4>Enum(%d,@ " variant;
     pp_value_list ppf params;
     Format.fprintf ppf ")@]"
  | MatchTag -> Format.fprintf ppf "MatchTag"
  | Func (params, locals, body) ->
     Format.fprintf ppf "@[<4>Func(%d,@ " params;
     pp_value_list ppf locals;
     list_sep ppf ();
     Format.fprintf ppf "%d)@]" body
and pp_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_value ppf values;
  Format.fprintf ppf ")@]"

let pp_env ppf env =
  Format.fprintf ppf "@[<1>(%d,@ %d,@ " env.return env.locals;
  pp_value_list ppf (Array.to_list env.stack);
  Format.fprintf ppf ")@]"

let pp_env_list ppf envs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_env ppf envs;
  Format.fprintf ppf ")@]"

(*** Builtins ***)

let print_int env = match env.stack.(0) with
  | Int x -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool env = match env.stack.(0) with
  | Bool b -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _ = Int (read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", (print_int, 1)
                                           ; "print_bool", (print_bool, 1)
                                           ; "input_int", (input_int, 0)])

let funcs = ref (Array.of_list [])

(*** Utilities ***)

let add_block, get_block, reserve_loop, get_loop, get_continue, set_loop =
  let block_id = ref 0 in
  let next_block () =
    let block = !block_id in
    incr block_id; block in
  let blocks = ref (BlockMap.add (-1) [] BlockMap.empty) in
  let loops = ref BlockMap.empty in
  let eval_add block =
    let this_block = next_block () in
    blocks := BlockMap.add this_block block !blocks;
    this_block in
  let eval_get block = BlockMap.find block !blocks in
  let reserve_loop id continue =
    let this_block = next_block () in
    loops := BlockMap.add id (this_block, continue) !loops in
  let get_loop id = fst (BlockMap.find id !loops) in
  let get_continue id = snd (BlockMap.find id !loops) in
  let set_loop id block = blocks := BlockMap.add (get_loop id) block !blocks in
  eval_add, eval_get, reserve_loop, get_loop, get_continue, set_loop

(*** Construction ***)

let rec flatten_expr acc expr = match expr with
  | FuncExpr (_, id) -> PushFunc id :: acc
  | EnvExpr (_, id) -> PushLocal id :: acc
  | BoundExpr (_, id) -> PushLocal id :: acc
  | ArgExpr (_, id) -> PushArg id :: acc
  | IntExpr (_, i) -> Push (Int i) :: acc
  | BoolExpr (_, b) -> Push (Bool b) :: acc
  | BinExpr (_, op, lhs, rhs) -> flatten_expr (flatten_expr (BinOp op :: acc) rhs) lhs
  | UnaryExpr (_, op, e) -> flatten_expr (UnaryOp op :: acc) e
  | LambdaExpr (_, params, captures, body) ->
     let captured = add_block (flatten_expr [Return] body) in
     let acc' = Capture (params, Array.length captures, captured) :: acc in
     captures |> Array.to_list |> List.rev |> List.fold_left flatten_expr acc'
  | CallExpr (_, callee, args) ->
     let return = add_block acc in
     let call' = Call (Array.length args, return) in
     let acc' = args |> Array.to_list |> List.rev |> List.fold_left flatten_expr [call'] in
     flatten_expr acc' callee
  | ConstructorExpr (_, variant, params) ->
     let acc' = Constructor (variant, Array.length params) :: acc in
     params |> Array.to_list |> List.rev |> List.fold_left flatten_expr acc'

let rec flatten_pattern next acc pattern = match pattern with
  | Variable (_, id) -> Bind id :: acc
  | Enum (_, var, patterns) ->
     let acc' = patterns |> List.rev |> List.fold_left (flatten_pattern next) acc in
     PatternEnum (var, next) :: acc'
  | Ignore _ -> Pop :: acc
  | Int (_, i) -> PatternInt (i, next) :: acc
  | Bool (_, b) -> PatternBool (b, next) :: acc

let rec flatten_stmt acc stmt = match stmt with
  | ReturnStmt (_, e) -> flatten_expr (Return :: acc) e (* Safe as return clears stack *)
  | ExprStmt (_, e) -> flatten_expr (Pop :: acc) e
  | BindStmt (_, id, e) -> flatten_expr (Bind id :: acc) e
  | MatchStmt (_, e, cases) ->
     let continue = add_block acc in
     let first_case, _ = List.fold_left flatten_case (None, continue) (List.rev cases) in
     flatten_expr [Jump (Option.get first_case)] e
  | IfStmt (_, cond, tblock, fblock) ->
     let continue = add_block acc in
     let tblock' = make_block tblock continue |> add_block in
     let fblock' = make_block fblock continue |> add_block in
     flatten_expr [If (tblock', fblock')] cond
  | WhileStmt (_, id, cond, lblock, eblock) ->
     let continue = add_block acc in
     reserve_loop id continue;
     let lblock' = make_block lblock (get_loop id) |> add_block in
     let eblock' = make_block eblock continue |> add_block in
     set_loop id (flatten_expr [If (lblock', eblock')] cond);
     [Jump (get_loop id)]
  | ContinueStmt (_, id) -> Jump (get_loop id) :: acc
  | BreakStmt (_, id) -> Jump (get_continue id) :: acc
and make_block stmts continue =
  List.fold_left flatten_stmt [Jump continue] (List.rev stmts)
and flatten_case (next, continue) (_, pattern, block) =
  let block' = PatternEnd :: make_block block continue in
  let id = add_block (Case :: flatten_pattern next block' pattern) in
  Some id, continue

let flatten_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let _, params = FuncMap.find func.name builtins in
      Func (params, [], add_block [Builtin func.name; Return])
    else
      failwith "built-in has no definition"
  else
    let locals = List.init func.num_locals (fun _ -> Unit) in
    let block = List.fold_left flatten_stmt [Push Unit; Return] (List.rev (Option.get func.body)) in
    Func (func.num_params, locals, add_block block)

(*** Evaluation ***)

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let pattern_fail next values envs =
  let rec drop values = match values with
    | [] -> failwith "No MatchTag on stack"
    | MatchTag :: rest -> rest
    | _ :: rest -> drop rest in
  get_block next, drop values, envs

let step directives values envs = match directives, values, envs with
  | [], values, envs -> [], values, envs

  | BinOp op :: rest, rhs :: lhs :: values', _ -> rest, do_bin_op op lhs rhs :: values', envs
  | UnaryOp op :: rest, v :: values', _ -> rest, do_unary_op op v :: values', envs

  | PushFunc id :: rest, _, _ -> rest, (!funcs).(id) :: values, envs
  | PushLocal id :: rest, _, env :: _ -> rest, env.stack.(id) :: values, envs
  | PushArg id :: rest, _, env :: _ ->
     rest, env.stack.(id + env.locals) :: values, envs
  | Push v :: rest, _, _ -> rest, v :: values, envs

  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Func (params, captures', body) :: values', envs
  | Builtin name :: rest, _, env :: _ ->
     rest, (fst (FuncMap.find name builtins)) env :: values, envs

  | Call (args, return) :: _, _, _ ->
     let args', values' = take_rev args [] values in
     (match values' with
      | Func (params, locals, body) :: values'' when params = args ->
         let env' = { return = return
                    ; stack = Array.of_list (locals @ args')
                    ; locals = List.length locals
                    } in
         get_block body, values'', env' :: envs
      | _ -> failwith "type mismatch")

  | Return :: _, _, env :: envs' -> get_block env.return, values, envs'

  | Constructor (variant, params) :: rest, _, _ ->
     let params', values' = take_rev params [] values in
     rest, Enum (variant, params') :: values', envs

  | Pop :: rest, _ :: values', _ -> rest, values', envs

  | Bind id :: rest, v :: values', env :: _ ->
     env.stack.(id) <- v;
     rest, values', envs

  | Case :: rest, v :: _, env :: _ ->
     let stack = Array.copy env.stack in
     let env' = { stack
                ; locals = env.locals
                ; return = env.return
                } in
     rest, v :: MatchTag :: values, env' :: envs
  | PatternEnum (pvar, _) :: rest, Enum (evar, params) :: values', _ when pvar = evar ->
     rest, params @ values', envs
  | PatternEnum (_, Some next) :: _, Enum _ :: values', _ :: envs' ->
     pattern_fail next values' envs'
  | PatternInt (i, _) :: rest, Int j :: values', _ when i = j ->
     rest, values', envs
  | PatternInt (_, Some next) :: _, Int _ :: values', _ :: envs' ->
     pattern_fail next values' envs'
  | PatternBool (b, _) :: rest, Bool b' :: values', _ when b = b' ->
     rest, values', envs
  | PatternBool (_, Some next) :: _, Bool _ :: values', _ :: envs' ->
     pattern_fail next values' envs'
  | PatternEnd :: rest, MatchTag :: _ :: values', env :: env' :: envs' ->
     Array.iteri (Array.set env'.stack) env.stack;
     rest, values', env' :: envs'

  | If (tblock, _) :: _, Bool true :: values', _ -> get_block tblock, values', envs
  | If (_, fblock) :: _, Bool false :: values', _ -> get_block fblock, values', envs

  | Jump block :: _, _, _ -> get_block block, values, envs

  | _, _, _ -> failwith "type mismatch"

let stage = ref 0

let driver debug env directives =
  let rec iter (directives, values, envs) =
    if debug
    then
        (let ppf = Format.std_formatter in
         incr stage;
         Format.fprintf ppf "@[<4>Step %d:@ " !stage;
         pp_directive_list ppf directives;
         list_sep ppf ();
         pp_value_list ppf values;
         list_sep ppf ();
         pp_env_list ppf envs;
         Format.fprintf ppf "@]";
         Format.pp_print_newline ppf ());
    match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter (directives, [], [env])

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  funcs := Array.map flatten_func program';
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { return = -1
            ; stack = Array.make main.num_locals Unit
            ; locals = main.num_locals
            } in
  match (!funcs).(main.id) with
  | Func(0, _, body) ->
     (match driver debug env (get_block body) with
      | [Unit] -> ()
      | _ -> failwith "type mismatch")
  | _ -> failwith "main not a function"
