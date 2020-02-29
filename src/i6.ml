(* Stack-machine interpretter.
 *
 * One shared stack for values and environment, with a separate heap. *)

open Typed_ast
open Ops

type env =
  { locals: int
  ; args: int
  }

type stack_value
  = Unit
  | Bool of bool
  | Int of int
  | CodeIndex of int
  | HeapIndex of int

type heap_value
  = Sized of stack_value
  (* variant * #params *)
  | Enum of int * int
  (* #locals * block *)
  | Func of int * int

type directive
  (* Binary op of two values *)
  = BinOp of bin_op
  (* Unary of of a value *)
  | UnaryOp of unary_op
  (* Pushes function *)
  | PushFunc of int
  (* Pushes stack value, relative to top *)
  | PushStack of int
  (* Pushes constant value *)
  | Push of stack_value
  (* Allocates space and pushes to heap. *)
  (* Parts are size on heap and heap value *)
  | AllocHeap of int * heap_value
  (* Calls builtin function *)
  | Builtin of string
  (* Calls function: #args * return block *)
  | Call of int * int
  (* Allocate space on the stack. *)
  | AllocStack of int
  (* Copies local values from heap onto stack. *)
  | CopyLocals of env
  (* Jumps to address and cleans up env  *)
  | Return of env
  (* Pops values *)
  | Pop of int
  (* Binds local to value *)
  | Bind of int
  (* Creates environment for pattern matching *)
  | Case of int
  (* Pattern matching enum: variant * params * fail branch. *)
  | PatternEnum of int * int * int option
  (* Pattern matching int: value * next case *)
  | PatternInt of int * int option
  (* Pattern matching bool: value * next case *)
  | PatternBool of bool * int option
  (* Indicates end of pattern matching *)
  | PatternEnd of int
  (* Conditional jump: true block * false block *)
  | If of int * int
  (* Jumps to constant block *)
  | Jump of int

module BlockMap = Map.Make(Int)
module FuncMap = Map.Make(String)

let do_bin_op op lhs rhs = match op, lhs, rhs with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Equal, Int a, Int b -> Bool (a = b)
  | Equal, Bool a, Bool b -> Bool (a = b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _, _, _ -> failwith "runtime error: bad args for bin op"

let do_unary_op op e = match op, e with
  | Invert, Bool b -> Bool (not b)
  | _, _ -> failwith "runtime error: bad args for unary op"

(*** Printing ***)

let list_sep ppf _ = Format.fprintf ppf ",@ "

let pp_list pp ppf list =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp ppf list;
  Format.fprintf ppf ")@]"

let pp_stack_value ppf value = match value with
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | CodeIndex i -> Format.fprintf ppf "@%d" i
  | HeapIndex i -> Format.fprintf ppf "*%d" i
let pp_stack_value_list ppf values = values |> List.map (!) |> pp_list pp_stack_value ppf

let pp_heap_value ppf value = match value with
  | Sized v -> pp_stack_value ppf v
  | Enum (variant, params) -> Format.fprintf ppf "Enum(%d, %d)" variant params
  | Func (locals, body) -> Format.fprintf ppf "Func(%d, %d)" locals body
let pp_heap_value_list ppf values = values |> List.map (!) |> pp_list pp_heap_value ppf

let rec pp_directive ppf directive = match directive with
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (string_of_bin_op op)
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (string_of_unary_op op)
  | PushFunc i -> Format.fprintf ppf "PushFunc(%d)" i
  | PushStack i -> Format.fprintf ppf "PushStack(%d)" i
  | Push v ->
     Format.fprintf ppf "@[<4>Push(";
     pp_stack_value ppf v;
     Format.fprintf ppf ")@]"
  | AllocHeap (i, v) ->
     Format.fprintf ppf "@[<4>AllocHeap(%d,@ " i;
     pp_heap_value ppf v;
     Format.fprintf ppf ")@]"
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call (args, return) -> Format.fprintf ppf "Call(%d, %d)" args return
  | AllocStack locals -> Format.fprintf ppf "AllocStack(%d)" locals
  | CopyLocals env -> Format.fprintf ppf "CopyLocals(%d, %d)" env.locals env.args
  | Return env -> Format.fprintf ppf "Return(%d, %d)" env.locals env.args
  | Pop count -> Format.fprintf ppf "Pop(%d)" count
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | Case depth -> Format.fprintf ppf "Case(%d)" depth
  | PatternEnum (var, params, None) -> Format.fprintf ppf "PatternEnum(%d, %d)" var params
  | PatternEnum (var, params, Some block) ->
     Format.fprintf ppf "PatternEnum(%d, %d, %d)" var params block
  | PatternInt (i, None) -> Format.fprintf ppf "PatternInt(%d)" i
  | PatternInt (i, Some block) -> Format.fprintf ppf "PatternInt(%d, %d)" i block
  | PatternBool (b, None) -> Format.fprintf ppf "PatternBool(%B)" b
  | PatternBool (b, Some block) -> Format.fprintf ppf "PatternBool(%B, %d)" b block
  | PatternEnd depth -> Format.fprintf ppf "PatternEnd(%d)" depth
  | If (tblock, fblock) ->
     Format.fprintf ppf "If(%d, %d)" tblock fblock
  | Jump block -> Format.fprintf ppf "Jump(%d)" block
and pp_directive_list ppf directives = pp_list pp_directive ppf directives

(*** Builtins ***)

let print_int stack = match stack with
  | _ :: { contents = Int x } :: _ ->
     print_int x;
     print_newline ();
     ref Unit :: stack
  | _ -> failwith "type mismatch"

let print_bool stack = match stack with
  | _ :: { contents = Bool b } :: _ ->
     print_string (string_of_bool b);
     print_newline ();
     ref Unit :: stack
  | _ -> failwith "type mismatch"

let input_int stack = ref (Int (read_int ())) :: stack

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", (print_int, 1)
                                           ; "print_bool", (print_bool, 1)
                                           ; "input_int", (input_int, 0)])

let funcs = ref (Array.of_list [])

(*** Utilities ***)

let rec cleanup directives cnt = match directives with
  | [] -> cnt []
  | Call (args, block) :: _ -> cnt [Call (args, block)]
  | Return env :: _ -> cnt [Return env]
  | Jump block :: _ -> cnt [Jump block]
  | If (tblock, fblock) :: _ -> cnt [If (tblock, fblock)]
  | x :: rest -> cleanup rest (fun rest' -> cnt (x :: rest'))

let add_block, get_block, flatten_blocks, reserve_loop, get_loop, get_continue, set_loop =
  let block_id = ref 1 in
  let next_block () =
    let block = !block_id in
    incr block_id; block in
  let blocks = ref (BlockMap.add 0 [] BlockMap.empty) in
  let loops = ref BlockMap.empty in
  let eval_add block =
    let this_block = next_block () in
    cleanup block (fun block' ->
        blocks := BlockMap.add this_block block' !blocks;
        this_block) in
  let eval_get block = BlockMap.find block !blocks in
  let eval_flattens () =
    let output = ref [] in
    BlockMap.map (fun dirs ->
        let pos = List.length !output in
        output := dirs @ !output;
        pos) !blocks in
  let reserve_loop id continue =
    let this_block = next_block () in
    loops := BlockMap.add id (this_block, continue) !loops in
  let get_loop id = fst (BlockMap.find id !loops) in
  let get_continue id = snd (BlockMap.find id !loops) in
  let set_loop id block =
    cleanup block (fun block' -> blocks := BlockMap.add (get_loop id) block' !blocks) in
  eval_add, eval_get, eval_flattens, reserve_loop, get_loop, get_continue, set_loop

(*** Construction ***)

(* Stack is args, return address, locals, temp, ... *)
(* Depth points to return address *)
let rec flatten_expr env acc depth expr = match expr with
  | FuncExpr (_, id) -> PushFunc id :: acc
  | EnvExpr (_, id) -> PushStack (depth - id - 1) :: acc
  | BoundExpr (_, id) -> PushStack (depth - id - 1) :: acc
  | ArgExpr (_, id) -> PushStack (depth + env.args - id) :: acc
  | IntExpr (_, i) -> Push (Int i) :: acc
  | BoolExpr (_, b) -> Push (Bool b) :: acc
  | BinExpr (_, op, lhs, rhs) ->
     flatten_expr env (flatten_expr env (BinOp op :: acc) (depth + 1) rhs) depth lhs
  | UnaryExpr (_, op, e) -> flatten_expr env (UnaryOp op :: acc) depth e
  | LambdaExpr (_, params, captures, body) ->
     let capturec = Array.length captures in
     let env' = { locals = capturec; args = params } in
     let body' = add_block (CopyLocals env' :: flatten_expr env' [Return env'] capturec body) in
     let acc' = AllocHeap (1 + capturec, Func (capturec, body')) :: acc in
     let depth_acc = (depth + capturec - 1, acc') in
     captures |> Array.to_list |> List.rev |> List.fold_left (flatten_exprs env) depth_acc |> snd
  | CallExpr (_, callee, args) ->
     let return = add_block acc in
     let argc = Array.length args in
     let call' = Call (argc, return) in
     let depth_acc = (depth + argc, [call']) in
     let acc' =
       args
       |> Array.to_list
       |> List.rev
       |> List.fold_left (flatten_exprs env) depth_acc
       |> snd in
     flatten_expr env acc' depth callee
  | ConstructorExpr (_, variant, params) ->
     let paramc = Array.length params in
     let acc' = AllocHeap (1 + paramc, Enum(variant, paramc)) :: acc in
     let depth_acc = (depth + paramc - 1, acc') in
     params |> Array.to_list |> List.rev |> List.fold_left (flatten_exprs env) depth_acc |> snd
and flatten_exprs env (depth, acc) expr = depth - 1, flatten_expr env acc depth expr

(* For patterns, depth doesn't include matched value. *)
let rec flatten_pattern next depth acc pattern = match pattern with
  | Variable (_, id) -> Bind (depth - id) :: acc
  | Enum (_, var, patterns) ->
     let depth_acc = (depth, acc) in
     let fail_block = Option.map (fun block -> add_block [Pop depth; Jump block]) next in
     let acc' = patterns |> List.rev |> List.fold_left (flatten_patterns next) depth_acc |> snd in
     PatternEnum (var, List.length patterns, fail_block) :: acc'
  | Ignore _ -> Pop 1 :: acc
  | Int (_, i) -> PatternInt (i, next) :: acc
  | Bool (_, b) -> PatternBool (b, next) :: acc
and flatten_patterns next (depth, acc) pattern = depth + 1, flatten_pattern next depth acc pattern

let rec flatten_stmt env acc stmt =
  let depth = env.locals in
  match stmt with
  | ReturnStmt (_, e) -> flatten_expr env (Return env :: acc) depth e
  | ExprStmt (_, e) -> flatten_expr env (Pop 1 :: acc) depth e
  | BindStmt (_, id, e) -> flatten_expr env (Bind (depth - id) :: acc) depth e
  | MatchStmt (_, e, cases) ->
     let continue = add_block acc in
     let first_case, _ = List.fold_left (flatten_case env) (None, continue) (List.rev cases) in
     flatten_expr env [Jump (Option.get first_case)] depth e
  | IfStmt (_, cond, tblock, fblock) ->
     let continue = [Jump (add_block acc)] in
     let tblock' = add_block (make_block env tblock continue) in
     let fblock' = add_block (make_block env fblock continue) in
     let acc' = [If (tblock', fblock')] in
     flatten_expr env acc' depth cond
  | WhileStmt (_, id, cond, lblock, eblock) ->
     let continue = add_block acc in
     reserve_loop id continue;
     let lblock' = add_block (make_block env lblock [Jump (get_loop id)]) in
     let eblock' = add_block (make_block env eblock [Jump continue]) in
     set_loop id (flatten_expr env [If (lblock', eblock')] depth cond);
     [Jump (get_loop id)]
  | ContinueStmt (_, id) -> Jump (get_loop id) :: acc
  | BreakStmt (_, id) -> Jump (get_continue id) :: acc
and make_block env stmts acc =
  List.fold_left (flatten_stmt env) acc (List.rev stmts)
and flatten_case env (next, continue) (_, pattern, block) =
  let block' = PatternEnd env.locals :: make_block env block [Jump continue] in
  let id = add_block (Case env.locals :: flatten_pattern next env.locals block' pattern) in
  Some id, continue

let flatten_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let _, params = FuncMap.find func.name builtins in
      let env = { locals = 0; args = params } in
      add_block [Builtin func.name; Return env]
    else
      failwith "built-in has no definition"
  else
    let env = { locals = func.num_locals; args = func.num_params } in
    let block = make_block env (Option.get func.body) [Push Unit; Return env] in
    add_block (AllocStack env.locals :: block)

(*** Evaluation ***)

let rec sublist base count list =
  if count = 0
  then []
  else (List.nth list base) :: (sublist (base + 1) (count - 1) list)

let unwrap base count heap =
  sublist base count heap
  |> List.map (fun v ->
         match !v with
         | Sized v' -> ref v'
         | _ -> failwith "runtime error: can't unwrap heap value")

let wrap data = List.map (fun v -> ref (Sized !v)) data

let rec drop n stack =
  if n = 0
  then stack
  else
    match stack with
    | [] -> failwith "not enough values to drop in stack"
    | _ :: rest -> drop (n - 1) rest

let split n stack = sublist 0 n stack, drop n stack

let rec assign base count stack =
  if count = 0
  then stack
  else
    match stack with
    | [] -> failwith "not enough values to assign in stack"
    | v :: rest ->
       (List.nth stack base) := !v;
       assign base (count - 1) rest

let pattern_fail fail values heap = match fail with
  | Some block -> get_block block, values, heap
  | None -> failwith "runtime error: cases not exhaustive"

let step directives values heap = match directives, values with
  | [], values -> [], values, heap

  | [Call (args, return)], _ ->
     (match !(List.nth values args) with
      | HeapIndex i ->
         (match !(List.nth heap i) with
          | Func (_, body) ->
             get_block body, ref (CodeIndex return) :: values, heap
          | _ -> failwith "runtime error: heap item not function")
      | _ -> failwith "runtime error: callee not on heap")
  | [Return env], value :: values' ->
     let return, values'' = match drop env.locals values' with
       | { contents = CodeIndex i } :: rest -> i, drop (env.args + 1) rest
       | _ -> failwith "runtime error: stack frame formatted wrong" in
     get_block return, value :: values'', heap

  | [If (tblock, _)], { contents = Bool true } :: values' -> get_block tblock, values', heap
  | [If (_, fblock)], { contents = Bool false } :: values' -> get_block fblock, values', heap

  | [Jump block], _ -> get_block block, values, heap

  | BinOp op :: rest, rhs :: lhs :: values' -> rest, ref (do_bin_op op !lhs !rhs) :: values', heap
  | UnaryOp op :: rest, v :: values' -> rest, ref (do_unary_op op !v) :: values', heap

  | PushFunc id :: rest, _ -> rest, ref (HeapIndex (!funcs).(id)) :: values, heap
  | PushStack pos :: rest, _ -> rest, ref !(List.nth values pos) :: values, heap
  | Push v :: rest, _ -> rest, ref v :: values, heap

  | AllocHeap (_, Func (locals, block)) :: rest, _ ->
     let out = List.length heap in
     let heap' = heap @ (ref (Func (locals, block)) :: wrap (sublist 0 locals values)) in
     rest, ref (HeapIndex out) :: (drop locals values), heap'
  | AllocHeap (_, Enum (variant, params)) :: rest, _ ->
     let out = List.length heap in
     let heap' = heap @ (ref (Enum (variant, params)) :: wrap (sublist 0 params values)) in
     rest, ref (HeapIndex out) :: (drop params values), heap'

  | Builtin name :: rest, _ -> rest, (fst (FuncMap.find name builtins)) values, heap
  | AllocStack locals :: rest, _ -> rest, List.init locals (fun _ -> ref Unit) @ values, heap
  | CopyLocals env :: rest, _ ->
     (match !(List.nth values (env.args + 1)) with
      | HeapIndex i -> rest, unwrap (i + 1) env.locals heap @ values, heap
      | _ -> failwith "runtime error: callee not on heap")

  | Pop n :: rest, _ -> rest, drop n values, heap
  | Bind n :: rest, _ -> rest, assign n 1 values, heap

  | Case depth :: rest, _ -> rest, sublist 0 (depth + 1) values @ values, heap
  | PatternEnum (pvar, paramc, fail) :: rest, { contents = HeapIndex i } :: values' ->
     (match !(List.nth heap i) with
      | Enum (evar, _) when pvar = evar ->
         rest, (unwrap (i + 1) paramc heap |> List.rev) @ values', heap
      | Enum _ -> pattern_fail fail values' heap
      | _ -> failwith "runtime error: enum match with not an enum")
  | PatternInt (i, _) :: rest, { contents = Int j } :: values' when i = j ->
     rest, values', heap
  | PatternInt (_, fail) :: _, { contents = Int _ } :: values' ->
     pattern_fail fail values' heap
  | PatternBool (b, _) :: rest, { contents = Bool b' } :: values' when b = b' ->
     rest, values', heap
  | PatternBool (_, fail) :: _, { contents = Bool _ } :: values' ->
     pattern_fail fail values' heap
  | PatternEnd depth :: rest, _ ->
     rest, values |> assign (depth + 1) depth |> drop 1, heap

  | _, _ -> failwith "runtime error: invalid operation"

let stage = ref 0

let driver debug stack directives heap =
  let rec iter (directives, values, heap) =
    if debug
    then
      (let ppf = Format.std_formatter in
       incr stage;
       Format.fprintf ppf "@[<4>Step %d:@ " !stage;
       pp_directive_list ppf directives;
       list_sep ppf ();
       pp_stack_value_list ppf values;
       list_sep ppf ();
       pp_heap_value_list ppf heap;
       Format.fprintf ppf "@]";
       Format.pp_print_newline ppf ();
       if List.length directives = 1 then
         (Format.pp_print_string ppf "*****";
          Format.pp_print_newline ppf ()));
    match directives with
    | [] -> values
    | _ -> iter (step directives values heap) in
  iter (directives, stack, heap)

let heapify_func heap block =
  let out = List.length !heap in
  let values = [ref (Func (0, block))] in
  heap := !heap @ values;
  out

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  let raw_funcs = Array.map flatten_func program' in
  let heap = ref [] in
  funcs := Array.map (heapify_func heap) raw_funcs;
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let main_ptr = (!funcs).(main.id) in
  let stack = [ref (CodeIndex 0); ref (HeapIndex main_ptr)] in
  match (raw_funcs.(main.id) |> get_block |> driver debug stack) !heap with
  | [{ contents = Unit }] -> ()
  | _ -> failwith "runtime error: didn't return unit"
