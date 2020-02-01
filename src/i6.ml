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
  (* #locals * block *)
  | Func of int * int

type directive
  (* Binary op of two values *)
  = BinOp of bin_op
  (* Unary of of a value *)
  | UnaryOp of unary_op
  (* Allocates space and pushes to heap. *)
  (* Parts are stack elements to move and heap value *)
  | AllocHeap of int * heap_value
  (* Pushes function *)
  | PushFunc of int
  (* Pushes stack value, relative to top *)
  | PushStack of int
  (* Pushes constant value *)
  | Push of stack_value
  (* Pops value *)
  | Pop
  (* Calls builtin function *)
  | Builtin of string
  (* Calls function: #args * return block *)
  | Call of int * int
  (* Jumps to address and cleans up env  *)
  | Return of env
  (* Jumps to constant block *)
  | Jump of int
  (* Binds local to value *)
  | Bind of int
  (* Conditional jump: true block * false block *)
  | If of int * int

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

let list_sep ppf _ = Format.fprintf ppf ",@ "

let pp_stack_value ppf value = match value with
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | CodeIndex i -> Format.fprintf ppf "@%d" i
  | HeapIndex i -> Format.fprintf ppf "*%d" i

let pp_stack_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_stack_value ppf (List.map (!) values);
  Format.fprintf ppf ")@]"

let pp_heap_value ppf value = match value with
  | Sized v -> pp_stack_value ppf v
  | Func (locals, body) -> Format.fprintf ppf "Func(%d, %d)" locals body

let pp_heap_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_heap_value ppf (List.map (!) values);
  Format.fprintf ppf ")@]"

let rec pp_directive ppf directive = match directive with
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (string_of_bin_op op)
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (string_of_unary_op op)
  | AllocHeap (i, v) ->
     Format.fprintf ppf "@[<4>AllocHeap(%d,@ " i;
     pp_heap_value ppf v;
     Format.fprintf ppf ")@]"
  | PushFunc i -> Format.fprintf ppf "PushFunc(%d)" i
  | PushStack i -> Format.fprintf ppf "PushStack(%d)" i
  | Push v ->
     Format.fprintf ppf "@[<4>Push(";
     pp_stack_value ppf v;
     Format.fprintf ppf ")@]"
  | Pop -> Format.fprintf ppf "Pop"
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call (args, return) -> Format.fprintf ppf "Call(%d, %d)" args return
  | Return env -> Format.fprintf ppf "Return(%d, %d)" env.locals env.args
  | Jump block -> Format.fprintf ppf "Jump(%d)" block
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "If(%d, %d)" tblock fblock
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"

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

let rec cleanup directives cnt = match directives with
  | [] -> cnt []
  | Call (args, block) :: _ -> cnt [Call (args, block)]
  | Return env :: _ -> cnt [Return env]
  | Jump block :: _ -> cnt [Jump block]
  | If (tblock, fblock) :: _ -> cnt [If (tblock, fblock)]
  | x :: rest -> cleanup rest (fun rest' -> cnt (x :: rest'))

let add_block, get_block, flatten_blocks =
  let block_id = ref 1 in
  let next_block () =
    let block = !block_id in
    incr block_id; block in
  let blocks = ref (BlockMap.add 0 [] BlockMap.empty) in
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
  eval_add, eval_get, eval_flattens

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
     let body' = add_block (flatten_expr env' [Return env'] capturec body) in
     let acc' = AllocHeap (capturec, Func (Array.length captures, body')) :: acc in
     let depth_acc = (depth + capturec - 1, acc') in
     snd (List.fold_left (flatten_exprs env) depth_acc (List.rev (Array.to_list captures)))
  | CallExpr (_, callee, args) ->
     let return = add_block acc in
     let argc = Array.length args in
     let call' = Call (argc, return) in
     let depth_acc = (depth + argc, [call']) in
     let acc' = List.fold_left (flatten_exprs env) depth_acc (List.rev (Array.to_list args)) in
     flatten_expr env (snd acc') depth callee
and flatten_exprs env (depth, acc) expr = depth - 1, flatten_expr env acc depth expr

let rec flatten_stmt env acc stmt =
  let depth = env.locals in
  match stmt with
  | ReturnStmt (_, e) -> flatten_expr env (Return env :: acc) depth e
  | ExprStmt (_, e) -> flatten_expr env (Pop :: acc) depth e
  | BindStmt (_, id, e) -> flatten_expr env (Bind (depth - id) :: acc) depth e
  | IfStmt (_, cond, tblock, fblock) ->
     let continue = [Jump (add_block acc)] in
     let acc' = [If (make_block env tblock continue, make_block env fblock continue)] in
     flatten_expr env acc' depth cond
and make_block env stmts acc =
  add_block (List.fold_left (flatten_stmt env) acc (List.rev stmts))

let rec sublist base count list =
  if count = 0
  then []
  else (List.nth list base) :: (sublist (base + 1) (count - 1) list)

let unwrap base count heap =
  List.map (fun v ->
      match !v with
      | Sized v' -> ref v'
      | _ -> failwith "runtime error: can't unwrap heap value") (sublist base count heap)

let wrap data = List.map (fun v -> ref (Sized !v)) data

let split n stack =
  let rec iter n stack cnt = match n, stack with
    | 0, _ -> cnt [] stack
    | _, v :: rest -> iter (n - 1) rest (fun acc rest' -> cnt (v :: acc) rest')
    | _, _ -> failwith "runtime error: not enough stack to split"
  in iter n stack (fun acc rest -> acc, rest)

let drop n stack = snd (split n stack)

let step directives values heap = match directives, values with
  | [], values -> [], values, heap

  | [Call (args, return)], _ ->
     (match !(List.nth values args) with
      | HeapIndex i ->
      (match !(List.nth heap i) with
       | Func (locals, body) ->
          let locals' = unwrap (i + 1) locals heap in
          get_block body, locals' @ (ref (CodeIndex return) :: values), heap
       | _ -> failwith "runtime error: heap item not function")
      | _ -> failwith "runtime error: callee not on heap")
  | [Return env], value :: values' ->
     let return, values'' = match drop env.locals values' with
       | { contents = CodeIndex i } :: rest -> i, drop (env.args + 1) rest
       | _ -> failwith "runtime error: stack frame formatted wrong" in
     get_block return, value :: values'', heap
  | [Jump block], _ -> get_block block, values, heap

  | [If (tblock, _)], { contents = Bool true } :: values' -> get_block tblock, values', heap
  | [If (_, fblock)], { contents = Bool false } :: values' -> get_block fblock, values', heap

  | BinOp op :: rest, rhs :: lhs :: values' -> rest, ref (do_bin_op op !lhs !rhs) :: values', heap
  | UnaryOp op :: rest, v :: values' -> rest, ref (do_unary_op op !v) :: values', heap

  | AllocHeap (ncopy, value) :: rest, _ ->
     let out = List.length heap in
     let data, values' = split ncopy values in
     rest, ref (HeapIndex out) :: values', heap @ (ref value :: wrap data)

  | PushFunc id :: rest, _ ->
     rest, ref (HeapIndex (Array.get !funcs id)) :: values, heap
  | PushStack pos :: rest, _ -> rest, List.nth values pos :: values, heap
  | Push v :: rest, _ -> rest, ref v :: values, heap
  | Pop :: rest, _ :: values' -> rest, values', heap

  | Builtin name :: rest, _ -> rest, (fst (FuncMap.find name builtins)) values, heap

  | Bind n :: rest, v :: values' ->
     (List.nth values' (n - 1)) := !v; rest, values', heap
  | _, _ -> failwith "runtime error: invalid operation"

let stage = ref 0

let driver debug stack directives heap =
  let rec iter (directives, values, heap) =
    if debug then begin
        incr stage;
        Format.fprintf Format.std_formatter "@[<4>Step %d: (@," !stage;
        pp_directive_list Format.std_formatter directives;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_stack_value_list Format.std_formatter values;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_heap_value_list Format.std_formatter heap;
        Format.fprintf Format.std_formatter ")@]";
        Format.pp_print_newline Format.std_formatter ();
      end;
    match directives with
    | [] -> values
    | _ -> iter (step directives values heap) in
  iter (directives, stack, heap)


let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let _, params = FuncMap.find func.name builtins in
      let env = { locals = 0; args = params } in
      0, add_block [Builtin func.name; Return env]
    else
      failwith "built-in has no definition"
  else
    let env = { locals = func.num_locals; args = func.num_params } in
    let block = make_block env (Option.get func.body) [Push Unit; Return env] in
    func.num_locals, block

let heapify_func heap (locals, block) =
  let out = List.length !heap in
  let values = ref (Func (locals, block)) :: (List.init locals (fun _ -> ref (Sized Unit))) in
  heap := !heap @ values;
  out

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  let raw_funcs = Array.map interpret_func program' in
  let heap = ref [] in
  funcs := Array.map (heapify_func heap) raw_funcs;
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let locals, body =  Array.get raw_funcs main.id in
  let stack = List.init locals (fun _ -> ref Unit) @ [ref (CodeIndex 0); ref Unit] in
  (match driver debug stack (get_block body) !heap with
   | [{ contents = Unit }] -> ()
   | _ -> failwith "type mismatch")
