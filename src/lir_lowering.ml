(* Convert IR to a lower IR. *)

open Lir
open Ir

let from_stack (source, offset, down) = FromOffset (source, if down then (-offset) else offset)
let to_stack (dest, offset, down) = ToOffset (dest, if down then (-offset) else offset)
let step (register, offset, down) = (register, offset + 1, down)
let load_heap register offset = (register, offset, false)
let reverse (register, offset, down) = (register, offset, not down)

let load stack_ref register acc =
  let stack = !stack_ref in
  stack_ref := step stack;
  Load (from_stack stack, register) :: acc

let assign stack_ref register acc =
  let stack = !stack_ref in
  stack_ref := step stack;
  Store (register, to_stack stack) :: acc

let load_stack offset register acc = Load (FromOffset (StackPointer, offset), register) :: acc
let store_stack offset register acc = Store (register, ToOffset (StackPointer, offset)) :: acc
let pop_stack register acc = StackPop (ToRegister register) :: acc
let push_stack register acc = StackPush (FromRegister register) :: acc

let chain register producer consumer acc = acc |> consumer register |> producer register
let rec repeat register producer consumer count acc =
  if count = 0 then acc
  else repeat register producer consumer (count - 1) (chain register producer consumer acc)

let shift (register, _, down) n acc =
  if n = 0
  then acc
  else
    let count = 8 * (if down then -n else n) in
    if count < 0
    then BinFrom (Ops.Sub, Constant (-count), register) :: acc
    else if count > 0
    then BinFrom (Ops.Add, Constant count, register) :: acc
    else acc

(*** Compilation ***)

let push_heap register value acc = match value with
  | Func (locals, block) ->
     let heap = ref (load_heap register 2) in
     StoreConst (locals, ToOffset (register, 0))
     :: Address (Block block, Temporary)
     :: Store (Temporary, ToOffset (register, 1))
     :: repeat Temporary pop_stack (assign heap) locals acc
  | Enum (variant, params) ->
     let heap = ref (load_heap register 2) in
     StoreConst (variant, ToOffset (register, 0))
     :: StoreConst (params, ToOffset (register, 1))
     :: repeat Temporary pop_stack (assign heap) params acc
  | _ -> failwith "cannot allocate to heap"

let lower_inst debug acc inst =
  let stack = (StackPointer, 0, true) in
  let acc' =
    match inst with
    | BinOp op -> Binary op :: acc
    | UnaryOp op -> Unary op :: acc
    | PushFunc fp -> Address (Function fp, Temporary) :: StackPush (FromRegister Temporary) :: acc
    | PushStack sp -> StackPush (FromOffset (StackPointer, sp)) :: acc
    | Push Unit -> StackPush (Constant 0) :: acc
    | Push (Bool true) -> StackPush (Constant 1) :: acc
    | Push (Bool false) -> StackPush (Constant 0) :: acc
    | Push (Int i) -> StackPush (Constant i) :: acc
    | Push _ -> failwith "invalid push"
    | AllocHeap (stacked, value) ->
       Call (LambdaAlloc stacked)
       :: push_heap Persistent value
          (StackPush (FromRegister Persistent) :: acc)
    | Builtin name ->
       Call (LambdaBuiltin name) :: Store (Persistent, (ToRegister StackPointer)) :: acc
    | Call (args, return) ->
       Load (FromOffset (StackPointer, args), Persistent)
       :: Address (Block return, Temporary)
       :: StackPush (FromRegister Temporary)
       :: Jump (OfOffset (Persistent, 1))
       :: acc
    | AllocStack locals -> shift stack locals acc
    | CopyLocals (locals, args) ->
       let heap = ref (reverse (load_heap Persistent (-locals - 1))) in
       Load (FromOffset (StackPointer, args + 1), Persistent)
       :: repeat Temporary (load heap) push_stack locals acc
    | Return (locals, args) ->
       StackPop (ToRegister Temporary)
       :: shift stack (-locals)
            (StackPop (ToRegister Persistent)
             :: shift stack (-args)
                  (Store (Temporary, ToOffset (StackPointer, 0))
                   :: Jump (OfRegister Persistent)
                   :: acc))
    | Pop n -> shift stack (-n) acc
    | Bind index -> StackPop (ToOffset (StackPointer, index - 1)) :: acc
    | Case depth ->
       repeat Temporary (load_stack depth) push_stack (depth + 1) acc
    | PatternEnum (_, paramc, None) ->
       let heap = ref (load_heap Persistent 2) in
       StackPop (ToRegister Persistent) :: repeat Temporary (load heap) push_stack paramc acc
    | PatternEnum (variant, paramc, Some block) ->
       let heap = ref (load_heap Persistent 2) in
       StackPop (ToRegister Persistent)
       :: Compare (variant, FromOffset (Persistent, 0))
       :: JumpCond (false, OfLabel (Block block))
       :: repeat Temporary (load heap) push_stack paramc acc
    | PatternInt (_, None) -> shift stack (-1) acc
    | PatternInt (i, Some block) ->
       StackPop (ToRegister Temporary)
       :: Compare (i, FromRegister Temporary)
       :: JumpCond (false, OfLabel (Block block))
       :: acc
    | PatternBool (_, None) -> shift stack (-1) acc
    | PatternBool (b, Some block) ->
       StackPop (ToRegister Temporary)
       :: Compare ((if b then 1 else 0), FromRegister Temporary)
       :: JumpCond (false, OfLabel (Block block))
       :: acc
    | PatternEnd depth ->
       repeat Temporary pop_stack (store_stack depth) depth (shift stack (-1) acc)
    | If (tblock, fblock) ->
       StackPop (ToRegister Temporary)
       :: Compare (1, FromRegister Temporary)
       :: JumpCond (true, OfLabel (Block tblock))
       :: JumpCond (false, OfLabel (Block fblock))
       :: acc
    | Jump block -> Jump (OfLabel (Block block)) :: acc in
  if debug
  then Debug (string_of_inst inst) :: acc'
  else acc'

let lower_block debug acc (id, block) =
  let acc =
    block
    |> List.rev
    |> List.fold_left (lower_inst debug) acc in
  Text :: Label (Block id) :: acc

let lower_function acc (id, name, block) =
  Data
  :: Export (Local name)
  :: Label (Local name)
  :: Label (Function id)
  :: Blob (Int 0)
  :: Blob (Label (Block block))
  :: acc

let lower debug program =
  let acc = [ Text
            ; Export (Global "lambda_main")
            ; Label (Global "lambda_main")
            ; StackPush (FromRegister HeapPointer)
            ; Load (FromRegister GivenHeap, HeapPointer)
            ; Address (Function program.main, Persistent)
            ; StackPush (FromRegister Persistent)
            ; Address (Global "lambda_end", Temporary)
            ; StackPush (FromRegister Temporary)
            ; Jump (OfOffset (Persistent, 1))
            ; Text
            ; Label (Global "lambda_end")
            ; StackPop (ToRegister Persistent)
            ; StackPop (ToRegister HeapPointer)
            ; End
            ] in
  let acc' =
    program.blocks
    |> Array.mapi (fun i b -> i, b)
    |> Array.fold_left (lower_block debug) acc in
  program.funcs
  |> Array.to_list
  |> List.mapi (fun i (name, block) -> (i, name, block))
  |> List.fold_left lower_function acc'
