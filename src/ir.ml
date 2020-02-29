(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the intermediate representation obtained
 * from the typed AST. It targets a stack machine, with instructions
 * documented individually below. At the highest level, the program
 * consists of a number of closures: toplevel functions themselves
 * are lowered to closures which do not capture anything.
 *
 * Some helper methods to dump the IR are also included.
 *)

type block = int
type stack_pointer = int
type heap_pointer = int
type function_pointer = int

(* Values that occupy the stack *)
type stack_value
  (* Unit value *)
  = Unit
  (* Boolean value *)
  | Bool of bool
  (* Integer value *)
  | Int of int
  (* Code pointer *)
  | CodeIndex of block
  (* Heap pointer *)
  | HeapIndex of heap_pointer

(* Values that occupy the heap *)
type heap_value
  (* Wrapper for stack values *)
  = Sized of stack_value
  (* Function with locals and body *)
  | Func of int * block
  (* Enum with variant and parameters *)
  | Enum of int * int

(* Instructions to execute *)
type inst
  (* Binary operation. Pops two values (rhs then lhs) and pushes result. *)
  = BinOp of Ops.bin_op
  (* Unary operation. Pops one value and pushes result. *)
  | UnaryOp of Ops.unary_op
  (* Pushes a function pointer to the stack. *)
  | PushFunc of function_pointer
  (* Pushes a value from lower on the stack to the top. The value is copied. *)
  | PushStack of stack_pointer
  (* Pushes a constant onto the stack.
   * Pointer valid before push. *)
  | Push of stack_value
  (* Allocate space on the heap, push value to heap, then shift items from stack top.
   * Pushes heap pointer *)
  | AllocHeap of int * heap_value
  (* Calls a builtin function. *)
  | Builtin of string
  (* Calls a lambda function. The top items are arguments.
   * Pushes return location then local variables and finally jumps to function. *)
  | Call of int * block
  (* Allocate space on the stack. *)
  | AllocStack of int
  (* Copy local variables from the heap onto the stack.
   * Parameters are number of locals then number of args. *)
  | CopyLocals of int * int
  (* Return from a function. Parameters are number of locals then number of args *)
  | Return of int * int
  (* Pops values from the stack. *)
  | Pop of int
  (* Pops top of stack and assigns local to it.
   * Pointer valid before pop. *)
  | Bind of stack_pointer
  (* Creates environment for pattern matching.
   * Parameter is number of locals. *)
  | Case of int
  (* Pattern match an enum.
   * Parameters are enum variant, parameter count and optional fail branch. *)
  | PatternEnum of int * int * block option
  (* Pattern match an int.
   * Parameters are int value and optional fail branch. *)
  | PatternInt of int * block option
  (* Pattern match a bool.
   * Parameters are bool value and optional fail branch. *)
  | PatternBool of bool * int option
  (* Indicates end of pattern matching. Parameter is number of locals. *)
  | PatternEnd of int
  (* Pop value from the stack. Jump to first if true, and second if false. *)
  | If of block * block
  (* Jump to a block. *)
  | Jump of block

module BlockMap = Map.Make(Int)

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

let pp_heap_value ppf value = match value with
  | Sized v -> pp_stack_value ppf v
  | Func (locals, body) -> Format.fprintf ppf "Func(%d, %d)" locals body
  | Enum (variant, params) -> Format.fprintf ppf "Enum(%d, %d)" variant params

let rec pp_inst ppf inst = match inst with
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (Ops.string_of_bin_op op)
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (Ops.string_of_unary_op op)
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
  | CopyLocals (locals, args) -> Format.fprintf ppf "CopyLocals(%d, %d)" locals args
  | Return (locals, args) -> Format.fprintf ppf "Return(%d, %d)" locals args
  | Pop count -> Format.fprintf ppf "Pop(%d)" count
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | Case locals -> Format.fprintf ppf "Case(%d)" locals
  | PatternEnum (variant, params, None) -> Format.fprintf ppf "PatternEnum(%d, %d)" variant params
  | PatternEnum (variant, params, Some block) ->
     Format.fprintf ppf "PatternEnum(%d, %d, %d)" variant params block
  | PatternInt (i, None) -> Format.fprintf ppf "PatternInt(%d)" i
  | PatternInt (i, Some block) -> Format.fprintf ppf "PatternInt(%d, %d)" i block
  | PatternBool (b, None) -> Format.fprintf ppf "PatternBool(%B)" b
  | PatternBool (b, Some block) -> Format.fprintf ppf "PatternBool(%B, %d)" b block
  | PatternEnd depth -> Format.fprintf ppf "PatternEnd(%d)" depth
  | If (tblock, fblock) ->
     Format.fprintf ppf "If(%d, %d)" tblock fblock
  | Jump block -> Format.fprintf ppf "Jump(%d)" block
and pp_inst_list ppf insts =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_inst ppf insts;
  Format.fprintf ppf ")@]"

let string_of_inst inst =
  let ppf = Format.str_formatter in
  Format.pp_set_margin ppf 1000000000;
  pp_inst ppf inst;
  Format.flush_str_formatter ()

(* Block map, function map and main function id *)
type program =
  { blocks: inst list array
  ; funcs: (string * block) array
  ; main: function_pointer
  }
