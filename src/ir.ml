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

(* Instructions to execute *)
type inst
  (* Binary operation. Pops two values (rhs then lhs) and pushes result. *)
  = BinOp of Ops.bin_op
  (* Unary operation. Pops one value and pushes result. *)
  | UnaryOp of Ops.unary_op
  (* Allocate space on the heap, push value to heap, then shift items from stack top.
   * Pushes heap pointer *)
  | AllocHeap of int * heap_value
  (* Allocate space on the stack. *)
  | AllocStack of int
  (* Copy local variables from the heap onto the stack.
   * Parameters are number of locals then number of args. *)
  | CopyLocals of int * int
  (* Pushes a function pointer to the stack. *)
  | PushFunc of function_pointer
  (* Pushes a value from lower on the stack to the top. The value is copied. *)
  | PushStack of stack_pointer
  (* Pushes a constant onto the stack.
   * Pointer valid before push. *)
  | Push of stack_value
  (* Pops top of stack and assigns local to it.
   * Pointer valid before pop. *)
  | Bind of stack_pointer
  (* Pops a value from the stack. *)
  | Pop
  (* Calls a builtin function. *)
  | Builtin of string
  (* Calls a lambda function. The top items are arguments.
     Pushes return location then local variables and finally jumps to function. *)
  | Call of int * block
  (* Return from a function. Parameters are number of locals then number of args *)
  | Return of int * int
  (* Jump to a block. *)
  | Jump of block
  (* Pop value from the stack. Jump to first if true, and second if false. *)
  | If of block * block

module BlockMap = Map.Make(Int)

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

let rec pp_inst ppf inst = match inst with
  | BinOp op -> Format.fprintf ppf "BinOp(%s)" (Ops.string_of_bin_op op)
  | UnaryOp op -> Format.fprintf ppf "UnaryOp(%s)" (Ops.string_of_unary_op op)
  | AllocHeap (i, v) ->
     Format.fprintf ppf "@[<4>AllocHeap(%d,@ " i;
     pp_heap_value ppf v;
     Format.fprintf ppf ")@]"
  | AllocStack locals -> Format.fprintf ppf "AllocStack(%d)" locals
  | CopyLocals (locals, args) -> Format.fprintf ppf "CopyLocals(%d, %d)" locals args
  | PushFunc i -> Format.fprintf ppf "PushFunc(%d)" i
  | PushStack i -> Format.fprintf ppf "PushStack(%d)" i
  | Push v ->
     Format.fprintf ppf "@[<4>Push(";
     pp_stack_value ppf v;
     Format.fprintf ppf ")@]"
  | Pop -> Format.fprintf ppf "Pop"
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call (args, return) -> Format.fprintf ppf "Call(%d, %d)" args return
  | Return (locals, args) -> Format.fprintf ppf "Return(%d, %d)" locals args
  | Jump block -> Format.fprintf ppf "Jump(%d)" block
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "If(%d, %d)" tblock fblock
and pp_inst_list ppf insts =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_inst ppf insts;
  Format.fprintf ppf ")@]"

(* Block map, function map and main function id *)
type program =
  { blocks: inst list array
  ; funcs: (string * block) array
  ; main: function_pointer
  }
