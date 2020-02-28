type register =
  | StackPointer
  | HeapPointer
  | Temporary
  | Persistent
  | GivenHeap

type source =
  | Constant of int
  | FromRegister of register
  | FromOffset of register * int

type dest =
  | ToRegister of register
  | ToOffset of register * int

type label =
  | Function of Ir.function_pointer
  | Block of Ir.block
  | Local of string
  | Global of string

type address =
  | OfLabel of label
  | OfRegister of register
  | OfOffset of register * int

type funct =
  | LambdaAlloc of int
  | LambdaBuiltin of string

type blob =
  | Int of int
  | Label of label

type instruction =
  | Binary of Ops.bin_op
  | Unary of Ops.unary_op
  | StackPush of source
  | StackPop of dest
  | Store of register * dest
  | StoreConst of int * dest
  | Load of source * register
  | Address of label * register
  | Jump of address
  | Compare of int * source
  | JumpCond of bool * address
  | Call of funct
  | BinFrom of Ops.bin_op * source * register
  | Text
  | Data
  | Export of label
  | Label of label
  | Blob of blob
  | Debug of string
  | End

let string_of_register register = match register with
  | StackPointer -> "sp"
  | HeapPointer -> "hp"
  | Temporary -> "tmp"
  | Persistent -> "pst"
  | GivenHeap -> "given_heap"

let string_of_source source = match source with
  | Constant i -> Printf.sprintf "%d" i
  | FromRegister r -> string_of_register r
  | FromOffset (r, offset) -> Printf.sprintf "%d(%s)" offset (string_of_register r)

let string_of_dest dest = match dest with
  | ToRegister r -> string_of_register r
  | ToOffset (r, offset) -> Printf.sprintf "%d(%s)" offset (string_of_register r)

let string_of_label label = match label with
  | Function fp -> Printf.sprintf "Function(%d)" fp
  | Block block -> Printf.sprintf "Block(%d)" block
  | Local s -> Printf.sprintf "Local(%s)" s
  | Global s -> Printf.sprintf "Global(%s)" s

let string_of_address address = match address with
  | OfLabel l -> string_of_label l
  | OfRegister r -> Printf.sprintf "*%s" (string_of_register r)
  | OfOffset (r, offset) -> Printf.sprintf "*%d(%s)" offset (string_of_register r)

let string_of_funct funct = match funct with
  | LambdaAlloc size -> Printf.sprintf "Alloc(%d)" size
  | LambdaBuiltin name -> Printf.sprintf "Builtin(%s)" name

let string_of_blob blob = match blob with
  | Int i -> string_of_int i
  | Label l -> string_of_label l

let string_of_inst inst = match inst with
  | Binary op -> Printf.sprintf "Binary(%s)" (Ops.string_of_bin_op op)
  | Unary op -> Printf.sprintf "Unary(%s)" (Ops.string_of_unary_op op)
  | StackPush s -> Printf.sprintf "StackPush(%s)" (string_of_source s)
  | StackPop d -> Printf.sprintf "StackPop(%s)" (string_of_dest d)
  | Store (r, d) -> Printf.sprintf "Store(%s, %s)" (string_of_register r) (string_of_dest d)
  | StoreConst (i, d) -> Printf.sprintf "StoreConst(%d, %s)" i (string_of_dest d)
  | Load (s, r) -> Printf.sprintf "Load(%s, %s)" (string_of_source s) (string_of_register r)
  | Address (l, r) -> Printf.sprintf "Address(%s, %s)" (string_of_label l) (string_of_register r)
  | Jump a -> Printf.sprintf "Jump(%s)" (string_of_address a)
  | Compare (i, s) -> Printf.sprintf "Compare(%d, %s)" i (string_of_source s)
  | JumpCond (b, a) -> Printf.sprintf "JumpCond(%B, %s)" b (string_of_address a)
  | Call f -> Printf.sprintf "Call(%s)" (string_of_funct f)
  | BinFrom (op, s, r) ->
     let op' = Ops.string_of_bin_op op in
     let s' = string_of_source s in
     let r' = string_of_register r in
     Printf.sprintf "BinFrom(%s, %s, %s)" op' s' r'
  | Text -> "Text"
  | Data -> "Data"
  | Export l -> Printf.sprintf "Export(%s)" (string_of_label l)
  | Label l -> Printf.sprintf "Label(%s)" (string_of_label l)
  | Blob b -> Printf.sprintf "Blob(%s)" (string_of_blob b)
  | End -> "End"
  | Debug _ -> "Debug"
