open Lir
(* open Ops *)

let write_register r = match r with
  | StackPointer -> "%rsp"
  | HeapPointer -> "%r12"
  | Temporary -> "%rcx"
  | Persistent -> "%rax"
  | GivenHeap -> "%rdi"

let write_offset r offset =
  let register = write_register r in
  if offset = 0
  then Printf.sprintf "(%s)" register
  else Printf.sprintf "%d(%s)" (8 * offset) register

let write_source source = match source with
  | Constant i -> Printf.sprintf "$%d" i
  | FromRegister r -> write_register r
  | FromOffset (r, offset) -> write_offset r offset

let write_dest dest = match dest with
  | ToRegister r -> write_register r
  | ToOffset (r, offset) -> write_offset r offset

let write_label label = match label with
  | Function fp -> Printf.sprintf "_function_%d" fp
  | Block block -> Printf.sprintf "_block_%d" block
  | Local name -> Printf.sprintf "_lambda_%s" name
  | Global name -> name

let write_address address = match address with
  | OfLabel l -> write_label l
  | OfRegister r -> Printf.sprintf "*%s" (write_register r)
  | OfOffset (r, offset) -> Printf.sprintf "*%s" (write_offset r offset)

let write_blob blob = match blob with
  | Int i -> string_of_int i
  | Label l -> write_label l

let write_inst debug out inst = match inst with
  | Binary Add ->
     output_string out "\tpopq %rcx\n";
     output_string out "\taddq %rcx, (%rsp)\n"
  | Binary Sub ->
     output_string out "\tpopq %rcx\n";
     output_string out "\tsubq %rcx, (%rsp)\n"
  | Binary And ->
     output_string out "\tpopq %rcx\n";
     output_string out "\tandq %rcx, (%rsp)\n"
  | Binary Or ->
     output_string out "\tpopq %rcx\n";
     output_string out "\torq %rcx, (%rsp)\n"
  | Binary Equal ->
     output_string out "\tpopq %rcx\n";
     output_string out "\txorq %rax, %rax\n";
     output_string out "\tcmpq %rcx, (%rsp)\n";
     output_string out "\tsete %al\n";
     output_string out "\tmovq %rax, (%rsp)\n"
  | Unary Invert -> output_string out "\txorq $1, (%rsp)\n"
  | StackPush s -> Printf.fprintf out "\tpushq %s\n" (write_source s)
  | StackPop d -> Printf.fprintf out "\tpopq %s\n" (write_dest d)
  | Store (r, d) -> Printf.fprintf out "\tmovq %s, %s\n" (write_register r) (write_dest d)
  | StoreConst (i, d) -> Printf.fprintf out "\tmovq $%d, %s\n" i (write_dest d)
  | Load (s, r) -> Printf.fprintf out "\tmovq %s, %s\n" (write_source s) (write_register r)
  | Address (l, r) -> Printf.fprintf out "\tleaq %s(%%rip), %s\n" (write_label l) (write_register r)
  | Jump a -> Printf.fprintf out "\tjmp %s\n" (write_address a)
  | Compare (i, s) -> Printf.fprintf out "\tcmpq $%d, %s\n" i (write_source s)
  | JumpCond (true, a) -> Printf.fprintf out "\tje %s\n" (write_address a)
  | JumpCond (false, a) -> Printf.fprintf out "\tjne %s\n" (write_address a)
  | Call funct ->
     output_string out "\tmovq %rsp, %rbx\n";
     output_string out "\tsubq $64, %rsp\n";
     output_string out "\tandq $-16, %rsp\n";
     output_string out "\tmovq %r12, %rdi\n";
     (match funct with
      | LambdaAlloc size ->
         Printf.fprintf out "\tmovq $%d, %%rsi\n" size;
         output_string out "\tcallq lambda_alloc\n";
         output_string out "\tmovq %rbx, %rsp\n"
      | LambdaBuiltin name ->
         output_string out "\tmovq %rbx, %rsi\n";
         Printf.fprintf out "\tcallq %s\n" name)
  | BinFrom (Add, s, r) -> Printf.fprintf out "\taddq %s, %s\n" (write_source s) (write_register r)
  | BinFrom (Sub, s, r) -> Printf.fprintf out "\tsubq %s, %s\n" (write_source s) (write_register r)
  | BinFrom _ -> failwith "fixme: implement other operations"
  | Text -> output_string out ".text\n"
  | Data -> output_string out ".data\n"
  | Export l -> Printf.fprintf out ".global %s\n" (write_label l)
  | Label l -> Printf.fprintf out "%s:\n" (write_label l)
  | Blob b -> Printf.fprintf out "\t.quad %s\n" (write_blob b)
  | Debug s -> if debug then Printf.fprintf out "\t # %s\n" s
  | End -> Printf.fprintf out "\tret\n"

let compile debug out insts = List.iter (write_inst debug out) insts
