(* Generate linux-unknown-x86_64 asm.
 *
 * Stack pointer saved in %rsp.
 * Heap pointer saved in %r12.
 * Functions are stored local count, return, locals.
 *)

open Ir
open Ops

let compile_block debug out id insts =
  Printf.fprintf out ".text\n";
  Printf.fprintf out "_block_%d:\n" id;
  List.iter (fun inst ->
      (if debug
       then
         let formatter = Format.str_formatter in
         Format.pp_set_margin formatter 76;
         pp_inst formatter inst;
         Format.flush_str_formatter ()
         |> String.split_on_char '\n'
         |> List.iter (Printf.fprintf out "  # %s\n"));
      match inst with
      | BinOp op ->
         Printf.fprintf out "\tpopq %%rcx # Pop rhs\n";
         (match op with
          | Add -> Printf.fprintf out "\taddq %%rcx, (%%rsp) # Add rhs to left\n"
          | Sub -> Printf.fprintf out "\tsubq %%rcx, (%%rsp) # Sub rhs from left\n"
          | And -> Printf.fprintf out "\tandq %%rcx, (%%rsp) # And rhs with left\n"
          | Or -> Printf.fprintf out "\torq %%rcx, (%%rsp) # Or rhs with left\n"
          | Equal ->
             Printf.fprintf out "\txorq %%rax, %%rax # Set rax to 0\n";
             Printf.fprintf out "\tcmpq %%rcx, (%%rsp) # Compare rhs with left\n";
             Printf.fprintf out "\tsete %%al # Set rax to 1 if equal\n";
             Printf.fprintf out "\tmovq %%rax, (%%rsp) # Push to stack\n")
      | UnaryOp Invert -> Printf.fprintf out "\txorq $1, (%%rsp) # Invert stack top\n"
      | AllocHeap (stacked, Func (locals, block)) when stacked = locals ->
         Printf.fprintf out "\tmovq %%rsp, %%rbx # Save stack pointer\n";
         Printf.fprintf out "\tsubq $64, %%rsp # Make space on stack\n";
         Printf.fprintf out "\tandq $-16, %%rsp # Align stack to 16-byte boundary\n";
         Printf.fprintf out "\tmovq %%r12, %%rdi # Set first arg to heap pointer\n";
         Printf.fprintf out "\tmovq $%d, %%rsi # Set second arg to space needed\n" (locals + 2);
         Printf.fprintf out "\tcallq lambda_alloc # Call the allocate function\n";
         Printf.fprintf out "\tmovq $%d, (%%rax) # Move local count to heap\n" locals;
         Printf.fprintf out "\tleaq _block_%d(%%rip), %%rcx # Save return to tempory\n" block;
         Printf.fprintf out "\tmovq %%rcx, 8(%%rax) # Move return address to heap\n";
         Printf.fprintf out "\tmovq %%rbx, %%rsp # Restore stack pointer\n";
         let rec capture n =
           if n = locals then ()
           else begin
               Printf.fprintf out "\tmovq %d(%%rsp), %%rcx # Move stack item to rcx\n" (8 * n);
               Printf.fprintf out "\tmovq %%rcx, %d(%%rax) # Move rcx to heap item\n" (8 * n + 16);
               capture (n + 1)
             end in
         capture 0;
         Printf.fprintf out "\taddq $%d, %%rsp # Readjust stack pointer\n" (8 * locals);
         Printf.fprintf out "\tpushq %%rax # Push heap position to stack\n"
      | AllocHeap _ -> failwith "invalid alloc heap"
      | CopyLocals (locals, args) ->
         Printf.fprintf out "\tmovq %d(%%rsp), %%rcx # Copy closure location\n" (8 * args + 8);
         let base = (8 * locals + 8) in
         let rec copy n =
           if n = locals then ()
           else begin
               Printf.fprintf out "\tpushq %d(%%rcx) # Push from heap to stack\n" (base - 8 * n);
               copy (n + 1)
             end in
         copy 0
      | PushFunc id ->
         Printf.fprintf out "\tleaq _function_%d(%%rip), %%rcx # Get address of function\n" id;
         Printf.fprintf out "\tpushq %%rcx # Push it onto stack\n"
      | PushStack id -> Printf.fprintf out "\tpushq %d(%%rsp) # Push value in stack\n" (8 * id)
      | Push Unit -> Printf.fprintf out "\tpushq $0 # Push unit\n"
      | Push (Bool true) -> Printf.fprintf out "\tpushq $1 # Push true\n"
      | Push (Bool false) -> Printf.fprintf out "\tpushq $0 # Push false\n"
      | Push (Int i) -> Printf.fprintf out "\tpushq $%d # Push int\n" i
      | Push _ -> failwith "invalid push"
      | Bind index ->
         Printf.fprintf out "\tpopq %%rcx # Pop value from stack\n";
         Printf.fprintf out "\tmovq %%rcx, %d(%%rsp) # Assign to local\n" (8 * index - 8)
      | Pop -> Printf.fprintf out "\tpopq %%rcx # Pop value from stack\n";
      | Builtin name ->
         Printf.fprintf out "\tmovq %%rsp, %%rbx # Save stack pointer\n";
         Printf.fprintf out "\tsubq $64, %%rsp # Make space on stack\n";
         Printf.fprintf out "\tandq $-16, %%rsp # Align stack to 16-byte boundary\n";
         Printf.fprintf out "\tmovq %%r12, %%rdi # Set first arg to heap pointer\n";
         Printf.fprintf out "\tmovq %%rbx, %%rsi # Set second arg to stack pointer\n";
         Printf.fprintf out "\tcallq %s # Call builtin function\n" name;
         Printf.fprintf out "\tmovq %%rax, %%rsp # Update stack pointer\n"
      | Call (args, return) ->
         Printf.fprintf out "\tmovq %d(%%rsp), %%rax # Copy closure location\n" (8 * args);
         Printf.fprintf out "\tleaq _block_%d(%%rip), %%rcx # Load return address\n" return;
         Printf.fprintf out "\tpushq %%rcx # Push return address\n";
         Printf.fprintf out "\tjmp *8(%%rax) # Jump to closure body\n"
      | Return (locals, args) ->
         Printf.fprintf out "\tpopq %%rcx # Pop return value\n";
         Printf.fprintf out "\taddq $%d, %%rsp # Pop locals\n" (8 * locals);
         Printf.fprintf out "\tpopq %%rax # Pop return address\n";
         Printf.fprintf out "\taddq $%d, %%rsp # Pop args and callee\n" (8 + 8 * args);
         Printf.fprintf out "\tpushq %%rcx # Push return value\n";
         Printf.fprintf out "\tjmp *%%rax # Jump to return address\n"
      | Jump block -> Printf.fprintf out "\tjmp _block_%d # Jump to block\n" block
      | If (tblock, fblock) ->
         Printf.fprintf out "\tpopq %%rcx # Pop off condition\n";
         Printf.fprintf out "\tcmpq $1, %%rcx # Check if true\n";
         Printf.fprintf out "\tje _block_%d # Jump for true\n" tblock;
         Printf.fprintf out "\tjne _block_%d # Jump for false\n" fblock
    ) insts

let compile_function _debug out id (locals, block) =
  Printf.fprintf out ".data\n";
  Printf.fprintf out "_function_%d:\n" id;
  Printf.fprintf out "\t.quad %d\n" locals;
  Printf.fprintf out "\t.quad _block_%d\n" block;

  let rec print_locals n =
    if n = 0 then ()
    else begin
        Printf.fprintf out "\t.quad 0\n";
        print_locals (n - 1) end in
  print_locals locals

let compile debug out program =
  Array.iteri (compile_block debug out) program.blocks;
  Array.iteri (compile_function debug out) program.funcs;
  Printf.fprintf out ".text\n";
  Printf.fprintf out ".global lambda_main\n";
  Printf.fprintf out "lambda_main:\n";
  Printf.fprintf out "\tpushq %%r12 # Callee saves\n";
  Printf.fprintf out "\tmovq %%rdi, %%r12 # Save heap location\n";
  Printf.fprintf out "\tleaq _function_%d(%%rip), %%rax # Get main function location\n" program.main;
  Printf.fprintf out "\tpushq %%rax # Push main function onto the stack\n";
  Printf.fprintf out "\tleaq _lambda_end(%%rip), %%rcx # Load return address\n";
  Printf.fprintf out "\tpushq %%rcx # Push return address\n";
  Printf.fprintf out "\tjmp *8(%%rax) # Jump to closure body\n";
  Printf.fprintf out ".text\n";
  Printf.fprintf out "_lambda_end:\n";
  Printf.fprintf out "\tpopq %%rax # Move return value into register\n";
  Printf.fprintf out "\tpopq %%r12 # Restore parent r12\n";
  Printf.fprintf out "\tret # Quit lambda land\n"
