################################################################################
# Compiler Construction - Minimal Lambda Language
################################################################################


################################################################################
# main
################################################################################
  .text
  .global main
main:
  .global _lambda_main
  jmp _lambda_main


################################################################################
# __builtin_input_int
################################################################################
  .text
  .global __builtin_input_int
__builtin_input_int:
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary and allocate some space.
  movq %rsp, %rbx
  subq $32, %rsp
  andq $-16, %rsp

  # Invoke scanf.
  leaq  scanf_format(%rip), %rdi
  movq  %rsp, %rsi
  callq scanf

  # Load the result.
  movq (%rsp), %rcx

  # Restore the stack.
  movq %rbx, %rsp

  popq %rax
  popq %rbp
  retq

  .data
scanf_format:
  .string "%d"

  .data
  .p2align 2
  .global __builtin_input_int_closure
  .quad 0
__builtin_input_int_closure:
  .quad __builtin_input_int



################################################################################
# __builtin_print_int
################################################################################
  .text
  .global __builtin_print_int
__builtin_print_int:
  # Save previous frame.
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary.
  movq %rsp, %rbx
  subq $16, %rsp
  andq $-16, %rsp

  # Invoke printf.
  leaq  printf_format_int(%rip), %rdi
  movq  16(%rbp), %rsi
  callq printf
  xorq  %rcx, %rcx

  # Restore previous frame.
  movq %rbx, %rsp
  popq %rax
  popq %rbp

  # Pop the argument to the function.
  popq %rdx
  addq $8, %rsp
  pushq %rdx
  retq

  .data
printf_format_int:
  .string "%d\n"

  .data
  .global __builtin_print_int_closure
  .quad 0
__builtin_print_int_closure:
  .quad __builtin_print_int



################################################################################
# __builtin_print_bool
################################################################################
  .text
  .global __builtin_print_bool
__builtin_print_bool:
  # Save previous frame.
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary.
  movq %rsp, %rbx
  subq $16, %rsp
  andq $-16, %rsp

  # Test if true
  cmpq $1, 16(%rbp)
  jne __builtin_print_bool_false

  # Invoke puts for true.
  leaq  puts_true(%rip), %rdi
  callq puts
  xorq  %rcx, %rcx
  jmp __builtin_print_bool_end

__builtin_print_bool_false:
  # Invoke puts for false.
  leaq  puts_false(%rip), %rdi
  callq puts
  xorq  %rcx, %rcx
  jmp __builtin_print_bool_end

__builtin_print_bool_end:
  # Restore previous frame.
  movq %rbx, %rsp
  popq %rax
  popq %rbp

  # Pop the argument to the function.
  popq %rdx
  addq $8, %rsp
  pushq %rdx
  retq

  .data
puts_true:
  .string "true"

  .data
puts_false:
  .string "false"

  .data
  .global __builtin_print_bool_closure
  .quad 0
__builtin_print_bool_closure:
  .quad __builtin_print_bool



################################################################################
# __builtin_allocate
################################################################################
  .text
  .global __builtin_allocate
__builtin_allocate:
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary.
  movq %rsp, %rbx
  subq $16, %rsp
  andq $-16, %rsp

  # Compute the size of the block: 8 * fields + header + code pointer
  leaq 1(%rcx), %r12
  leaq 16(,%rcx, 8), %rdi
  # Invoke malloc.
  callq malloc
  # Store header.
  movq %r12, (%rax)
  # Return pointer in %rcx.
  leaq 8(%rax), %rcx

  # Restore stack.
  movq %rbx, %rsp

  popq %rax
  popq %rbp
        retq
