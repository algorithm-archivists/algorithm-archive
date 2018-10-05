.intel_syntax noprefix

.section .rodata
  fmt:  .string "%d\n"

.section .text
  .global main
  .extern printf

euclid_mod:
  # Abs of the first argument
  mov    rax, rdi
  sar    rax, 31
  xor    rdi, rax
  sub    rdi, rax

  # Abs of the second argument
  mov    rax, rsi
  sar    rax, 31
  xor    rsi, rax
  sub    rsi, rax

  # While loop
  jmp    mod_check
mod_loop:
  xor    rdx, rdx
  mov    rax, rdi
  div    rsi
  mov    rdi, rsi
  mov    rsi, rdx
mod_check:
  cmp    rsi, 0
  jne    mod_loop

  mov    rax, rdi
  ret

euclid_sub:
  # Abs of the first argument
  mov    rax, rdi
  sar    rax, 31
  xor    rdi, rax
  sub    rdi, rax

  # Abs of the second argument
  mov    rax, rsi
  sar    rax, 31
  xor    rsi, rax
  sub    rsi, rax

  # While loop
  jmp    check
loop:
  cmp    rdi, rsi
  jle    if_true

  sub    rdi, rsi
  jmp    check
if_true:
  sub    rsi, rdi
check:
  cmp    rsi, rdi
  jne    loop

  mov    rax, rdi
  ret

main:
  # Calling euclid_mod
  mov    rdi, 4288
  mov    rsi, 5184
  call   euclid_mod

  # Printing euclid_mod output
  mov    rdi, OFFSET fmt
  mov    rsi, rax
  xor    rax, rax
  call   printf

  # Calling euclid_sub
  mov    rdi, 1536
  mov    rsi, 9856
  call   euclid_sub

  # Printing euclid_sub output
  mov    rdi, OFFSET fmt
  mov    rsi, rax
  xor    rax, rax
  call   printf

  xor    rax, rax
  ret
