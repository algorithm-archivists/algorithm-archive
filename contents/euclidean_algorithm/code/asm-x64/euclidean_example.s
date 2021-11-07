.intel_syntax noprefix

.section .rodata
  fmt:  .string "%d\n"

.section .text
  .global main
  .extern printf

# rdi - a
# rsi - b
# RET rax - gcd of a and b
euclid_mod:
  mov    rax, rdi           # Get abs of a
  sar    rax, 31
  xor    rdi, rax
  sub    rdi, rax
  mov    rax, rsi           # Get abs of b
  sar    rax, 31
  xor    rsi, rax
  sub    rsi, rax
  jmp    mod_check
mod_loop:
  xor    rdx, rdx           # Take the mod of a and b
  mov    rax, rdi
  div    rsi
  mov    rdi, rsi           # Set b to the mod of a and b
  mov    rsi, rdx           # Set a to b
mod_check:
  cmp    rsi, 0             # Check if b is non-zero
  jne    mod_loop
  mov    rax, rdi           # Return the result
  ret

euclid_sub:
  mov    rax, rdi           # Get abs of a
  sar    rax, 31
  xor    rdi, rax
  sub    rdi, rax
  mov    rax, rsi           # Get abs of b
  sar    rax, 31
  xor    rsi, rax
  sub    rsi, rax
  jmp    check
loop:
  cmp    rdi, rsi           # Find which is bigger
  jle    if_true
  sub    rdi, rsi           # If a is bigger then a -= b
  jmp    check
if_true:
  sub    rsi, rdi           # Else b -= a
check:
  cmp    rsi, rdi           # Check if a and b are not equal
  jne    loop
  mov    rax, rdi           # Return results
  ret

main:
  mov    rdi, 4288          # Call euclid_mod
  mov    rsi, 5184
  call   euclid_mod
  mov    rdi, OFFSET fmt    # Print output
  mov    rsi, rax
  xor    rax, rax
  call   printf
  mov    rdi, 1536          # Call euclid_sub
  mov    rsi, 9856
  call   euclid_sub
  mov    rdi, OFFSET fmt    # Print output
  mov    rsi, rax
  xor    rax, rax
  call   printf
  xor    rax, rax           # Return 0
  ret
