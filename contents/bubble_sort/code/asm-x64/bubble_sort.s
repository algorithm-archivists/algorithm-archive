.intel_syntax noprefix

.section .rodata
  array:
    .align  16
    .int    1, 45, 756, 4569, 56, 3, 8, 5, -10, -4
    .equ    array_len, (.-array) / 4
  array_fmt: .string "%d "
  lf:        .string "\n"
  unsorted:  .string "Unsorted array: "
  sorted:    .string "Sorted array: "

.section .text
  .global main
  .extern printf

print_array:
  push   r12
  push   r13

  mov    r12, rdi                 # Loop variable
  lea    r13, [rdi + 4*rsi]       # Pointer after the last element

print_array_loop:
  cmp    r12, r13                 # If we're done iterating over the array then bail
  jge    print_array_done
  mov    rdi, OFFSET array_fmt    # Otherwise print the current value
  mov    esi, DWORD PTR [r12]
  xor    rax, rax
  call   printf
  lea    r12, [r12 + 4]           # And increment the loop variable pointer
  jmp    print_array_loop

print_array_done:
  mov    rdi, OFFSET lf           # Print a closing newline
  xor    rax, rax
  call   printf

  pop    r13
  pop    r12
  ret

bubble_sort:
  xor    rcx, rcx                 # The outer loop counter
  lea    rdx, [rdi + 4*rsi - 4]   # The end address for the inner loop

outer_loop:
  cmp    rcx, rsi                 # We first check if the outer loop is done
  jge    bubble_sort_done         # And if it is, return
  mov    rax, rdi                 # Otherwise we initialize the loop variable of the inner loop
inner_loop:
  mov    r8d, DWORD PTR [rax]     # Load array[j] and array[j+1] through a pointer
  mov    r9d, DWORD PTR [rax + 4]
  cmp    r8d, r9d                 # If array[j] <= array[j+1]
  jle    loop_counters            # Then we can skip this iteration
  mov    DWORD PTR [rax], r9d     # Otherwise swap the values
  mov    DWORD PTR [rax + 4], r8d
loop_counters:
  lea    rax, [rax + 4]           # First, advance the inner loop
  cmp    rax, rdx
  jl     inner_loop               # And in case it's not done, repeat
  inc    rcx                      # If it is done, go back to doing the outer loop
  jmp    outer_loop

bubble_sort_done:
  ret

main:
  # Set up our stack
  sub    rsp, 40

  # We load the array in chunks onto the stack
  movaps xmm0, XMMWORD PTR [array]
  movaps XMMWORD PTR [rsp], xmm0
  movaps xmm0, XMMWORD PTR [array + 16]
  movaps XMMWORD PTR [rsp + 16], xmm0
  mov    rax, QWORD PTR [array + 32]
  mov    QWORD PTR [rsp + 32], rax

  # Print the unsorted array
  mov    rdi, OFFSET unsorted
  xor    rax, rax
  call   printf

  mov    rdi, rsp
  mov    rsi, array_len
  call   print_array

  # Sort
  mov    rdi, rsp
  mov    rsi, array_len
  call   bubble_sort

  # Print the sorted array
  mov    rdi, OFFSET sorted
  xor    rax, rax
  call   printf

  mov    rdi, rsp
  mov    rsi, array_len
  call   print_array

  # Restore the stack pointer, set return value to 0
  add    rsp, 40
  xor    rax, rax
  ret
