.intel_syntax noprefix

.section .rodata
  array:
    .align 16
    .int 1, 3654, 78, 654, -234, -12, 4, 3, -6, -100
    .equ array_len, (.-array) / 4
  array_fmt: .string "%d "
  lf:        .string "\n"
  unsorted:  .string "Unsorted array: "
  sorted:    .string "Sorted array: "

.section .text
  .global main
  .extern printf, rand, srand

# rdi - array ptr
# rsi - array size
print_array:
  push   r12
  push   r13
  mov    r12, rdi                 # Loop variable
  lea    r13, [rdi + 4 * rsi]     # Pointer after the last element
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

# rdi - array ptr
# rsi - array size
# RET rax - boolean
is_sorted:
  sub    rsi, 1                   # Getting array + n and *array + n - 1
  lea    rcx, [rsi - 1]
  lea    rcx, [rdi + 4 * rcx]
  lea    rsi, [rdi + 4 * rsi]
is_sorted_loop:
  cmp    rsi, rdi                 # Check if array + n - 1 == array
  je     is_sorted_done
  mov    edx, DWORD PTR [rsi]     # Load value to register
  xor    rax, rax                 # Set rax to 0
  cmp    edx, DWORD PTR [rcx]     # Check if array[n] < array[n - 1]
  jl     is_sorted_return
  sub    rcx, 4                   # If not make pointers go to down an element
  sub    rsi, 4
  jmp    is_sorted_loop
is_sorted_done:
  mov    rax, 1                   # If sorted then set rax to 1
is_sorted_return:
  ret                             # Return

# rdi - array ptr
# rsi - array size
shuffle:
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                          # Save parameters
  mov    r13, rsi
  xor    r14, r14
shuffle_loop:
  cmp    r14, r13                          # Check if i == array size
  je     shuffle_done
  mov    r15d, DWORD PTR [r12 + r14 * 4]   # Save array[i]
  call   rand                              # Swap a random element with array[i]
  xor    edx, edx
  div    r13                               # Mod random number to keep in array
  mov    eax, DWORD PTR [r12 + rdx * 4]
  mov    DWORD PTR [r12 + r14 * 4], eax
  mov    DWORD PTR [r12 + rdx * 4], r15d
  add    r14, 1                            # increment then repeat
  jmp    shuffle_loop
shuffle_done:
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - array ptr
# rsi - array size
bogo_sort:
  push   r12
  push   r13
  mov    r12, rdi
  mov    r13, rsi
bogo_sort_loop:
  mov    rdi, r12                         # Check if the array is sorted
  mov    rsi, r13
  call   is_sorted
  test   rax, rax
  jnz    bogo_sort_done
  mov    rdi, r12                         # If not then shuffle
  mov    rsi, r13
  call   shuffle
  jmp    bogo_sort_loop
bogo_sort_done:
  pop    r13
  pop    r12
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
  call   bogo_sort
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

