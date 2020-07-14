.intel_syntax noprefix

.section .rodata
  three:      .double -3.0
  fabs_const:
              .long   4294967295
              .long   2147483647
              .long   0
              .long   0
  inital_val: .double 1.0
  threshold:  .double 0.01
  timestep:   .double 0.01
  error_fmt:  .string "%f    %f\n"
  fmt:        .string "%d\n"

.section .text
  .global main
  .extern printf
  .extern exp

# rdi  - array size
# rsi  - array ptr
# xmm0 - timestep
solve_euler:
  movsd  xmm1, inital_val
  lea    rax, [rsi + 8 * rdi + 8]    # Set to end of the array
solve_euler_loop:
  movsd  xmm3, three                 # Set to -3.0
  mulsd  xmm2, xmm1                  # xmm2 = -3.0 * array[i-1] * timestep
  mulsd  xmm2, xmm0
  subsd  xmm1, xmm2                  # xmm1 = xmm1 - xmm2
  movsd  QWORD PTR [rsi], xmm1
  add    rsi, 8
  cmp    rsi, rax                    # Test if we have gone through the array
  jne    solve_euler_loop
solve_euler_return:
  ret

# rdi  - array size
# rsi  - array ptr
# xmm0 - timestep
# xmm1 - threshold
# RET rax - success code 0 if sucess else 1
check_result:
  push   r12
  push   r13
  xor    rax, rax                    # Return code is 0
  xor    r12, r12                    # The index is set to 0
  mov    r13, rdi                    # Moving array size to free rdi for printf
  movsd  xmm2, xmm0                  # Moving timestep to free xmm0 for exp
  jmp    loop_check
results_loop:
  cvtsi2sd xmm0, r12                 # Making int to a double
  movsd  xmm3, three                 # Calculating exp(-3.0 * i * timestep)
  mulsd  xmm0, xmm3
  mulsd  xmm0, xmm2
  call   exp
  movsd  xmm3, QWORD PTR [rsi + r12 * 8] # Calculating abs(array[i] - xmm0)
  subsd  xmm2, xmm3
  movq   xmm3, fabs_const
  andpd  xmm0, xmm3
  comisd xmm0, xmm1                  # Check if abs(...) > threshold
  jbe    if_false
  mov    rdi, OFFSET error_fmt       # If true print out array[i] and solution
  mov    rax, 1
  call   printf
  mov    rax, 1                      # and set sucess code to failed (rax = 1)
if_false:
  add    r12, 1
loop_check:
  cmp    r12, r13                    # Check if index is less the array size
  jle    results_loop
  pop    r13
  pop    r12
  ret

main:
  push   rbp
  sub    rsp, 800                    # Making double array[100]
  mov    rdi, 100
  mov    rsi, rsp
  movsd  xmm0, timestep
  call   solve_euler                 # Calling solve_euler
  mov    rdi, 100
  mov    rsi, rsp
  movsd  xmm0, timestep
  movsd  xmm1, threshold
  call   check_result                # Check if results are correct
  mov    rdi, OFFSET fmt
  mov    rsi, rax
  xor    rax, rax
  call   printf                      # Print out success code
  add    rsp, 800                    # Deallocating array
  pop    rbp
  xor    rax, rax
  ret

