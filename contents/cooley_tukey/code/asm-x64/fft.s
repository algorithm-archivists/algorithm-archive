.intel_syntax noprefix

.section .rodata
  two:           .double 2.0
  one:           .double 1.0
  two_pi:        .double -6.28318530718
  rand_max:      .long 4290772992
                 .long 1105199103
  fmt:           .string "%g\n"

.section .text
  .global main
  .extern printf, memset, memcpy, srand, rand, time, cexp, __muldc3, cabs, log2

# rdi - array ptr
# rsi - array size
dft:
  push   rbx
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                    # Save parameters
  mov    r13, rsi
  sub    rsp, r13                    # Make a double complex array
  xor    r14, r14                    # Set index to 0
dft_loop_i:
  cmp    r14, r13                    # Check if index is equal to array size
  je     dft_end_i
  lea    rax, [rsp + r14]            # Set tmp array to zero at r14
  mov    QWORD PTR [rax], 0
  mov    QWORD PTR [rax + 8], 0
  xor    r15, r15                    # Set second index to 0
dft_loop_j:
  cmp    r15, r13                    # Check if the index is equal to array size
  je     dft_end_j
  movsd  xmm1, two_pi                # Calculate xmm1 = -2pi * i * j / N
  mov    rax, r14
  imul   rax, r15
  shr    rax, 4
  cvtsi2sdq xmm2, rax
  mulsd  xmm1, xmm2
  cvtsi2sdq xmm2, r13
  divsd  xmm1, xmm2
  pxor   xmm0, xmm0                  # Set xmm0 to 0
  call   cexp
  lea    rax, [r12 + r15]            # Calculate X[i] * cexp(-2pi * i * j / N)
  movsd  xmm2, QWORD PTR [rax]
  movsd  xmm3, QWORD PTR [rax + 8]
  call   __muldc3
  lea    rax, [rsp + r14]
  movsd  xmm6, QWORD PTR [rax]       # Sum to tmp array
  movsd  xmm7, QWORD PTR [rax + 8]
  addsd  xmm6, xmm0
  addsd  xmm7, xmm1
  movsd  QWORD PTR [rax], xmm6       # Save to tmp array
  movsd  QWORD PTR [rax + 8], xmm7
  add    r15, 16
  jmp    dft_loop_j
dft_end_j:
  add    r14, 16
  jmp    dft_loop_i
dft_end_i:
  mov    rdi, r12                    # Move tmp array to array ptr
  mov    rsi, rsp
  mov    rdx, r13
  call   memcpy
  add    rsp, r13
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  pop    rbx
  ret

# rdi - array ptr
# rsi - array size
cooley_tukey:
  cmp    rsi, 16                     # Check if size if greater then 1
  jle    cooley_tukey_return
  push   rbx
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                    # Save parameters
  mov    r13, rsi
  mov    r14, rsi                    # Save N / 2
  shr    r14, 1
  sub    rsp, r14                    # Make a tmp array
  xor    r15, r15
  mov    rbx, r12
cooley_tukey_spliting:
  cmp    r15, r14
  je     cooley_tukey_split
  lea    rax, [r12 + 2 * r15]        # Moving all odd entries to the front of the array
  movaps xmm0, XMMWORD PTR [rax + 16]
  movaps xmm1, XMMWORD PTR [rax]
  movaps XMMWORD PTR [rsp + r15], xmm0
  movaps XMMWORD PTR [rbx], xmm1
  add    rbx, 16
  add    r15, 16
  jmp    cooley_tukey_spliting
cooley_tukey_split:
  mov    rax, rsp
  lea    rdi, [r12 + r13]
cooley_tukey_mov_data:
  cmp    rbx, rdi
  je     cooley_tukey_moved
  movaps xmm0, XMMWORD PTR [rax]
  movaps XMMWORD PTR [rbx], xmm0
  add    rbx, 16
  add    rax, 16
  jmp    cooley_tukey_mov_data
cooley_tukey_moved:
  add    rsp, r14
  mov    rdi, r12                   # Makking a recursive call
  mov    rsi, r14
  call   cooley_tukey
  lea    rdi, [r12 + r14]           # Makking a recursive call
  mov    rsi, r14
  call   cooley_tukey
  lea    rbx, [r12 + r14]
  mov    r14, rbx
  mov    r15, r12
cooley_tukey_loop:
  cmp    r15, rbx
  je     cooley_tukey_end
  pxor   xmm0, xmm0                 # Calculate cexp(-2.0 * I * M_PI * i / N)
  movsd  xmm1, two_pi
  mov    rax, r14
  sub    rax, rbx
  cvtsi2sdq xmm2, rax
  cvtsi2sdq xmm3, r13
  divsd  xmm2, xmm3
  mulsd  xmm1, xmm2
  call   cexp
  movq   xmm2, QWORD PTR [r14]      # Calculating X[i] - cexp() * X[i + N / 2]
  movq   xmm3, QWORD PTR [r14 + 8]
  call   __muldc3
  movq   xmm2, QWORD PTR [r15]
  movq   xmm3, QWORD PTR [r15 + 8]
  subsd  xmm2, xmm0
  subsd  xmm3, xmm1
  movq   QWORD PTR [r14], xmm2      # Save value in X[i + N / 2]
  movq   QWORD PTR [r14 + 8], xmm3
  movq   xmm0, QWORD PTR [r15]      # Calculating X[i] -= X[i + N / 2] - X[i]
  movq   xmm1, QWORD PTR [r15 + 8]
  subsd  xmm2, xmm0
  subsd  xmm3, xmm1
  subsd  xmm0, xmm2
  subsd  xmm1, xmm3
  movq   QWORD PTR [r15], xmm0
  movq   QWORD PTR [r15 + 8], xmm1
  add    r14, 16
  add    r15, 16
  jmp    cooley_tukey_loop
cooley_tukey_end:
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  pop    rbx
cooley_tukey_return:
  ret

# rdi - array ptr
# rsi - array size
bit_reverse:
  push   rbx
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                  # Save parameters
  mov    r13, rsi
  shr    r13, 4
  xor    r14, r14                  # Loop through all entries
bit_reverse_entries:
  cmp    r14, r13
  je     bit_reverse_return
  cvtsi2sdq xmm0, r13              # Calculating the number of bit in N
  call   log2
  cvttsd2si rcx, xmm0
  mov    rdi, 1                    # Calculating (1 << log2(N)) - 1
  sal    edi, cl
  sub    edi, 1
  sub    ecx, 1
  mov    rax, r14
  mov    r15, r14
bit_reverse_loop:
  sar    r15                       # Check if r15 is 0
  je     bit_reverse_reversed
  sal    rax, 1                    # Calculating (rax << 1) | (r15 & 1)
  mov    rsi, r15
  and    rsi, 1
  or     rax, rsi
  sub    ecx, 1                    # Decrement bit count
  jmp    bit_reverse_loop
bit_reverse_reversed:
  sal    eax, cl                   # Calculate (rax << rcx) & (1 << bit count)
  and    rax, rdi
  cmp    rax, r14                  # Check if rax is greater then r14
  jle    bit_reverse_no_swap       # If so then swap entries
  shl    rax, 4                    # Times index by 16 to get bytes to entry
  shl    r14, 4
  movaps xmm0, XMMWORD PTR [r12 + rax]
  movaps xmm1, XMMWORD PTR [r12 + r14]
  movaps XMMWORD PTR [r12 + rax], xmm1
  movaps XMMWORD PTR [r12 + r14], xmm0
  shr    r14, 4
bit_reverse_no_swap:
  add    r14, 1
  jmp    bit_reverse_entries
bit_reverse_return:
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  pop    rbx
  ret

# rdi - array ptr
# rsi - array size
iterative_cooley_tukey:
  push   r12
  push   r13
  push   r14
  push   r15
  push   rbx
  sub    rsp, 48
  mov    r12, rdi
  mov    r13, rsi
  call   bit_reverse              # Bit reversing array
  sar    r13, 4                   # Calculate log2(N)
  cvtsi2sdq xmm0, r13
  call   log2
  cvttsd2si rax, xmm0
  mov    QWORD PTR [rsp], rax     # Save it to the stack
  mov    r14, 1
iter_ct_loop_i:
  cmp    r14, rax                 # Check if r14 is greater then log2(N)
  jg     iter_ct_end_i
  movsd  xmm0, two                # Calculate stride = 2^(r14)
  cvtsi2sdq xmm1, r14
  call   pow
  cvttsd2si r10, xmm0
  mov    QWORD PTR [rsp + 40], r10# move stride to stack
  movsd  xmm1, two_pi             # Calculating cexp(-2pi * I / stride)
  divsd  xmm1, xmm0
  pxor   xmm0, xmm0
  call   cexp
  movq   QWORD PTR [rsp + 8], xmm0  # Save it to stack
  movq   QWORD PTR [rsp + 16], xmm1
  xor    r15, r15
iter_ct_loop_j:
  cmp    r15, r13                 # Check if r15 is less then array size
  je     iter_ct_end_j
  movsd  xmm4, one                # Save 1 + 0i to stack
  pxor   xmm5, xmm5
  movsd  QWORD PTR [rsp + 24], xmm4
  movsd  QWORD PTR [rsp + 32], xmm5
  xor    rbx, rbx
  mov    rax, QWORD PTR [rsp + 40]# Calculate stride / 2
  sar    rax, 1
iter_ct_loop_k:
  cmp    rbx, rax                 # Check if rbx is less then stride / 2
  je     iter_ct_end_k
  mov    r8, r15                  # Saving pointers to X[k + j + stride / 2] and X[k + j]
  add    r8, rbx
  sal    r8, 4
  mov    r9, QWORD PTR [rsp + 40]
  sal    r9, 3
  add    r9, r8
  lea    r9, [r12 + r9]
  lea    r8, [r12 + r8]
  movsd  xmm0, QWORD PTR [r9]     # Calculate X[k + j] - v * X[k + j + stride / 2]
  movsd  xmm1, QWORD PTR [r9 + 8]
  movsd  xmm2, QWORD PTR [rsp + 24]
  movsd  xmm3, QWORD PTR [rsp + 32]
  call   __muldc3
  movsd  xmm2, QWORD PTR [r8]
  movsd  xmm3, QWORD PTR [r8 + 8]
  subsd  xmm2, xmm0
  subsd  xmm3, xmm1
  movsd  QWORD PTR [r9], xmm2     # Saving answer
  movsd  QWORD PTR [r9 + 8], xmm3
  movsd  xmm0, QWORD PTR [r8]     # Calculating X[k + j] - (X[k + j + stride / 2] - X[k + j])
  movsd  xmm1, QWORD PTR [r8 + 8]
  subsd  xmm2, xmm0
  subsd  xmm3, xmm1
  subsd  xmm0, xmm2
  subsd  xmm1, xmm3
  movsd  QWORD PTR [r8], xmm0     # Saving answer
  movsd  QWORD PTR [r8 + 8], xmm1
  movsd  xmm0, QWORD PTR [rsp + 24] # Calculating v * w
  movsd  xmm1, QWORD PTR [rsp + 32]
  movsd  xmm2, QWORD PTR [rsp + 8]
  movsd  xmm3, QWORD PTR [rsp + 16]
  call   __muldc3
  movsd  QWORD PTR [rsp + 24], xmm0 # Saving answer
  movsd  QWORD PTR [rsp + 32], xmm1
  add    rbx, 1
  mov    rax, QWORD PTR [rsp + 40]
  sar    rax, 1
  jmp    iter_ct_loop_k
iter_ct_end_k:
  add    r15, QWORD PTR [rsp + 40]
  jmp    iter_ct_loop_j
iter_ct_end_j:
  add    r14, 1
  mov    rax, QWORD PTR [rsp]
  jmp    iter_ct_loop_i
iter_ct_end_i:
  add    rsp, 48
  pop    rbx
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - array a ptr
# rsi - array b ptr
# rdx - array size
approx:
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi
  mov    r13, rsi
  mov    r14, rdx
  lea    r15, [rdi + rdx]
  sub    rsp, 8
approx_loop:
  cmp    r12, r15
  je     approx_return
  movsd  xmm0, QWORD PTR[r13]
  movsd  xmm1, QWORD PTR[r13 + 8]
  call   cabs
  movsd  QWORD PTR [rsp], xmm0
  movsd  xmm0, QWORD PTR[r12]
  movsd  xmm1, QWORD PTR[r12 + 8]
  call   cabs
  movsd  xmm1, QWORD PTR [rsp]
  subsd  xmm0, xmm1
  mov    rdi, OFFSET fmt
  mov    rax, 1
  call   printf
  add    r12, 16
  add    r13, 16
  jmp    approx_loop
approx_return:
  add    rsp, 8
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

main:
  push   r12
  sub    rsp, 2048
  mov    rdi, 0
  call   time
  mov    edi, eax
  call   srand
  lea    r12, [rsp + 1024]
loop:
  cmp    r12, rsp
  je     end_loop
  sub    r12, 16
  call   rand
  cvtsi2sd xmm0, rax
  divsd  xmm0, rand_max
  lea    rax, [r12 + 1024]
  movsd  QWORD PTR [r12], xmm0
  movsd  QWORD PTR [rax], xmm0
  mov    QWORD PTR [r12 + 8], 0
  mov    QWORD PTR [rax + 8], 0
  jmp    loop
end_loop:
  mov    rdi, rsp
  mov    rsi, 1024
  call   iterative_cooley_tukey
  lea    rdi, [rsp + 1024]
  mov    rsi, 1024
  call   cooley_tukey
  mov    rdi, rsp
  lea    rsi, [rsp + 1024]
  mov    rdx, 1024
  call   approx
  xor    rax, rax
  add    rsp, 2048
  pop    r12
  ret


