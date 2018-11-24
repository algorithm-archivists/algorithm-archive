.intel_syntax noprefix

.section .rodata
  two_pi:        .double -6.28318530718
  conj_const:    .long 0
                 .long -2147483648
                 .long 0
                 .long 0
  one:           .double 1.0
  fmt:           .string "%zu %f %+fi\n"
  txt:           .string "Output Differnece between the two methods:\n"

.section .text
  .global main
  .extern printf, memcpy, cexp, muldc3

# rdi - array ptr
# rsi - array size
fft:
  push   rbx
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                    # Save parameters
  mov    r13, rsi
  sub    rsp, r13                    # Make a double complex array
  xor    r14, r14                    # Set index to 0
fft_loop_i:
  cmp    r14, r13                    # Check if index is equal to array size
  je     fft_end_i
  lea    rax, [rsp + r14]            # Set tmp array to zero at r14
  mov    QWORD PTR [rax], 0
  mov    QWORD PTR [rax + 8], 0
  xor    r15, r15                    # Set second index to 0
fft_loop_j:
  cmp    r15, r13                    # Check if the index is equal to array size
  je     fft_end_j
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
  jmp    fft_loop_j
fft_end_j:
  add    r14, 16
  jmp    fft_loop_i
fft_end_i:
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
# rsi - array ptr
ifft:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi
  mov    r13, rsi
  lea    r14, [rdi + rsi]
  mov    rax, rdi
  movq   xmm1, conj_const
ifft_conj:
  cmp    rax, r14
  je     ifft_end_conj
  movsd  xmm0, QWORD PTR [rax + 8]
  xorpd  xmm0, xmm1
  movsd  QWORD PTR [rax + 8], xmm0
  add    rax, 16
  jmp    ifft_conj
ifft_end_conj:
  call   fft
  movq   xmm2, conj_const
  cvtsi2sdq xmm3, r14
  mov    rax, r12
ifft_normalise:
  cmp    rax, r14
  je     ifft_return
  movsd  xmm0, QWORD PTR [rax]
  movsd  xmm1, QWORD PTR [rax + 8]
  xorpd  xmm1, xmm2
  divsd  xmm0, xmm3
  divsd  xmm1, xmm3
  movsd  QWORD PTR [rax], xmm0
  movsd  QWORD PTR [rax + 8], xmm1
  add    rax, 16
  jmp    ifft_normalise
ifft_return:
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - signal 1 array ptr
# rsi - signal 2 array ptr
# rdx - out array ptr
# rcx - signal 1 size
# r8  - signal 2 size
conv:
  push   r12
  push   r13
  push   r14
  push   r15
  push   rbx
  push   rbp
  sub    rsp, 24
  mov    r12, rdi
  mov    r13, rsi
  mov    r14, rdx
  lea    rax, [rcx + r8]                     # Save the sum sizes into the stack
  mov    QWORD PTR [rsp], rax
  pxor   xmm0, xmm0
  movsd  QWORD PTR [rsp + 8], xmm0           # Save 0 + 0i into the stack
  movsd  QWORD PTR [rsp + 16], xmm0
  mov    rbp, rcx                            # Find smallest array size
  cmp    rbp, r8
  jge    conv_keep_size
  mov    rbp, r8
conv_keep_size:
  xor    r15, r15
conv_loop_i:
  cmp    r15, QWORD PTR [rsp]                # Go through all entries in out array
  je     conv_end_i
  xor    rbx, rbx
conv_loop_j:
  cmp    rbx, r15                            # Check if the rbx is less then r15
  je     conv_end_j
  cmp    r15, rbp                            # Check if r15 is less then the smallest array size
  je     conv_end_j
  lea    rdi, [r12 + rbx]                    # Get signal1[j] * signal2[i-j]
  mov    rsi, r15
  sub    rsi, rbx
  lea    rsi, [r13 + rsi]
  movsd  xmm0, QWORD PTR [rdi]
  movsd  xmm1, QWORD PTR [rdi + 8]
  movsd  xmm2, QWORD PTR [rsi]
  movsd  xmm3, QWORD PTR [rsi + 8]
  call   __muldc3
  movsd  xmm2, QWORD PTR [rsp + 8]           # Sum result to sum
  movsd  xmm3, QWORD PTR [rsp + 16]
  addsd  xmm0, xmm2
  addsd  xmm1, xmm3
  movsd  QWORD PTR [rsp + 8], xmm0           # Save sum
  movsd  QWORD PTR [rsp + 16], xmm1
  add    rbx, 16
  jmp    conv_loop_j
conv_end_j:
  movsd  QWORD PTR [r14 + r15], xmm0         # Save sum to out[i]
  movsd  QWORD PTR [r14 + 8 + r15], xmm1
  pxor   xmm0, xmm0                          # Set sum to 0+0i
  movsd  QWORD PTR [rsp + 8], xmm0
  movsd  QWORD PTR [rsp + 16], xmm0
  add    r15, 16
  jmp    conv_loop_i
conv_end_i:
  add    rsp, 24
  pop    rbp
  pop    rbx
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - signal 1 array ptr
# rsi - signal 2 array ptr
# rdx - out array ptr
# rcx - array size
conv_fft:
  push   r12
  push   r13
  push   r14
  push   r15
  push   rbx
  mov    r12, rdi
  mov    r13, rsi
  mov    r14, rdx
  mov    r15, rcx
  mov    rsi, rcx
  call   fft                             # FFT on both signals
  mov    rdi, r13
  mov    rsi, r15
  call   fft
  xor    rbx, rbx
conv_fft_mult:
  cmp    rbx, r15                        # Loop through all elements in the arrays
  je     conv_fft_return
  movsd  xmm0, QWORD PTR [r12 + rbx]     # signal1[i] * signal2[i]
  movsd  xmm1, QWORD PTR [r12 + 8 + rbx]
  movsd  xmm2, QWORD PTR [r13 + rbx]
  movsd  xmm3, QWORD PTR [r13 + 8 + rbx]
  call   __muldc3
  movsd  QWORD PTR [r14 + rbx], xmm0     # Save results to out[i]
  movsd  QWORD PTR [r14 + 8 + rbx], xmm1
  add    rbx, 16
  jmp    conv_fft_mult
conv_fft_return:
  mov    rdi, r14                        # Preform a IFFT on the out array
  mov    rsi, r15
  call   ifft
  pop    rbx
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

main:
  push r12
  xor eax, eax
  mov rdx, -16
  push rbp
  push rbx
  sub rsp, 10240
  pxor  xmm1, xmm1
  pxor  xmm0, xmm0
  movsd xmm3, one
  pxor  xmm2, xmm2
  jmp .L7
.L13:
  movsd QWORD PTR [rsp+rax], xmm3
  movsd QWORD PTR [rsp+8+rax], xmm2
  movsd QWORD PTR [rsp+1024+rax], xmm3
  movsd QWORD PTR [rsp+1032+rax], xmm2
  movsd QWORD PTR [rsp+2048+rax], xmm3
  movsd QWORD PTR [rsp+2056+rax], xmm2
  movsd QWORD PTR [rsp+4096+rax], xmm3
  movsd QWORD PTR [rsp+4104+rax], xmm2
  movsd QWORD PTR [rsp+6144+rax], xmm1
  movsd QWORD PTR [rsp+6152+rax], xmm0
  movsd QWORD PTR [rsp+8192+rax], xmm1
  movsd QWORD PTR [rsp+8200+rax], xmm0
.L3:
  add rdx, 1
  add rax, 16
.L7:
  cmp rdx, 31
  jbe .L13
  lea rcx, [rdx+16]
  cmp rcx, 63
  jbe .L4
  movsd QWORD PTR [rsp+2048+rax], xmm1
  movsd QWORD PTR [rsp+2056+rax], xmm0
  movsd QWORD PTR [rsp+4096+rax], xmm1
  movsd QWORD PTR [rsp+4104+rax], xmm0
  movsd QWORD PTR [rsp+6144+rax], xmm1
  movsd QWORD PTR [rsp+6152+rax], xmm0
  movsd QWORD PTR [rsp+8192+rax], xmm1
  movsd QWORD PTR [rsp+8200+rax], xmm0
  cmp rdx, 111
  jne .L3
  mov r8d, 64
  mov ecx, 64
  mov rdi, rsp
  xor eax, eax
  lea rdx, [rsp+6144]
  lea rsi, [rsp+1024]
  xor ebx, ebx
  xor ebp, ebp
  call conv
  mov ecx, 128
  xor eax, eax
  lea rdx, [rsp+8192]
  lea rsi, [rsp+4096]
  lea rdi, [rsp+2048]
  call conv_fft
  lea r12, [rsp+6152]
  mov edi, OFFSET txt
  call printf
.L6:
  movsd xmm1, QWORD PTR [r12+rbx]
  mov rsi, rbp
  mov edi, OFFSET fmt
  movsd xmm0, QWORD PTR [rsp+6144+rbx]
  subsd xmm1, QWORD PTR [rsp+8200+rbx]
  mov eax, 2
  add rbp, 1
  subsd xmm0, QWORD PTR [rsp+8192+rbx]
  add rbx, 16
  call printf
  cmp rbp, 128
  jne .L6
  add rsp, 10240
  xor eax, eax
  pop rbx
  pop rbp
  pop r12
  ret
.L4:
  movsd QWORD PTR [rsp+rax], xmm1
  movsd QWORD PTR [rsp+8+rax], xmm0
  movsd QWORD PTR [rsp+1024+rax], xmm1
  movsd QWORD PTR [rsp+1032+rax], xmm0
  movsd QWORD PTR [rsp+2048+rax], xmm1
  movsd QWORD PTR [rsp+2056+rax], xmm0
  movsd QWORD PTR [rsp+4096+rax], xmm1
  movsd QWORD PTR [rsp+4104+rax], xmm0
  movsd QWORD PTR [rsp+6144+rax], xmm1
  movsd QWORD PTR [rsp+6152+rax], xmm0
  movsd QWORD PTR [rsp+8192+rax], xmm1
  movsd QWORD PTR [rsp+8200+rax], xmm0
  jmp .L3
