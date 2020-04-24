.intel_syntax noprefix

.section .rodata
  pi:            .double 3.141592653589793
  one:           .double 1.0
  four:          .double 4.0
  hundred:       .double 100.0
  rand_max:      .long 4290772992
                 .long 1105199103
  fabs_const:    .long 4294967295
                 .long 2147483647
                 .long 0
                 .long 0
  estimate_fmt:  .string "The estaimate of pi is %lf\n"
  error_fmt:     .string "Percentage error: %0.2f\n"

.section .text
  .global main
  .extern printf, srand, time, rand

# xmm0 - x
# xmm1 - y
# RET rax - bool
in_circle:
  mulsd  xmm0, xmm0                  # Calculate x * x + y * y
  mulsd  xmm1, xmm1
  addsd  xmm0, xmm1
  movsd  xmm1, one                   # Set circle radius to 1
  xor    rax, rax
  comisd xmm1, xmm0                  # Return bool xmm0 < xmm1
  seta al
  ret

# rdi - samples
# RET xmm0 - estimate
monte_carlo:
  pxor   xmm2, xmm2                  # Setting it to zero for loop
  cvtsi2sd xmm3, rdi                 # From int to double
  pxor   xmm4, xmm4                  # Setting to zero for counter
monte_carlo_iter:
  comisd xmm2, xmm3                  # Check if we went through all samples
  je     monte_carlo_return
  call   rand                        # Get random point in the first quartile
  cvtsi2sd xmm0, rax
  divsd  xmm0, rand_max
  call   rand
  cvtsi2sd xmm1, rax
  divsd  xmm1, rand_max
  call   in_circle                   # Check if its in the circle
  test   rax, rax
  jz     monte_carlo_false
  addsd  xmm4, one                   # if so increment counter
monte_carlo_false:
  addsd  xmm2, one
  jmp    monte_carlo_iter
monte_carlo_return:
  mulsd  xmm4, four                  # Return estimate
  divsd  xmm4, xmm2
  movsd  xmm0, xmm4
  ret

main:
  push   rbp
  sub    rsp, 16
  mov    rdi, 0
  call   time
  mov    rdi, rax
  call   srand
  mov    rdi, 1000000
  call   monte_carlo
  movsd  QWORD PTR [rsp], xmm0      # Save estimate to stack
  mov    rdi, OFFSET estimate_fmt   # Print estimate
  mov    rax, 1
  call   printf
  movsd  xmm0, QWORD PTR [rsp]      # Get estimate from stack
  movsd  xmm1, pi                   # Calculate fabs(M_PI - estimate)
  subsd  xmm0, xmm1
  movq   xmm1, fabs_const
  andpd  xmm0, xmm1
  divsd  xmm0, pi                   # Print percentage error on pi
  mulsd  xmm0, hundred
  mov    rdi, OFFSET error_fmt
  mov    rax, 1
  call   printf
  add    rsp, 16
  pop    rbp
  ret

