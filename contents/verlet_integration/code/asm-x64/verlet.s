.intel_syntax noprefix

.section .rodata
  zero:          .double 0.0
  two:           .double 2.0
  half:          .double 0.5
  verlet_fmt:    .string "Time for Verlet integration is: %lf\n"
  stormer_fmt:   .string "Time and Velocity for Stormer Verlet Integration is: %lf, %lf\n"
  velocity_fmt:  .string "Time and Velocity for Velocity Verlet Integration is: %lf, %lf\n"
  pos:           .double 5.0
  acc:           .double -10.0
  dt:            .double 0.01

.section .text
  .global main
  .extern printf

# xmm0 - pos
# xmm1 - acc
# xmm2 - dt
# RET xmm0 - time
verlet:
  pxor   xmm7, xmm7                  # Holds 0 for comparisons
  pxor   xmm3, xmm3                  # Holds time value
  comisd xmm0, xmm7                  # Check if pos is greater then 0.0
  jbe    verlet_return
  movsd  xmm6, xmm1                  # xmm6 = acc * dt * dt
  mulsd  xmm6, xmm2
  mulsd  xmm6, xmm2
  movsd  xmm5, xmm0                  # Holds previous position
verlet_loop:
  addsd  xmm3, xmm2                  # Adding dt to time
  movsd  xmm4, xmm0                  # Hold old value of posistion
  addsd  xmm0, xmm0                  # Calculating new position
  subsd  xmm0, xmm5
  addsd  xmm0, xmm6
  movsd  xmm5, xmm4
  comisd xmm0, xmm7                  # Check if position is greater then 0.0
  ja     verlet_loop
verlet_return:
  movsd  xmm0, xmm3                  # Saving time value
  ret

# xmm0 - pos
# xmm1 - acc
# xmm2 - dt
# RET xmm0 - time
# RET xmm1 - velocity
stormer_verlet:
  pxor   xmm7, xmm7                  # Holds 0 for comparisons
  pxor   xmm3, xmm3                  # Holds time value
  comisd xmm0, xmm7                  # Check if pos is greater then 0.0
  jbe    stormer_verlet_return
  movsd  xmm6, xmm1                  # xmm6 = acc * dt * dt
  mulsd  xmm6, xmm2
  mulsd  xmm6, xmm2
  movsd  xmm5, xmm0                  # Holds previous position
stormer_verlet_loop:
  addsd  xmm3, xmm2                  # Adding dt to time
  movsd  xmm4, xmm0                  # Hold old value of posistion
  addsd  xmm0, xmm0                  # Calculating new position
  subsd  xmm0, xmm5
  addsd  xmm0, xmm6
  movsd  xmm5, xmm4
  comisd xmm0, xmm7                  # Check if position is greater then 0.0
  ja     stormer_verlet_loop
stormer_verlet_return:
  movsd  xmm0, xmm3                  # Saving time and velocity
  mulsd  xmm3, xmm1
  movsd  xmm1, xmm3
  ret

# xmm0 - pos
# xmm1 - acc
# xmm2 - dt
# RET xmm0 - time
# RET xmm1 - velocity
velocity_verlet:
  pxor   xmm7, xmm7                  # Holds 0 for comparisons
  pxor   xmm3, xmm3                  # Holds the velocity value
  pxor   xmm4, xmm4                  # Holds the time value
  comisd xmm0, xmm7                  # Check if pos is greater then 0.0
  jbe    velocity_verlet_return
  movsd  xmm5, half                  # xmm5 = 0.5 * dt * dt * acc
  mulsd  xmm5, xmm2
  mulsd  xmm5, xmm2
  mulsd  xmm5, xmm1
velocity_verlet_loop:
  movsd  xmm6, xmm3                  # Move velocity into register
  mulsd  xmm6, xmm2                  # Calculate new position
  addsd  xmm6, xmm5
  addsd  xmm0, xmm6
  addsd  xmm4, xmm2                  # Incrementing time
  movsd  xmm3, xmm4                  # Updating velocity
  mulsd  xmm3, xmm1
  comisd xmm0, xmm7
  ja     velocity_verlet_loop
velocity_verlet_return:
  movsd  xmm0, xmm4                  # Saving time and velocity
  movsd  xmm1, xmm3
  ret

main:
  push   rbp
  movsd  xmm0, pos                   # Calling verlet
  movsd  xmm1, acc
  movsd  xmm2, dt
  call   verlet
  mov    rdi, OFFSET verlet_fmt      # Print output
  mov    rax, 1
  call   printf
  movsd  xmm0, pos                   # Calling stormer_verlet
  movsd  xmm1, acc
  movsd  xmm2, dt
  call   stormer_verlet
  mov    rdi, OFFSET stormer_fmt     # Print output
  mov    rax, 1
  call   printf
  movsd  xmm0, pos                   # Calling velocity_verlet
  movsd  xmm1, acc
  movsd  xmm2, dt
  call   velocity_verlet
  mov    rdi, OFFSET velocity_fmt    # Print output
  mov    rax, 1
  call   printf
  pop    rbp
  ret

