.intel_syntax noprefix

# System V calling convention cheatsheet
# Params: rdi, rsi, rdx, rcx, r8, r9, xmm0-7
# Return: rax (int 64 bits), rax:rdx (int 128 bits), xmm0 (float)
# Callee cleanup: rbx, rbp, r12-15
# Scratch: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11

.section .rodata
  not_bt:     .string "This is not a binary tree.\n"
  fmt_tree:   .string "%d \n"

  .equ stack_size,  16
  .equ stack_array, 0
  .equ stack_top,   8
  .equ stack_cap,   12

  .equ queue_size,  20
  .equ queue_array, 0
  .equ queue_front, 8
  .equ queue_back,  12
  .equ queue_cap,   16

  .equ tree_children,     0
  .equ tree_num_children, 8
  .equ tree_value,        12
  .equ tree_size,         16
.section .text
  .global main
  .extern printf, malloc, free, memcpy

# rdi - stack ptr
get_stack:
  push   r12
  mov    r12, rdi
  mov    rdi, 32                   # Creating a 32 byte array
  call   malloc
  mov    QWORD PTR [r12], rax      # Saving the data into the stack
  mov    DWORD PTR [r12 + 8], 0
  mov    DWORD PTR [r12 + 12], 32
  pop    r12
  ret

# rdi - stack ptr
# rsi - element ptr
stack_push:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi                  # Saving the variables
  mov    r13, rsi
  mov    r14d, DWORD PTR [r12 + 8]
  mov    esi, DWORD PTR [r12 + 12]
  cmp    rsi, r14                  # Check if top is equal to capacity
  jne    stack_push_append
  shl    rsi, 1                    # Calculate new capacity in bytes
  mov    DWORD PTR [r12 + 12], esi # Saving new capcaity
  mov    rdi, [r12]
  call   realloc                   # Making the array bigger
  mov    QWORD PTR [r12], rax
stack_push_append:
  add    r14, 8
  mov    rax, QWORD PTR [r12]
  lea    rax, [rax + r14]
  mov    QWORD PTR [rax], r13      # Saving element and new top
  mov    DWORD PTR [r12 + 8], r14d
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - stack ptr
# RET rax - element ptr
stack_pop:
  push   r12
  mov    r12d, DWORD PTR [rdi + 8] # Get top
  test   r12, r12                  # Check if top is zero
  jne    stack_pop_element
  xor    rax, rax                  # Return 0
  jmp    stack_pop_return
stack_pop_element:
  mov    rax, [rdi]
  lea    rax, [rax + r12]          # Get the element
  mov    rax, QWORD PTR [rax]
  sub    r12, 8                    # Subtract 1 from top and save it
  mov    DWORD PTR [rdi + 8], r12d
stack_pop_return:
  pop    r12
  ret

# rdi - stack ptr
free_stack:
  mov    rdi, QWORD PTR [rdi]
  call   free                      # Free stack array
  ret

# rdi - queue ptr
get_queue:
  push   r12
  mov    r12, rdi
  mov    rdi, 32                  # Create a 32 byte array
  call   malloc
  mov    QWORD PTR [r12], rax     # Saving data to the queue pointer
  mov    QWORD PTR [r12 + 8], 0
  mov    DWORD PTR [r12 + 16], 32
  pop    r12
  ret

# rdi - queue ptr
queue_resize:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi
  mov    edi, DWORD PTR [r12 + 16] # Get new capacity and create new array
  shl    rdi, 1
  call   malloc
  mov    r13, rax
  mov    r14, QWORD PTR[r12]
  mov    rdi, r13                  # Copy data from front to capacity
  mov    eax, DWORD PTR [r12 + 8]
  lea    rsi, [r14 + rax]
  mov    edx, DWORD PTR [r12 + 16]
  sub    edx, DWORD PTR [r12 + 8]
  call   memcpy
  mov    eax, DWORD PTR [r12 + 16] # Copy data from start of array to front
  sub    eax, DWORD PTR [r12 + 8]
  lea    rdi, [r13 + rax]
  mov    rsi, r14
  mov    edx, DWORD PTR [r12 + 8]
  call   memcpy
  mov    rdi, r14                  # New array has front at 0 and back at the old capacity
  call   free                      # So free the old array then save the new queue
  mov    QWORD PTR [r12], r13
  mov    eax, DWORD PTR [r12 + 16]
  sub    rax, 8
  mov    DWORD PTR [r12 + 12], eax
  mov    DWORD PTR [r12 + 8], 0
  mov    eax, DWORD PTR [r12 + 16]
  shl    rax, 1
  mov    DWORD PTR [r12 + 16], eax
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - queue ptr
# rsi - element
enqueue:
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi                 # Saving parameters
  mov    r13, rsi
  mov    r14d, DWORD PTR [rdi + 8]
  mov    eax, DWORD PTR [rdi + 12]# Calculating new back
  add    eax, 8
  mov    edi, DWORD PTR [r12 + 16]
  cdq
  idiv   edi
  cmp    rdx, r14                 # Check if front and new back are equal
  jne    enqueue_append
  mov    rdi, r12                 # If so resize the queue
  call   queue_resize
enqueue_append:
  mov    r14, QWORD PTR [r12]     # Saving the element
  mov    r15d, DWORD PTR [r12 + 12]
  lea    r14, [r14 + r15]
  mov    QWORD PTR [r14], r13
  mov    r14d, DWORD PTR [r12 + 16]# Calculating new back and then saving it
  add    r15, 8
  mov    rax, r15
  cdq
  idiv   r14d
  mov    DWORD PTR [r12 + 12], edx
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - queue ptr
# RET rax - element
dequeue:
  push   r12
  push   r13
  mov    r12d, DWORD PTR [rdi + 8] # Check if queue is empty
  mov    r13d, DWORD PTR [rdi + 12]
  xor    rax, rax
  cmp    r12, r13
  je     dequeue_return            # if empty return null
  mov    r12, QWORD PTR [rdi]      # else return element pointer
  mov    r13d, DWORD PTR [rdi + 8]
  lea    r13, [r12 + r13]
  mov    eax, DWORD PTR [rdi + 8]
  add    eax, 8
  mov    r12d, DWORD PTR [rdi + 16] # Calculate new front
  cdq
  idiv   r12d
  mov    DWORD PTR [rdi + 8], edx   # Save new front
  mov    rax, QWORD PTR [r13]
dequeue_return:
  pop    r13
  pop    r12
  ret

# rdi - queue ptr
free_queue:
  mov    rdi, QWORD PTR [rdi]       # Free queue array
  call   free
  ret

# rdi - levels
# rsi - children_size
# RET rax:rdx - the tree - children|value|children_size
create_tree:
  push   rbx
  push   r12
  push   r13
  push   r14
  push   r15
  mov    r12, rdi
  mov    r13, rsi
  test   rdi, rdi
  jz     create_tree_leaf
  mov    r14, rsi                  # We'll allocate sizeof(tree) * children_size bytes of memory
  shl    r14, 4                    # save the size calculation to a callee-saved register so we can reuse it after the malloc
  mov    rdi, r14
  call   malloc
  mov    r15, rax                  # Save the children address twice, once for the return value, once for the loop variable
  mov    rbx, rax
  lea    r14, [rax + r14]          # Calculate the address of the element after last of the children array
create_tree_children:
  cmp    rbx, r14
  je     create_tree_return
  lea    rdi, [r12 - 1]            # levels - 1
  mov    rsi, r13
  call   create_tree
  mov    QWORD PTR [rbx], rax      # Save the created tree to memory
  mov    QWORD PTR [rbx + 8], rdx  # The offset of children_size, writing out explicitly would've made the line way too long
  add    rbx, tree_size
  jmp    create_tree_children
create_tree_leaf:
  mov    r15, 0
  xor    r13, r13                  # Leaves won't have any children
create_tree_return:
  mov    rax, r15                  # The children pointer will be in r15
  mov    rdx, r12
  shl    rdx, 32                   # The tree's value will be the current "levels"
  shl    r13, 4
  or     rdx, r13                  # Generate the return value by moving the value to the upper 32 bits
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  pop    rbx
  ret

# rdi - children ptr
# rsi - children size
free_tree:
  push   r12
  push   r13
  push   r14
  push   r15
  test   rdi, rdi                 # Make sure the pointer is non-zero
  jz     free_tree_return
  mov    r12, rdi                 # Saving array
  lea    r13, [r12 + rsi]         # Get start and end of the array
  mov    r14, r12
free_tree_free_kid:
  cmp    r14, r13                 # Loop thought the array and free all children
  je     free_tree_free_array
  mov    rdi, QWORD PTR [r14]
  mov    esi, DWORD PTR [r14 + 8]
  call   free_tree
  add    r14, tree_size
  jmp    free_tree_free_kid
free_tree_free_array:
  mov    rdi, r12                 # Free the array
  call   free
free_tree_return:
  pop    r15
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - children ptr
# rsi - value|children_size
dfs_recursive:
  push   r12
  push   r13
  mov    r12, rdi
  mov    r13, rsi
  mov    rdi, OFFSET fmt_tree      # Handle the current node
  shr    rsi, 32                   # The tree value is in the upper 32 bits
  xor    rax, rax
  call   printf
  mov    r13d, r13d                # Zero out the top 32 bits
  add    r13, r12                  # Pointer pointing after the last element of the children array
dfs_recursive_children:
  cmp    r12, r13                  # If we reached the end, return
  je     dfs_recursive_return
  mov    rdi, QWORD PTR [r12]
  mov    rsi, QWORD PTR [r12 + 8]
  call   dfs_recursive
  add    r12, tree_size
  jmp    dfs_recursive_children
dfs_recursive_return:
  pop    r13
  pop    r12
  ret

# rdi - children ptr
# rsi - value|children_size
dfs_recursive_postorder:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi
  mov    r13, rsi
  mov    r14, rsi
  mov    r13d, r13d                # Zero out the top 32 bits
  add    r13, r12                  # Pointer pointing after the last element of the children array
dfs_recursive_po_children:
  cmp    r12, r13                  # If we reached the end, return
  je     dfs_recursive_po_return
  mov    rdi, QWORD PTR [r12]
  mov    rsi, QWORD PTR [r12 + 8]
  call   dfs_recursive_postorder
  add    r12, tree_size
  jmp    dfs_recursive_po_children
dfs_recursive_po_return:
  mov    rdi, OFFSET fmt_tree      # Handle the current node
  mov    rsi, r14
  shr    rsi, 32                   # The tree value is in the upper 32 bits
  xor    rax, rax
  call   printf
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - children ptr
# rsi - value|children_size
dfs_recursive_inorder_btree:
  push   r12
  push   r13
  mov    r12, rdi
  mov    r13, rsi
  mov    rax, rsi
  mov    eax, eax
  cmp    rax, 0                    # Check what type of tree it is.
  je     dfs_recursive_bt_size0
  cmp    rax, 16
  je     dfs_recursive_bt_size1
  cmp    rax, 32
  je     dfs_recursive_bt_size2
  mov    rdi, OFFSET not_bt        # If the tree is not binary then print a warning
  xor    rax, rax
  call   printf
  jmp    dfs_recursive_bt_return
dfs_recursive_bt_size0:
  mov    rdi, OFFSET fmt_tree      # If the node is a leaf then print its id
  shr    rsi, 32
  xor    rax, rax
  call   printf
  jmp    dfs_recursive_bt_return
dfs_recursive_bt_size1:
  mov    rdi, QWORD PTR [r12]      # If the node has 1 child then call the function and print the id
  mov    rsi, QWORD PTR [r12 + 8]
  call   dfs_recursive_inorder_btree
  mov    rdi, OFFSET fmt_tree
  mov    rsi, r13
  shr    rsi, 32
  xor    rax, rax
  call   printf
  jmp    dfs_recursive_bt_return
dfs_recursive_bt_size2:
  mov    rdi, QWORD PTR [r12]     # Same as above just print id inbetween the calls
  mov    rsi, QWORD PTR [r12 + 8]
  call   dfs_recursive_inorder_btree
  mov    rdi, OFFSET fmt_tree
  mov    rsi, r13
  shr    rsi, 32
  xor    rax, rax
  call   printf
  mov    rdi, QWORD PTR [r12 + 16]
  mov    rsi, QWORD PTR [r12 + 24]
  call   dfs_recursive_inorder_btree
dfs_recursive_bt_return:
  pop    r13
  pop    r12
  ret

# rdi - children ptr
# rsi - value|children_size
dfs_stack:
  push   r12
  push   r13
  push   r14
  sub    rsp, 16                  # Create stack
  mov    r12, rsp
  push   rsi                      # Save node to use as pointer
  push   rdi
  mov    rdi, r12
  call   get_stack                # Init stack
  mov    rdi, r12
  mov    rsi, rsp
  call   stack_push               # Push node
  mov    rdi, r12                 # Pop stack
  call   stack_pop
dfs_stack_loop:
  test   rax, rax                 # Test if stack is empty
  jz     dfs_stack_return
  mov    r13, rax
  mov    rdi, OFFSET fmt_tree     # Print id
  mov    esi, DWORD PTR [r13 + 12]
  xor    rax, rax
  call   printf
  mov    eax, DWORD PTR [r13 + 8] # Get start and end of array
  mov    r13, QWORD PTR [r13]
  lea    r14, [r13 + rax]
dfs_stack_push_child:
  cmp    r13, r14                 # Check if the pointers are the same
  je     dfs_stack_end_push
  mov    rdi, r12                 # Push node into the stack
  mov    rsi, r13
  call   stack_push
  add    r13, tree_size
  jmp    dfs_stack_push_child
dfs_stack_end_push:
  mov    rdi, r12                 # Pop stack
  call   stack_pop
  jmp    dfs_stack_loop
dfs_stack_return:
  mov    rdi, r12                 # Free stack
  call   free_stack
  add    rsp, 32
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - children ptr
# rsi - value|children_size
bfs_queue:
  push   r12
  push   r13
  push   r14
  sub    rsp, 20                  # Create queue
  mov    r12, rsp
  push   rsi                      # Save node to use as pointer
  push   rdi
  mov    rdi, r12
  call   get_queue                # Init queue
  mov    rdi, r12
  mov    rsi, rsp
  call   enqueue                  # enqueue node
  mov    eax, DWORD PTR [r12 + 8]
  mov    edi, DWORD PTR [r12 + 12]
bfs_queue_loop:
  cmp    eax, edi
  je     bfs_queue_return
  mov    rdi, r12                 # dequeue
  call   dequeue
  test   rax, rax                 # Test if queue is empty
  jz     bfs_queue_return
  mov    r13, rax
  mov    rdi, OFFSET fmt_tree     # Print id
  mov    esi, DWORD PTR [r13 + 12]
  xor    rax, rax
  call   printf
  mov    eax, DWORD PTR [r13 + 8] # Get start and end of array
  mov    r13, QWORD PTR [r13]
  lea    r14, [r13 + rax]
bfs_queue_push_child:
  cmp    r13, r14                 # Check if the pointers are the same
  je     bfs_queue_end_push
  mov    rdi, r12                 # enqueue node
  mov    rsi, r13
  call   enqueue
  add    r13, tree_size
  jmp    bfs_queue_push_child
bfs_queue_end_push:
  mov    eax, DWORD PTR [r12 + 8]
  mov    edi, DWORD PTR [r12 + 12]
  jmp    bfs_queue_loop
bfs_queue_return:
  mov    rdi, r12                 # Free queue
  call   free_queue
  add    rsp, 36
  pop    r14
  pop    r13
  pop    r12
  ret

main:
  push   r12
  push   r13
  mov    rdi, 3
  mov    rsi, 3
  call   create_tree
  mov    r12, rax
  mov    r13, rdx
  mov    rdi, rax
  mov    rsi, rdx
  call   bfs_queue
  mov    rdi, r12
  mov    rsi, r13
  mov    esi, esi
  call   free_tree
  pop    r13
  pop    r12
  ret

