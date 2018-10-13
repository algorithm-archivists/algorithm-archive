.intel_syntax noprefix

# System V calling convention cheatsheet
# Params: rdi, rsi, rdx, rcx, r8, r9, xmm0-7
# Return: rax (int 64 bits), rax:rdx (int 128 bits), xmm0 (float)
# Callee cleanup: rbx, rbp, r12-15
# Scratch: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11

.section .rodata
  text:       .string "bibbity bobbity"
  original:   .string "Original message: %s\n"
  encoded:    .string "Encoded message: "
  decoded:    .string "Decoded message: %s\n"

  .equ bitstr_len,       32
  .equ bitstr_size,      40
  .equ codebook_size,    256 * bitstr_size

  .equ tree_left,        0
  .equ tree_right,       8
  .equ tree_count,       16
  .equ tree_value,       20
  .equ tree_size,        24

  .equ heap_len,         0
  .equ heap_data,        4
  .equ heap_size,        512 * 8 + 16         # 512 ptrs + 4 byte length + 12 byte padding
  .equ counts_size,      256 * 4

  .equ msg_len,          0
  .equ msg_data,         8
.section .text
  .global main
  .extern printf, calloc, malloc, memset, puts

main:
  push   r12
  push   r13
  sub    rsp, codebook_size + 16              # 8 extra bytes for the Huffman-tree ptr, 8 bytes for padding

  # Print the original text
  mov    rdi, OFFSET original
  mov    rsi, OFFSET text
  xor    rax, rax
  call   printf

  # First encode the text. This will also initialize the Huffman-tree and the codebook
  mov    rdi, OFFSET text
  mov    rsi, rsp
  lea    rdx, [rsp + codebook_size]
  call   encode
  mov    r12, rax                             # Save the returned message ptr

  # Print the codebook and the encoded message
  mov    rdi, rsp
  call   print_codebook
  mov    rdi, OFFSET encoded
  xor    rax, rax
  call   printf
  mov    rdi, r12
  call   print_message

  # Decode and print the message
  mov    rdi, r12
  mov    rsi, QWORD PTR [rsp + codebook_size]
  call   decode
  mov    r13, rax
  mov    rdi, OFFSET decoded
  mov    rsi, r13
  xor    rax, rax
  call   printf

  # Free allocated resources
  mov    rdi, r12
  call   free
  mov    rdi, r13
  call   free
  mov    rdi, QWORD PTR [rsp + codebook_size]
  call   free_tree

  add    rsp, codebook_size + 16
  pop    r13
  pop    r12

  # Indiciate success with a 0 exit code
  xor    rax, rax
  ret

# rdi - text
# rsi - codebook ptr
# rdx - Huffman-tree ptr
# RET rax - encoded message ptr
encode:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi                             # Save the original arguments
  mov    r13, rsi
  mov    r14, rdx
  call   generate_tree                        # The text is already in rdi
  mov    QWORD PTR [r14], rax                 # Save the Huffman-tree's root
  mov    rdi, r13                             # Set up the parameters for codebook generation: codebook ptr, Huffman-tree root
  mov    rsi, rax
  call   generate_codebook
  xor    rax, rax
  xor    r14, r14                             # We'll use r14 to keep track of the length of the message
  mov    rcx, r12                             # Make a copy of the pointer to the message to be encoded
encode_calculate_length:
  mov    al, BYTE PTR [rcx]
  test   al, al                               # If we're at the terminating null character then we're ready to encode
  jz     encode_message
  lea    rdx, [rax + 4*rax]                   # We get the codebook entry at the specific index
  lea    r8, [r13 + 8*rdx]
  add    r14, QWORD PTR [r8 + bitstr_len]     # And add the encoded word length to the total
  inc    rcx
  jmp    encode_calculate_length
encode_message:
  mov    rdi, 1
  lea    rsi, [r14 + 7]                       # Calculate the number of bytes we need to allocate to fit all the bits
  shr    rsi, 3                               # length % 8 rounded up = (length + 8 - 1) / 8
  lea    rsi, [rsi + 8]                       # Make space for an 8-byte length field
  call   calloc                               # Allocate the necessary memory, the message will be in rax
  mov    QWORD PTR [rax], r14                 # Save the length of the message
  # Registers:
  #   - r12: text
  #   - r13: codebook_ptr
  #   - rax: message ptr
  #   - free to use: rdi, rsi, rcx, rdx, r8, r9, r10, r11, r14
  xor    r8, r8                               # Bit offset
  lea    r9, [rax + 8]                        # 8-byte message block
encode_message_bits:
  xor    rdi, rdi                             # We need to clear rdi because moving a single byte to dil doesn't do so
  mov    dil, BYTE PTR [r12]                  # Iterate the message again
  test   dil, dil                             # If we're at the the null terminator we're done
  jz     encode_done
  lea    rdx, [rdi + 4*rdi]                   # Get the codebook entry
  lea    r10, [r13 + 8*rdx]
  mov    r11, QWORD PTR [r10 + bitstr_len]    # Load the bitstring length
  lea    r14, [r10]                           # The bitstring qword we're currently processing
encode_message_bits_qword:
  mov    rdi, QWORD PTR [r14]                 # Calculate the first mask: [code qword] << [bit offset]
  mov    rsi, rdi                             # Get a second copy of the code's current qword
  mov    rcx, r8
  shl    rdi, cl
  or     QWORD PTR [r9], rdi                  # Apply the mask to the current block
  mov    rcx, 64                              # Calculate the second mask: [code qword] >> [64 - bit offset]
  sub    rcx, r8
  shr    rsi, cl
  mov    rcx, r11                             # Copy the code length so we can manipulate it without destroying the original value
  sub    rcx, 64
  jle    encode_message_bits_try_overflow     # If the length was less than or equal to 64, check if the code qword would overflow the current message block
  mov    r11, rcx                             # We wanted to subtract 64 from the code length anyway
  lea    r9, [r9 + 8]                         # Load the next message block
  or     QWORD PTR [r9], rsi                  # Save the second mask to the new message block
  jmp    encode_message_bits_qword
encode_message_bits_try_overflow:
  add    rcx, r8                              # Calculate [code length] + [bit offset] - 64
  jl     encode_calculate_new_bit_offset      # If the result is less than 0 then we have no remaining bits -> calculate the new bit offset
  mov    r8, rcx                              # Otherwise this also happens to be our new bit offset
  lea    r9, [r9 + 8]                         # Load the next message block
  or     QWORD PTR [r9], rsi                  # Save the second mask to the new message block
  inc    r12                                  # Go to the next character in the input
  jmp    encode_message_bits
encode_calculate_new_bit_offset:
  lea    r8, [r8 + r11]                       # Calculate the bit offset for the next code qword
  inc    r12
  jmp    encode_message_bits
encode_done:
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - encoded message
# rsi - Huffman-tree root (ptr)
# RET rax - the decoded message
decode:
  push   r12
  push   r13
  push   r14
  mov    r12, rdi
  mov    r13, rsi
  mov    rdi, QWORD PTR [r12]                 # Load the length of the message
  mov    r14, rdi                             # We'll use the length of the message as a loop counter later
  lea    rdi, [rdi + 1]                       # The null terminator
  call   malloc                               # This will usually be more than enough memory to contain the whole decoded message (we don't handle pathological cases right now)
  mov    rdi, r12                             # The single-character decoder doesn't touch rdi so we can hoist it before the loop
  xor    rcx, rcx
  mov    rdx, rax                             # The current byte in the output string
decode_loop:
  cmp    rcx, r14                             # The encoded message bit counter
  jge    decode_done
  mov    rsi, r13                             # The current node in the Huffman-tree
decode_loop_char:
  test   rsi, rsi                             # If the Huffman-tree node is null then we reached a dead-end -> start over
  jz     decode_loop
  cmp    QWORD PTR [rsi + tree_left], 0       # If the node has either a left or a right child, treat it as a branch
  jnz    decode_loop_char_branch
  cmp    QWORD PTR [rsi + tree_right], 0
  jnz    decode_loop_char_branch
  mov    r9d, DWORD PTR [rsi + tree_value]    # Load the value in this node in case the next iteration needs it
  mov    BYTE PTR [rdx], r9b                  # And save it to the output
  lea    rdx, [rdx + 1]                       # Advance the output string
  jmp    decode_loop
decode_loop_char_branch:
  mov    r9, rcx                              # First, load the byte of the message the current bit is in
  shr    r9, 3
  mov    r10b, BYTE PTR [rdi + r9 + msg_data]
  mov    r11, rcx                             # Save rcx in another register temporarily so we can restore it without push/pop
  and    rcx, 7
  shr    r10, cl                              # Get the bit we're interested in to position 0
  lea    rcx, [r11 + 1]                       # Restore rcx and immediately add 1 to get the next bit to decode
  and    r10, 0x1                             # Zero out all other bits
  mov    r8, rsi
  mov    rsi, QWORD PTR [r8 + tree_left]      # Take the left branch for 0, the right branch for a non-zero bit
  cmovnz rsi, QWORD PTR [r8 + tree_right]
  jmp    decode_loop_char
decode_done:
  mov    BYTE PTR [rdx], 0                    # Write the null terminator at the end of the string
  pop    r14
  pop    r13
  pop    r12
  ret

# rdi - The starting address of the codebook we want to generate
# rsi - Huffman-tree root (ptr)
generate_codebook:
  push   r12
  sub    rsp, bitstr_size + 16                # 16 extra bytes for alignment
  mov    r12, rsi
  xorps  xmm0, xmm0                           # Create a 0-initialized bitstring. This will be
  movaps XMMWORD PTR [rsp], xmm0              # used in the recursive function calls
  movaps XMMWORD PTR [rsp + 16], xmm0
  mov    QWORD PTR [rsp + 32], 0
  xor    rsi, rsi
  mov    rdx, codebook_size
  call   memset
  mov    rdi, rax
  mov    rsi, r12
  mov    rdx, rsp
  call   generate_codebook_recurse
  add    rsp, bitstr_size + 16
  pop    r12
  ret

# rdi - The codebook's starting address
# rsi - The current Huffman-tree node
# rdx - The bitstring used for code generation
generate_codebook_recurse:
  push   rbp
  push   r12
  push   r13
  test   rdi, rdi                             # If we reached a null pointer we're done
  jz     generate_codebook_recurse_done
  mov    r12, rsi
  cmp    QWORD PTR [r12 + tree_left], 0       # If at least one of the children is not null
  jnz    generate_codebook_branch             # then we need to treat the current node as a branch
  cmp    QWORD PTR [r12 + tree_right], 0
  jnz    generate_codebook_branch
  mov    r8d, DWORD PTR [r12 + tree_value]    # Get the value of the current node
  movaps xmm0, XMMWORD PTR [rdx]              # Get the values of the current bitstring into some registers
  movaps xmm1, XMMWORD PTR [rdx + 16]
  mov    r9, QWORD PTR [rdx + 32]
  lea    rax, [r8 + 4*r8]                     # The index calculation needs to add 40 * index. With lea arithmetic this can be represented as
  lea    r10, [rdi + 8*rax]                   # base address + 8 * (5 * index). This is done in two lea instructions
  movups XMMWORD PTR [r10], xmm0              # And copy the data over to it
  movups XMMWORD PTR [r10 + 16], xmm1
  mov    QWORD PTR [r10 + 32], r9
  jmp    generate_codebook_recurse_done
generate_codebook_branch:
  # First, calculate the necessary indices and bitmask to use for the bitstring
  mov    r13, QWORD PTR [rdx + bitstr_len]    # Load the current length of the bitstring
  mov    rcx, r13                             # This will be used to index into the bitstring data. We'll need two copies for it
  shr    r13, 6                               # We first get which 64 bit chunk of the bitstring we want to modify
  and    rcx, 63                              # Then the bit we want to change
  mov    rbp, 1                               # Generate the mask we'll use to set the correct bit
  shl    rbp, cl
  # We'll start with the right branch
  or     QWORD PTR [rdx + 8*r13], rbp         # Set the bit
  inc    QWORD PTR [rdx + bitstr_len]         # Increase the bitstring length
  mov    rsi, QWORD PTR [r12 + tree_right]
  call   generate_codebook_recurse
  # Now we move on to the left branch: rbx - left child, r13 - bitstring index, rbp - mask
  not    rbp
  and    QWORD PTR [rdx + 8*r13], rbp
  mov    rsi, QWORD PTR [r12 + tree_left]
  call   generate_codebook_recurse
  dec    QWORD PTR [rdx + bitstr_len]         # Decrease the bitstring length
generate_codebook_recurse_done:
  pop    r13
  pop    r12
  pop    rbp
  ret

# rdi - text
# RET rax - Huffman-tree root (ptr)
generate_tree:
  push   r12
  push   r13
  sub    rsp, 5128                            # 1024 bytes for the char counts, 4 bytes for heap length, 4096 bytes for the heap, 4 byte padding
  mov    r12, rdi                             # Save the original text so it doesn't get clobbered
  mov    rdi, rsp                             # Zero out the character counts and the heap length
  xor    rsi, rsi
  mov    rdx, 1040
  call   memset
  xor    rax, rax
generate_tree_count_chars:
  mov    al, BYTE PTR [r12]
  test   al, al
  jz     generate_tree_leaves_setup
  inc    DWORD PTR [rsp + 4*rax]
  inc    r12
  jmp    generate_tree_count_chars
generate_tree_leaves_setup:
  mov    r12, 255                             # The loop counter. We can only get here if the "test" on line 301 resulted in a zero so the next jl instruction will do the right thing
generate_tree_leaves:
  jl     generate_tree_branches               # If not then it's time to generate the branches
  mov    r13d, DWORD PTR [rsp + 4*r12]        # Load the count at the ith position
  test   r13d, r13d                           # And check if it's zero
  jz     generate_tree_leaves_counters        # If it is we can skip this iteration
  mov    rdi, 1                               # If not, we need to allocate a new leaf node
  mov    rsi, tree_size
  call   calloc
  mov    DWORD PTR [rax + tree_value], r12d   # Save the value and the count to the tree
  mov    DWORD PTR [rax + tree_count], r13d
  lea    rdi, [rsp + counts_size]             # Then push it onto the heap
  mov    rsi, rax
  call   heap_push
generate_tree_leaves_counters:
  dec    r12                                  # Decrement the loop counter and start over
  jmp    generate_tree_leaves
generate_tree_branches:
  cmp    DWORD PTR [rsp + counts_size], 1     # Check if there are still at least two elements in the heap
  jle    generate_tree_done                   # If not, we're done
  lea    rdi, [rsp + counts_size]             # Get the left child
  call   heap_pop
  mov    r12, rax
  lea    rdi, [rsp + counts_size]             # Get the right child
  call   heap_pop
  mov    r13, rax
  mov    rdi, tree_size                       # Create the new tree node, the pointer to it will be in rax
  call   malloc
  mov    ecx, DWORD PTR [r12 + tree_count]    # The new node's count: left count + right count
  add    ecx, DWORD PTR [r13 + tree_count]
  mov    QWORD PTR [rax + tree_left], r12     # Save the new node's fields: left, right, count (leave value unititialized, it shouldn't be used with branch nodes)
  mov    QWORD PTR [rax + tree_right], r13
  mov    DWORD PTR [rax + tree_count], ecx
  lea    rdi, [rsp + counts_size]             # Add the branch to the heap
  mov    rsi, rax
  call   heap_push
  jmp    generate_tree_branches
generate_tree_done:
  lea    rdi, [rsp + counts_size]             # The tree's root will be in rax after the pop
  call   heap_pop
  add    rsp, 5128
  pop    r13
  pop    r12
  ret

# rdi - heap ptr
# rsi - tree ptr
heap_push:
  lea    rax, QWORD PTR [rdi + heap_data]     # We load the heap's data ptr and length to the respective registers
  mov    ecx, DWORD PTR [rdi + heap_len]      # Load the current length
  lea    edx, [ecx + 1]                       # First, calculate the new length (length + 1)
  mov    DWORD PTR [rdi + heap_len], edx      # Then save it
  mov    QWORD PTR [rax + 8*rcx], rsi         # And finally add the new value at the end of the array
heap_push_sift_up:
  test   rcx, rcx                             # Test if we got to the root (index == 0)
  jz     heap_push_done
  lea    rdx, [rcx - 1]                       # Calculate the parent index: (index - 1) / 2
  shr    rdx, 1
  lea    r8, [rax + 8*rcx]                    # Get the pointer to the current and parent elements
  lea    r9, [rax + 8*rdx]
  mov    r10, QWORD PTR [r8]                  # Load the current and the parent elements
  mov    r11, QWORD PTR [r9]
  mov    esi, DWORD PTR [r10 + tree_count]    # Load the current tree's count
  cmp    DWORD PTR [r11 + tree_count], esi    # If parent count <= current count
  jle    heap_push_done                       # Then we're done
  mov    QWORD PTR [r8], r11                  # Otherwise swap the two elements
  mov    QWORD PTR [r9], r10
  mov    rcx, rdx
  jmp    heap_push_sift_up
heap_push_done:
  ret

# rdi - heap ptr
# RET rax - tree ptr
heap_pop:
  mov    r8d, DWORD PTR [rdi + heap_len]      # Load the heap's length
  test   r8d, r8d                             # If it's 0 then the heap's empty
  jz     heap_empty
  lea    rdx, [rdi + heap_data]               # Get the heap's data ptr
  mov    rax, QWORD PTR [rdx]                 # The return value will be the tree's current root
  lea    r8d, [r8d - 1]                       # Calculate the new length
  mov    DWORD PTR [rdi + heap_len], r8d      # And save it
  mov    rsi, QWORD PTR [rdx + 8*r8]          # Load the element we're going to swap with the root
  mov    QWORD PTR [rdx], rsi                 # Swap the root and the last element
  mov    QWORD PTR [rdx + 8*r8], rax
  xor    r9, r9                               # The loop index
heap_pop_sift_down:
  mov    rcx, r9                              # Save the target index at the start of the loop
  lea    r10, [r9 + r9 + 1]                   # The left child index
  lea    r11, [r9 + r9 + 2]                   # The right child index
  cmp    r10, r8
  jge    heap_pop_check_right
  mov    rdi, QWORD PTR [rdx + 8*r10]         # Load the left child
  mov    rsi, QWORD PTR [rdx + 8*rcx]         # Load the target
  mov    esi, DWORD PTR [rsi + tree_count]    # Load the target tree count
  cmp    DWORD PTR [rdi + tree_count], esi    # If the left tree count < target tree count
  jge    heap_pop_check_right
  mov    rcx, r10
heap_pop_check_right:
  cmp    r11, r8
  jge    heap_pop_compare_indices
  mov    rdi, QWORD PTR [rdx + 8*r11]         # Load the right child
  mov    rsi, QWORD PTR [rdx + 8*rcx]         # Load the target
  mov    esi, DWORD PTR [rsi + tree_count]    # Load the target tree count
  cmp    DWORD PTR [rdi + tree_count], esi    # If the right tree count < target tree count
  jge    heap_pop_compare_indices
  mov    rcx, r11
heap_pop_compare_indices:
  cmp    r9, rcx                              # If the target index == current index we're done
  je     heap_pop_done
  mov    rdi, QWORD PTR [rdx + 8*r9]          # Otherwise we swap the values
  mov    rsi, QWORD PTR [rdx + 8*rcx]
  mov    QWORD PTR [rdx + 8*r9], rsi
  mov    QWORD PTR [rdx + 8*rcx], rdi
  mov    r9, rcx
  jmp    heap_pop_sift_down
heap_empty:
  xor    rax, rax                             # Return a null pointer to indicate the heap was empty
heap_pop_done:
  ret

# rdi - codebook start ptr
print_codebook:
  push   rbx
  push   r12
  sub    rsp, 272                             # The bitstring we're going to print
  mov    r12, rdi
  xor    rbx, rbx                             # Save the loop counter into a register that doesn't get clobbered
print_codebook_loop:
  cmp    rbx, 255
  jg     print_codebook_done
  lea    rax, [rbx + 4*rbx]                   # We get the codebook entry at the specific index
  lea    r10, [r12 + 8*rax]
  mov    rdx, QWORD PTR [r10 + bitstr_len]    # Load the length of the bitstring
  test   rdx, rdx                             # If it's zero then the codepoint didn't exist in the original alphabet, skip
  jz     print_codebook_counters
print_codebook_char:
  mov    BYTE PTR [rsp], bl                   # First, the character we're printing the code for
  mov    WORD PTR [rsp + 1], 0x203a           # Then ": "
  mov    BYTE PTR [rsp + rdx + 3], 0x00       # At the end add the null terminator
print_codebook_generate_binary:
  dec    rdx
  jl     print_codebook_binary
  mov    r9, rdx                              # Two copies of the loop counter
  mov    rcx, rdx
  shr    r9, 6                                # Calculate the bitstring part we're going to load
  and    rcx, 63                              # The bit we're interested in
  mov    rsi, QWORD PTR [r10 + r9]            # One of the 4, 64 bit parts of the bitstring we're going to print
  shr    rsi, cl                              # Get the relevant bit into the 0th position
  and    rsi, 1                               # Mask the rest of the bits
  add    rsi, '0'                             # Convert it to ASCII
  mov    BYTE PTR [rsp + rdx + 3], sil        # And copy it into the string
  jmp    print_codebook_generate_binary
print_codebook_binary:
  mov    rdi, rsp                             # Print the current bitstring
  call   puts
print_codebook_counters:
  inc    rbx                                  # And go to the next codebook entry
  jmp    print_codebook_loop
print_codebook_done:
  add    rsp, 272
  pop    r12
  pop    rbx
  ret

# rdi - message ptr
# This would run out of stack space for long messages but it will do for now
print_message:
  push   r12
  push   r13
  mov    r12, rdi
  mov    r13, QWORD PTR [rdi]                 # Get the length of the message
  lea    rdi, [r13 + 1]                       # For the length of the string we'll need an additional the null terminator
  call   malloc
  xor    rdx, rdx
print_message_generate_string:
  cmp    rdx, r13
  jge    print_message_puts
  mov    r8, rdx                              # Get two copies of the current index
  mov    rcx, rdx
  shr    r8, 3                                # We first get the byte we want to print
  mov    r10b, BYTE PTR [r12 + r8 + msg_data]
  and    rcx, 7                               # Then the bit in that byte
  shr    r10, cl
  and    r10, 0x1                             # Mask it so only the bit we're interested in is visible
  add    r10, '0'                             # Convert it to ASCII
  mov    BYTE PTR [rax + rdx], r10b           # Write it into the printable string
  inc    rdx
  jmp    print_message_generate_string
print_message_puts:
  mov    BYTE PTR [rax + rdx], 0x00           # Write the null terminator
  mov    rdi, rax                             # And print the string
  call   puts
  pop    r13
  pop    r12
  ret

# rdi - tree ptr
free_tree:
  push   rbx
  mov    rbx, rdi
  test   rbx, rbx                             # When the tree ptr we're trying to free is already null we reached the termination condition
  jz     free_tree_done
  mov    rdi, [rbx + tree_left]               # Otherwise free the left child first
  call   free_tree
  mov    rdi, [rbx + tree_right]              # Then the right child
  call   free_tree
  mov    rdi, rbx                             # And finally, the node itself
  call   free
free_tree_done:
  pop    rbx
  ret
