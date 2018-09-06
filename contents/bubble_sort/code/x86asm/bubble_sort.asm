section .text
    global main
    extern printf
    extern putchar

section .data
    unsorted_str db "Unsorted array:", 10,0
    fmt db "%d", 10,0
    sorted_str db "Sorted array:", 10, 0

print_range:
    push ebp
    mov ebp, esp
    push esi
    push edi
    push ebx
    sub esp, 4
    mov DWORD [ebp - 16], 0
    jmp .for_check
.for_loop:
    mov eax, DWORD [ebp - 16]
    lea edx, [0 + eax * 4]
    mov eax, DWORD [ebp + 8]
    add eax, edx
    push DWORD [eax]
    push fmt
    call printf
    add esp, 8
    add DWORD [ebp - 16], 1
.for_check:
    mov eax, DWORD [ebp - 16]
    cmp eax, DWORD [ebp + 12]
    jb .for_loop
    push 10
    call putchar
    add esp, 4
    pop ebx
    pop edi
    pop esi
    leave
    ret


bubble_sort:
    push ebp
    mov ebp, esp
    push esi
    push edi
    push ebx
    sub esp, 12
    mov DWORD [ebp - 16], 0
    jmp .for_a_check
.for_a_loop:
    mov DWORD [ebp - 20], 0
    jmp .for_b_check
.for_b_loop:
    mov eax, DWORD [ebp - 20]
    lea edx, [0 + eax * 4]
    mov eax, DWORD [ebp + 8]
    add eax, edx
    mov edx, DWORD [eax]
    mov eax, DWORD [ebp - 20]
    add eax, 1
    lea ecx, [0 + eax * 4]
    mov eax, DWORD [ebp + 8]
    add eax, ecx
    mov eax, DWORD [eax]
    cmp edx, eax
    jle .if_false
    mov eax, DWORD [ebp - 20]
    lea edx, [0 + eax * 4]
    mov eax, DWORD [ebp + 8]
    add eax, edx
    mov eax, DWORD [eax]
    mov DWORD [ebp - 24], eax
    mov eax, DWORD [ebp - 20]
    add eax, 1
    lea edx, [0 + eax * 4]
    mov eax, DWORD [ebp+8]
    add eax, edx
    mov edx, DWORD [ebp - 20]
    lea ecx, [0 + edx * 4]
    mov edx, DWORD [ebp + 8]
    add edx, ecx
    mov eax, DWORD [eax]
    mov DWORD [edx], eax
    mov eax, DWORD [ebp - 20]
    add eax, 1
    lea edx, [0 + eax * 4]
    mov eax, DWORD [ebp + 8]
    add edx, eax
    mov eax, DWORD [ebp-24]
    mov DWORD [edx], eax
.if_false:
    add DWORD [ebp - 20], 1
.for_b_check:
    mov eax, DWORD [ebp + 12]
    sub eax, 1
    cmp DWORD [ebp - 20], eax
    jb .for_b_loop
    add DWORD [ebp - 16], 1
.for_a_check:
    mov eax, DWORD [ebp - 16]
    cmp eax, DWORD [ebp + 12]
    jb .for_a_loop
    pop ebx
    pop edi
    pop esi
    leave
    ret

main:
    push ebp
    mov ebp, esp
    push esi
    push edi
    push ebx
    sub esp, 44
    mov DWORD [ebp - 56], 10
    mov DWORD [ebp - 52], 1
    mov DWORD [ebp - 48], 45
    mov DWORD [ebp - 44], 756
    mov DWORD [ebp - 40], 4569
    mov DWORD [ebp - 36], 56
    mov DWORD [ebp - 32], 3
    mov DWORD [ebp - 28], 8
    mov DWORD [ebp - 24], 5
    mov DWORD [ebp - 20], -10
    mov DWORD [ebp - 16], -4
    push unsorted_str
    call printf
    add esp, 4
    push DWORD [ebp - 56]
    lea eax, [ebp - 52]
    push eax
    call print_range
    call bubble_sort
    push sorted_str
    call printf
    add esp, 4
    call print_range
    add esp, 52
    pop ebx
    pop edi
    pop esi
    leave
    ret
