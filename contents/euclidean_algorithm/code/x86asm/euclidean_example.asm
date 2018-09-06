section .text
    global main
    extern printf

section .data
    msg db "%d", 10, 0

euclid_mod:
    push ebp
    mov ebp, esp
    push esi
    push edi
    push ebx
    sub esp, 4
    mov eax, DWORD [ebp + 8]
    sar eax, 31
    xor DWORD [ebp + 8], eax
    sub DWORD [ebp + 8], eax
    mov eax, DWORD [ebp + 12]
    sar eax, 31
    xor DWORD [ebp + 12], eax
    sub DWORD [ebp + 12], eax
    jmp .while_check
.while:
    mov eax, DWORD [ebp + 12]
    mov DWORD [ebp - 16], eax
    mov eax, DWORD [ebp + 8]
    cdq
    idiv DWORD [ebp + 12]
    mov DWORD [ebp + 12], edx
    mov eax, DWORD [ebp - 16]
    mov DWORD [ebp + 8], eax
.while_check:
    cmp DWORD [ebp + 12], 0
    jne .while
    mov eax, DWORD [ebp + 8]
    pop ebx
    pop edi
    pop esi
    leave
    ret

euclid_sub:
    push ebp
    mov ebp, esp
    push esi
    push edi
    push ebx
    sub esp, 4
    mov eax, DWORD [ebp + 8]
    sar eax, 31
    xor DWORD [ebp + 8], eax
    sub DWORD [ebp + 8], eax
    mov eax, DWORD [ebp + 12]
    sar eax, 31
    xor DWORD [ebp + 12], eax
    sub DWORD [ebp + 12], eax
    jmp .while_check
.while:
    mov eax, DWORD [ebp + 8]
    cmp eax, DWORD [ebp + 12]
    jle .if_true
    mov eax, DWORD [ebp + 12]
    sub DWORD [ebp + 8], eax
    jmp .while_check
.if_true:
    mov eax, DWORD [ebp + 8]
    sub DWORD [ebp + 12], eax
.while_check:
    mov eax, DWORD [ebp + 8]
    cmp eax, DWORD [ebp + 12]
    jne .while
    mov eax, DWORD [ebp + 8]
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
    push 5184
    push 4288
    call euclid_mod
    add esp, 8
    push eax
    push msg
    call printf
    add esp, 8
    push 9856
    push 1536
    call euclid_sub
    add esp, 8
    push eax
    push msg
    call printf
    mov eax, 0
    pop ebx
    pop edi
    pop esi
    leave
    ret
