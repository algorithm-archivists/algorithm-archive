Reading the input: a, b
[SPACE][SPACE][SPACE][LF]       push 0
[SPACE][SPACE][SPACE][TAB][LF]  push 1
[TAB][LF][TAB][TAB]             readi
[TAB][LF][TAB][TAB]             readi

Loop: a, b => b, a%b
[LF][SPACE][SPACE][LF]          label_0:
[SPACE][SPACE][SPACE][LF]         push 0
[TAB][TAB][TAB]                   retrieve
[SPACE][LF][SPACE]                dup
[LF][TAB][SPACE][TAB][LF]         jmp zero label_1
[SPACE][SPACE][SPACE][TAB][LF]    push 1
[TAB][TAB][TAB]                   retrieve
[SPACE][LF][TAB]                  swap
[TAB][SPACE][TAB][TAB]            mod
[SPACE][SPACE][SPACE][LF]         push 0
[TAB][TAB][TAB]                   retrieve
[SPACE][SPACE][SPACE][TAB][LF]    push 1
[SPACE][LF][TAB]                  swap
[TAB][TAB][SPACE]                 store
[SPACE][SPACE][SPACE][LF]         push 0
[SPACE][LF][TAB]                  swap
[TAB][TAB][SPACE]                 store
[LF][SPACE][LF][LF]               jmp label_0

Exit when b=0
[LF][SPACE][SPACE][TAB][LF]     label_1:
[SPACE][SPACE][SPACE][TAB][LF]    push 1
[TAB][TAB][TAB]                   retrieve
[TAB][LF][SPACE][TAB]             printi
[LF][LF][LF][LF]                  end
