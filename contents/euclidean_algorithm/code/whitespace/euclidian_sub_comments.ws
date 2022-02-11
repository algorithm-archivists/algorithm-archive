Reading the input: a, b
[SPACE][SPACE][SPACE][LF]            push 0
[SPACE][SPACE][SPACE][TAB][LF]       push 1
[TAB][LF][TAB][TAB]                  readi
[TAB][LF][TAB][TAB]                  readi

Loop: a, b => a, b-a
[LF][SPACE][SPACE][LF]               label_0:
[SPACE][SPACE][SPACE][LF]              push 0
[TAB][TAB][TAB]                        retrieve
[SPACE][SPACE][SPACE][TAB][LF]         push 1
[TAB][TAB][TAB]                        retrieve
[TAB][SPACE][SPACE][TAB]               sub
[SPACE][LF][SPACE]                     dup
[LF][TAB][SPACE][TAB][LF]              jmp zero label_1
[SPACE][LF][SPACE]                     dup
[LF][TAB][TAB][TAB][SPACE][LF]         jmp neg label_2
[SPACE][SPACE][SPACE][LF]              push 0
[SPACE][LF][TAB]                       swap
[TAB][TAB][SPACE]                      store
[LF][SPACE][LF][LF]                    jmp label_0

Exit when a=b
[LF][SPACE][SPACE][TAB][LF]          label_1:
[SPACE][SPACE][SPACE][LF]              push 0
[TAB][TAB][TAB]                        retrieve
[TAB][LF][SPACE][TAB]                  printi
[LF][LF][LF]                           end

If a>b: a, b => a-b, b
[LF][SPACE][SPACE][TAB][SPACE][LF]   label_2:
[SPACE][SPACE][SPACE][LF]              push 0
[SPACE][LF][TAB]                       swap
[TAB][SPACE][SPACE][TAB]               sub
[SPACE][SPACE][SPACE][TAB][LF]         push 1
[SPACE][LF][TAB]                       swap
[TAB][TAB][SPACE]                      store
[LF][SPACE][LF][LF]                    jmp label_0
