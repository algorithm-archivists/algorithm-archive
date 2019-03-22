euclid_sub(A, A, A) :- !.
euclid_sub(A, B, Result) :- A1 is abs(A), B1 is abs(B),
                            A1 >  B1, !, T is A1 - B1, euclid_sub(T, B1, Result).
euclid_sub(A, B, Result) :- A1 is abs(A), B1 is abs(B),
                            A1 =< B1, !, T is B1 - A1, euclid_sub(A1, T, Result).

euclid_mod(A, 0, A) :- !.
euclid_mod(A, B, Result) :- A1 is abs(A), B1 is abs(B),
                            M is mod(A1, B1), euclid_mod(B1, M, Result).

test_euclid_sub(Result) :- euclid_sub(4288, 5184, Result). % Result: 64
test_euclid_mod(Result) :- euclid_mod(1536, 9856, Result). % Result: 128 
