max(A, B, C, A) :- 
    A >= B, A >= C, !.

max(A, B, C, B) :- 
    B >= A, B >= C, !.

max(A, B, C, C) :- 
    C >= A, C >= B, !.