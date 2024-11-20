translation(1, 'Integer Overflow').
translation(2, 'Division by zero').
translation(3, 'ID Unknown').

translate(Code, Meaning) :- translation(Code, Meaning).
