% Alínea B)

:- op(1100, fx, if).     % 'if' binds looser (prefix).
:- op(1000, xfx, then).  % 'then' binds tighter (non-associative infix).
:- op(900, xfx, else).   % 'else' binds the loosest (non-associative infix).
