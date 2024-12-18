:-op(500, xfx, na).
:-op(500, yfx, la).
:-op(500, xfy, ra).

% Alínea A) a ra b na c

    ra
   /  \
  a    na
      /  \
     b    c

% Alínea B) a la b na c

% error 

% Alínea C) a na b la c

       la
      /  \
     na   c
    /  \
   a    b

% Alínea D) a na b ra c

% error

% Alínea E) a na b na c

% error

% Alínea F) a la b la c

      la
     /  \
    la   c
   /  \
  a    b

% Alínea G) a ra b ra c

      ra
     /  \
    a    ra
        /  \
       b    c

% Alínea H) a la b ra c

% error
