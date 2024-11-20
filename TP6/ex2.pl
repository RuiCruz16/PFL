% Professors teaching courses
teaches(adalberto, algorithms).
teaches(bernardete, databases).
teaches(capitolino, compilers).
teaches(dalmindo, statistics).
teaches(ermelinda, networks).

% Students attending courses
attends(alberto, algorithms).
attends(alberto, compilers).
attends(antonio, databases).
attends(antonio, statistics).
attends(alvaro, networks).
attends(beatriz, networks).
attends(bernardo, compilers).
attends(bruna, algorithms).
attends(bruno, databases).
attends(bruna, statistics).
attends(claudio, statistics).
attends(claudio, networks).
attends(clara, compilers).
attends(cristina, algorithms).
attends(cristina, databases).
attends(diana, compilers).
attends(diana, networks).
attends(diogo, algorithms).
attends(duarte, databases).
attends(duarte, statistics).
attends(eduarda, algorithms).
attends(eduardo, databases).
attends(eduardo, networks).
attends(eva, statistics).
attends(eurico, compilers).

% i - Statistics
% ii - No
% iii - Statistics, Networks
% iv - No
% v - No
% vi - No

student(X,Y) ;- teaches(Y,Z), attends(X,Z), X \= Y.

students_of(X) :- teaches(X, C), attends(S, C).

teachers_of(Student) :- attends(Student, Course), teaches(Teacher, Course).

common_students(Teacher1, Teacher2, Student) :- attends(Student, Course1), attends(Student, Course2), teaches(Teacher1, Course1), teaches(Teacher2, Course2), Teacher1 \= Teacher2, Course1 \= Course2.

colleagues(X, Y) :- attends(X, C), attends(Y,C), X \= Y ; teaches(X, C), teaches(Y, C), X \= Y.

attends_more_than_1class(Student) :- attends(Student, Course1), attends(Student, Course2), Course1 \= Course2.
