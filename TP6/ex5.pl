job(technician, eleuterio). % The job of Eleuterio is Technician
job(technician, juvenaldo).
job(analyst, leonilde).
job(analyst, marciliano).
job(engineer, osvaldo).
job(engineer, porfirio).
job(engineer, reginaldo).
job(supervisor, sisnando).
job(chief_supervisor, gertrudes).
job(secretary, felismina).
job(director, asdrubal).
supervised_by(technician, engineer). % Technician supervised by engineer
supervised_by(engineer, supervisor).
supervised_by(analyst, supervisor).
supervised_by(supervisor, chief_supervisor).
supervised_by(chief_supervisor, director).
supervised_by(secretary, director).

% supervised_by(analyst, _X), job(_X, sisnando). - Is there a person named Sisnando who has a job that supervises analysts? - True

% supervised_by(technician, _X), supervised_by(_X, Y). - Who supervises technicians directly (_X) and who supervises that supervisor (Y)? - Supervisor

% job(J, P), supervised_by(J, supervisor). - Which people (P) and their corresponding jobs (J) are supervised by a supervisor? - (Technician, Eleuterio), (Technician, Juvenaldo), (Analyst, Leonilde), (Analyst, Marciliano), (Engineer, Osvaldo), (Engineer, Porfirio), (Engineer, Reginaldo)

% job(_J, asdrubal), supervised_by(_S, _J), job(_S, P). - Who (P) is supervised by Asdrubal, the director? - Gertrudes, Felismina

direct_supervised(X,Y) :- job(JX, X), job(JY,Y), supervised_by(JX, JY), X \= Y.

supervised_by_same_job(X,Y) :- job(JX, X), job(JY,Y), supervised_by(JX, SX), supervised_by(JY, SY), SX = SY. % Bastaria verificar JX = JY, porque para serem supervisionados pelo mesmo cargo, X e Y têm de ter o mesmo cargo

responsible_more_than_1job(X) :- job(JX, X), supervised_by(S1, JX), supervised_by(S2, JX), S1 \= S2.

supervisor_of_supervisor(X,Y) :- job(JX,X), job(JY,Y), supervised_by(JY, SY), supervised_by(SY, JX), X \= Y.