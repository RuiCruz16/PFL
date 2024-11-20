pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(lamb, breitling).
team(besenyei, red_bull).
team(chambliss, red_bull).
team(maclean, mediterranean_racing_team).
team(mangold, cobra).
team(jones, matador).
team(bonhomme, matador).

plane(lamb, mx2).
plane(besenyei, edge540).
plane(chambliss, edge540).
plane(maclean, edge540).
plane(mangold, edge540).
plane(jones, edge540).
plane(bonhomme, edge540).

circuit(istanbul).
circuit(budapest).
circuit(porto).

won(jones, porto).
won(mangold, budapest).
won(mangold, istanbul).

gates(istanbul, 9).
gates(budapest, 6).
gates(porto, 5).

team_wins(Team, Circuit) :- won(Pilot, Circuit), team(Pilot, Team).

more_than_1win(Pilot) :- won(Pilot, Circuit1), won(Pilot, Circuit2), Circuit1 \= Circuit2.

winner_plane(Plane, Circuit) :- plane(Pilot, Plane), won(Pilot, Circuit).

% i - Jones
% ii - Matador
% iii - Istanbul
% iv - Lamb
% v - Mangold
% vi - Edge540