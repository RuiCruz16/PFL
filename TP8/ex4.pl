% Alínea B)

list_member(Elem, List) :- % Verificamos se Elem pertence à lista `List` assumindo a mesma como o resultado final do append, dividindo-a em 2, se explicar um caso em que uma das listas começa em Elem, retorna true.
    append(_, [Elem|_], List).

% Alínea D)

list_nth(N, List, Elem) :- % Queremos o 3º elemento (Index/N = 2).
    length(Prefix, N), % Length calcula uma lista com valores que não são relevantes, porém com tamanho 2.
    append(Prefix, [Elem|_], List). % Desta forma sabemos que o `Elem` que queremos obter estará na Head da segunda lista.

% Alínea F)

list_del(List, Elem, Res) :-
    append(ListAux, [Elem|Tail], List), % Verifica se o `Elem` pertence à lista, tentando várias combinações de forma a tentar obter a lista original.
    append(ListAux, Tail, Res). % Em caso de sucesso basta retirar à combinação encontrada o `Elem`.
