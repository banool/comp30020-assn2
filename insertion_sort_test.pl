insertion_sort([pair([slot(_G2266874), slot(_G2266901)], [[slot(h), slot(a)]]), pair([slot(_G2266874), slot(_G2266879), slot(y)], [[slot(h), slot(e), slot(y)]]), pair([slot(_G2266901), slot(_G2266906), slot(o)], [[slot(a), slot(y), slot(o)]]), pair([slot(_G2266879), slot(_G2266906), slot(_G2266934)], [[slot(h), slot(e), slot(y)], [slot(a), slot(y), slot(o)], [slot(e), slot(y), slot(e)]])], X).

insert_sort(List, Sorted) :-
    i_sort(List, [], Sorted).

i_sort([], Acc, Acc).
i_sort([H|T], Acc, Sorted) :- 
    insert(H, Acc, NAcc),
    i_sort(T, NAcc, Sorted).
   
insert(X, [Y|T], [Y|NT]) :- 
    X > Y,
    insert(X, T, NT).

insert(X, [Y|T], [X,Y|T]) :- 
    X =< Y.
insert(X, [], [X]).