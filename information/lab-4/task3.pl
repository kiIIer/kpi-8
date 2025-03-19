domain([p, a, f, i]).

solve :-
    domain(Names), % [p,a,f,i]
    domain(Surnames),
    permutation(Surnames, [S1,S2,S3,S4]),
    Students = [(p,S1),(a,S2),(f,S3),(i,S4)],

    all_distinct_pairs(Students),

    check_surname_a(Students),

    member((p, SurOfP), Students),
    member((SurOfP, SurOfSurOfP), Students),
    member((NameWithF, f), Students),
    NameWithF = SurOfSurOfP,

    writeln(Students),
    fail ; true.

all_distinct_pairs([]).
all_distinct_pairs([(N,S)|T]) :-
    N \= S,
    all_distinct_pairs(T).

check_surname_a([]).
check_surname_a([(N,a)|T]) :- N \= i, check_surname_a(T).
check_surname_a([(_,S)|T]) :- S \= a, check_surname_a(T).
