wives([diana, elizabeth, nicole, mod]).
cigWife(diana, 3).
cigWife(elizabeth, 2).
cigWife(nicole, 4).
cigWife(mod, 1).

husbands([simon, pierre, louis, christian]).
ratio(simon, 1).
ratio(pierre, 2).
ratio(louis, 3).
ratio(christian, 4).

total(32).

solve :-
    wives(WList), husbands(HList),
    permutation(WList, [W1, W2, W3, W4]),    % Reorder wives
    ratio(simon, RS), ratio(pierre, RP), ratio(louis, RL), ratio(christian, RC),
    cigWife(W1, SW1), cigWife(W2, SW2), cigWife(W3, SW3), cigWife(W4, SW4),
    HS1 is SW1 * RS,
    HS2 is SW2 * RP,
    HS3 is SW3 * RL,
    HS4 is SW4 * RC,
    Sum is SW1 + SW2 + SW3 + SW4 + HS1 + HS2 + HS3 + HS4,
    total(Total),
    Sum =:= Total,
    write_result(W1, W2, W3, W4, [HS1,HS2,HS3,HS4]).

write_result(W1, W2, W3, W4, [H1,H2,H3,H4]) :-
    format("Simon & ~w: ~w + ~w~n", [W1, H1, cWife(W1)]),
    format("Pierre & ~w: ~w + ~w~n", [W2, H2, cWife(W2)]),
    format("Louis & ~w: ~w + ~w~n", [W3, H3, cWife(W3)]),
    format("Christian & ~w: ~w + ~w~n", [W4, H4, cWife(W4)]),
    format("Louis's wife is ~w~n", [W3]).

cWife(W) :- cigWife(W, C), format("~w", [C]).

