:- module(map_merger, [
	      merger/3   % also used by other modules
	  ]).

:- use_module(library(lists)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public merger/3.

amalgame_module(amalgame:'MapMerger').

%%      merge_mappings(+ListOfGraphs, -Merged)
%
%       Merge alignment terms. ListOfGraphs is ordered.

merger([], [], _) :- !.
merger([L], L, _) :- !.
merger([[]|Tail], Merged, Options) :- merger(Tail, Merged, Options).
merger([L|Ls], [Merged|Ms], Options) :-
        smallest_heads(Ls, [L], Heads, Rest),
        merge_provenance(Heads, [Merged]),
        merger(Rest, Ms, Options).


smallest_heads([], Smallest, Heads, Tails) :-
        heads_tails(Smallest, Heads, Tails).
smallest_heads([[]|Ls1], Ls0, Smallest, Rest) :-
        !,
        smallest_heads(Ls1, Ls0, Smallest, Rest).
smallest_heads([L1|Ls1], [L0|Ls0], Smallest, Rest) :-
        L1 = [align(S1,T1,_)|_],
        L0 = [align(S0,T0,_)|_],
        (   S1 == S0,
            T1 == T0
        ->  smallest_heads(Ls1, [L0,L1|Ls0], Smallest, Rest)
        ;   (   (S1 == S0, compare(<, T1, T0))
            ;   compare(<, S1, S0)
            )
        ->  smallest_heads(Ls1, [L1], Smallest, Rest0),
            append([L0|Ls0], Rest0, Rest)
        ;   Rest = [L1|Rest0],
            smallest_heads(Ls1, [L0|Ls0], Smallest, Rest0)
        ).


heads_tails([], [], []).
heads_tails([[H]|Ls], [H|Hs], Ts) :-
        !,
        heads_tails(Ls, Hs, Ts).
heads_tails([[H1|T1]|Ls], [H1|Hs], [T1|Ts]) :-
        heads_tails(Ls, Hs, Ts).
