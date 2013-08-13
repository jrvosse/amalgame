:- module(most_methods, []).

:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof([target, source]), target,
	  'target = select target with most labels for each source,
	   source = select source with most labels for each target').

amalgame_module(amalgame:'MostMethods').

%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%       Source is a sorted list of alignment terms.
%       TODO: add source/target option

selecter(AlignmentGraph, S, D, U, _Options) :-
	partition_(AlignmentGraph, S, D, U).

partition_([], [], [], []).
partition_([align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   most_methods([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(Rest, SelRest, DisRest, UndRest).

most_methods(As, Selected, [A|T]) :-
	group_method_count(As, Counts),
	sort(Counts, [N-Selected,N1-A|T0]),
	pairs_values(T0, T),
	\+ N == N1.

group_method_count([], []).
group_method_count([Align|As], [Count-Align|Ts]) :-
	Align = align(_,_,Provenance),
	findall(M, (member(P,Provenance),memberchk(M,P)), Methods),
	length(Methods, Count0),
	Count is 1/Count0,
	group_method_count(As, Ts).
