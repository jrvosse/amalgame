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

ap(Most, SecondMost, align(S,T,P), align(S,T,Pnew)) :-
	append(P, [[method(most_methods), score([most(Most), second(SecondMost)])]], Pnew).


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
	group_method_count(As, Counts0),
	sort(Counts0, Counts1),
	reverse(Counts1, [Most-Selected0,SecondMost-A|T0]),
	Most \== SecondMost,
	pairs_values(T0, T),
	maplist(ap(Most, SecondMost), [Selected0], [Selected]).


group_method_count([], []).
group_method_count([Align|As], [Count-Align|Ts]) :-
	Align = align(_,_,Provenance),
	findall(M, (member(P,Provenance),memberchk(M,P)), Methods),
	length(Methods, Count),
	group_method_count(As, Ts).
