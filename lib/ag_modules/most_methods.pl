:- module(most_methods, []).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(sort)).

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

selecter(AlignmentGraph, S, D, U, Options) :-
	option(type(target), Options, target),!,
	partition_(target, AlignmentGraph, S, D, U).
selecter(AlignmentGraph, S, D, U, Options) :-
	option(type(source), Options, target),!,
	predsort(ag_map:compare_align(target), AlignmentGraph, SortedAlignmentGraph),
	partition_(source, SortedAlignmentGraph, Sel0, Disc0, Und0),
	predsort(ag_map:compare_align(source), Sel0,  S),
	predsort(ag_map:compare_align(source), Disc0, D),
	predsort(ag_map:compare_align(source), Und0,  U).

ap(Type, Result, align(S,T,P), align(S,T,Pnew)) :-
	append([[method(most_methods),
		 score([ result(Result),
			 type(Type)
		       ])
		]],
	       P,
	       Pnew).
ap(Type, Result, Most, SecondMost, align(S,T,P), align(S,T,Pnew)) :-
	append([[method(most_methods),
		 score([ result(Result),
			 type(Type),
			 most(Most),
			 second(SecondMost)])
		]],
	       P,
	       Pnew).

partition_(_, [], [], [], []).
partition_(source, [align(S,T,P)|As], Sel, Dis, Und) :-
	same_target(As, T, Same, Rest),
	(   most_methods(source, [align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [align(S,T,P)|Same],
	    maplist(ap(source, undecided), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

partition_(target, [align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   most_methods(target, [align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [align(S,T,P)|Same],
	    maplist(ap(target, undecided), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Rest, SelRest, DisRest, UndRest).

most_methods(Type, As, Selected, Discarded) :-
	group_method_count(As, Counts0),
	sort(Counts0, Sorted),
	append(T0, [SecondMost-A,Most-Selected0], Sorted),
	Most \== SecondMost,
	pairs_values(T0, T),
	maplist(ap(Type, selected, Most, SecondMost), [Selected0], [Selected]),
	maplist(ap(Type, discarded, Most, SecondMost), [A|T], Discarded).

group_method_count([], []).
group_method_count([Align|As], [Count-Align|Ts]) :-
	Align = align(_,_,Provenance),
	findall(M, (member(P,Provenance),
		    positive_result(M,P)), Methods),
	length(Methods, Count),
	group_method_count(As, Ts).

positive_result(Method, Evidence) :-
	memberchk(method(Method), Evidence),
	\+ ( memberchk(score(Score), Evidence),
	     memberchk(result(Result), Score),
	     Result \= selected
	   ).

