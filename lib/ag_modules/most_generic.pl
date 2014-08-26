:- module(most_generic,[]).

:- use_module(library(skos/util)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof([target, source]), target,
	  'target = select most general (broader) target for each source, source = select most general source for each target').
parameter(most_least,
	  oneof([most, least]), most,
	  'select most or least generic alternative').

amalgame_module(amalgame:'MostGeneric').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%

selecter(AlignmentGraph, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, target),
	option(most_least(Most), Options, most),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, Most, AlignmentGraph, Sel, Disc, Und)
	;   predsort(ag_map:compare_align(target), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, Most, SortedAlignmentGraph, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Disc0, Disc),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).

method_name(most, most_generic).
method_name(least, least_generic).

ap(Type, Result, Most, D, align(S,T,P), align(S,T,Pnew)) :-
	method_name(Most, Method),
	append([[method(Method),
		 score([ result(Result),
			 type(Type),
			 nr_ambiguous_correspondences(D)
		       ])
		]],
	       P,
	       Pnew).

partition_(_, _, [], [], [], []).
partition_(target, Most, [A|As], Sel, Dis, Und) :-
	A = align(S,_,_),
	same_source(As, S, Same, Rest),
	length(Same, L), D is L +1,
	(   hierarchy_related(Same, D, target, Most, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [A|Same],
	    maplist(ap(target, undecided, Most, D), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Most, Rest, SelRest, DisRest, UndRest).
partition_(source, Most, [A|As], Sel, Dis, Und) :-
	A = align(_,T,_),
	same_target(As, T, Same, Rest),
	length(Same, L), D is L + 1,
	(   hierarchy_related(Same, D, source, Most, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [A|Same],
	    maplist(ap(source, undecided, Most, D), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Most, Rest, SelRest, DisRest, UndRest).

hierarchy_related([], L, Type, Most, align(S,T,P), align(S,T,Pnew), []) :-
	append([[method(most_generic),
		    score([ result(selected),
			    type(Type),
			    most(Most),
			    nr_ambiguous_correspondences(L)
			  ])
		]],P, Pnew).

hierarchy_related([A|As], L, target, Most, G0, G, [A2|Rest]) :-
	A = align(_,T,_),
	G0 = align(_,T0,_),
	(   skos_descendant_of(T0, T)
	->  (Most == most
	    ->	G1 = G0,
		A1 = A
	    ;	G1 = A,
		A1 = G0
	    )
	;   skos_descendant_of(T, T0)
	->  (Most == most
	    ->	G1 = A,
		A1 = G0
	    ;	G1 = G0,
		A1 = A
	    )
	),
	ap(target, discarded, Most, L, A1, A2),
	hierarchy_related(As, L, target, Most, G1, G, Rest).

hierarchy_related([A|As], L, source, Most, G0, G, [A2|Rest]) :-
	A = align(S,_,_),
	G0 = align(S0,_,_),
	(   skos_descendant_of(S0, S)
	->  ( Most == most
	    ->	G1 = G0,
		A1 = A
	    ;	G1 = A,
		A1 = G0
	    )
	;   skos_descendant_of(S, S0)
	->  (Most == most
	    ->	G1 = A,
		A1 = G0
	    ;	G1 = G0,
		A1 = A
	    )
	),
	ap(target, discarded, Most, L, A1, A2),
	hierarchy_related(As, L, source, Most, G1, G, Rest).


