:- module(most_generic,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof([target, source]), target,
	  'target = select most general (broader) target for each source, source = select most general source for each target').

amalgame_module(amalgame:'MostGeneric').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%

selecter(AlignmentGraph, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, target),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, AlignmentGraph, Sel, Disc, Und)
	;   predsort(ag_map:compare_align(target), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, SortedAlignmentGraph, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Disc0, Disc),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).

ap(Type, Result, D, align(S,T,P), align(S,T,Pnew)) :-
	append([[method(most_generic),
		 score([ result(Result),
			 type(Type),
			 nr_ambiguous_correspondences(D)
		       ])
		]],
	       P,
	       Pnew).

partition_(_, [], [], [], []).
partition_(target, [A|As], Sel, Dis, Und) :-
	A = align(S,_,_),
	same_source(As, S, Same, Rest),
	length(Same, L), D is L +1,
	(   hierarchy_related(Same, D, target, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [A|Same],
	    maplist(ap(target, undecided, D), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Rest, SelRest, DisRest, UndRest).
partition_(source, [A|As], Sel, Dis, Und) :-
	A = align(_,T,_),
	same_target(As, T, Same, Rest),
	length(Same, L), D is L + 1,
	(   hierarchy_related(Same, D, source, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [A|Same],
	    maplist(ap(source, undecided, D), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

hierarchy_related([], Type, L, align(S,T,P), align(S,T,Pnew), []) :-
	append([[method(most_generic),
		    score([ result(selected),
			    type(Type),
			    nr_ambiguous_correspondences(L)
			  ])
		]],P, Pnew).

hierarchy_related([A|As], L, target, G0, G, [A2|Rest]) :-
	A = align(_,T,_),
	G0 = align(_,T0,_),
	(   skos_descendant_of(T0, T)
	->  G1 = G0,
	    A1 = A
	;   skos_descendant_of(T, T0)
	->  G1 = A,
	    A1 = G0
	),
	ap(target, discarded, L, A1, A2),
	hierarchy_related(As, L, target, G1, G, Rest).

hierarchy_related([A|As], L, source, G0, G, [A2|Rest]) :-
	A = align(S,_,_),
	G0 = align(S0,_,_),
	(   skos_descendant_of(S0, S)
	->  G1 = G0,
	    A1 = A
	;   skos_descendant_of(S, S0)
	->  G1 = A,
	    A1 = G0
	),
	ap(target, discarded, L, A1, A2),
	hierarchy_related(As, L, source, G1, G, Rest).
