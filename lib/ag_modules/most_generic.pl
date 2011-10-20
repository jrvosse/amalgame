:- module(most_generic,[]).

:- use_module(library(semweb/rdf_db)).
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
	;   predsort(ag_map:compare_align(targetplus), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, SortedAlignmentGraph, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(sourceplus), Sel0,  Sel),
	    predsort(ag_map:compare_align(sourceplus), Disc0, Disc),
	    predsort(ag_map:compare_align(sourceplus), Und0,  Und)
	).

partition_(_, [], [], [], []).
partition_(target, [A|As], Sel, Dis, Und) :-
	A = align(S,_,_),
	same_source(As, S, Same, Rest),
	(   hierarchy_related(Same, target, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Rest, SelRest, DisRest, UndRest).
partition_(source, [A|As], Sel, Dis, Und) :-
	A = align(_,T,_),
	same_target(As, T, Same, Rest),
	(   hierarchy_related(Same, source, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

hierarchy_related([], _, G, G, []).
hierarchy_related([A|As], target, G0, G, [A1|Rest]) :-
	A = align(_,T,_),
	G0 = align(_,T0,_),
	(   rdf_reachable(T, skos:broader, T0)
	->  G1 = G0,
	    A1 = A
	;   rdf_reachable(T0, skos:broader, T)
	->  G1 = A,
	    A1 = G0
	),
	hierarchy_related(As, G1, target, G, Rest).

hierarchy_related([A|As], source, G0, G, [A1|Rest]) :-
	A = align(S,_,_),
	G0 = align(S0,_,_),
	(   rdf_reachable(S, skos:broader, S0)
	->  G1 = G0,
	    A1 = A
	;   rdf_reachable(S0, skos:broader, S)
	->  G1 = A,
	    A1 = G0
	),
	hierarchy_related(As, source, G1, G, Rest).
