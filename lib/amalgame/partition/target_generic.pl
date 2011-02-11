:- module(target_generic,[]).

:- use_module(library(semweb/rdf_db)).

:- public partition/3.
:- multifile amalgame:component/2.

amalgame:component(partition, target_generic(alignment_graph, [alignment_graph], [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition(AlignmentGraph, ListOfGraphs, _Options) :-
	ListOfGraphs = [selected(Sel),
			discarded(Dis),
			undecided(Und)
		       ],
	partition_(AlignmentGraph, Sel, Dis, Und).

partition_([], [], [], []).
partition_([A|As], Sel, Dis, Und) :-
	A = align(S,_,_),
	same_source(As, S, Same, Rest),
	(   hierarchy_related(Same, A, Parent, Dis0)
	->  Sel = [Parent|SelRest],
	    append(Dis0, DisRest, Dis),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(Rest, SelRest, DisRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).


hierarchy_related([], G, G, []).
hierarchy_related([A|As], G0, G, [A1|Rest]) :-
	A = align(_,T,_),
	G0 = align(_,T0,_),
	(   rdf_reachable(T, skos:broader, T0)
	->  G1 = G0,
	    A1 = A
	;   rdf_reachable(T0, skos:broader, T)
	->  G1 = A,
	    A1 = G0
	),
	hierarchy_related(As, G1, G, Rest).
