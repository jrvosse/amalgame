:- module(target_sibling,[]).

:- use_module(library(semweb/rdf_db)).

:- public partition/3.
:- multifile amalgame:component/2.

amalgame:component(partition, target_sibling(alignment_graph, [alignment_graph], [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition(AlignmentGraph, ListOfGraphs, Options) :-
	ListOfGraphs = [selected(Sel),
 			undecided(Und)
		       ],
	option(depth(Depth), Options, 2),
	partition_(AlignmentGraph, Depth, Sel, Und).

partition_([], _, [], []).
partition_([A|As], Depth, Sel, Und) :-
	A = align(S,T,_),
	same_source(As, S, Same, Rest),
	(   rdf_reachable(T, skos:broader, Parent, Depth, _),
	    siblings(Same, Parent, Depth)
 	->  append([A|Same], SelRest, Sel),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest
	),
	partition_(Rest, Depth, SelRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).


siblings([], _, _).
siblings([A|As], Parent, Depth) :-
	A = align(_,T,_),
	rdf_reachable(T, skos:broader, Parent, Depth, _),
	!,
	siblings(As, Parent, Depth).

