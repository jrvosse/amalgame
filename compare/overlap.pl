:- module(ag_overlap,
	  [
	   find_overlap/2,
	   clear_overlaps/0
	  ]
	 ).

/** <module> Amalgame compare mapping module: overlaps

This module compares mappings as they are found by different matchers.
It does so by computing the (lack of) overlap between the different
matchers. It assumes matchers assert mappings in different name graphs.

@author Jacco van Ossenbruggen
@license GPL
*/

:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).

:- use_module(amalgame(mappings/map)).
:- use_module(amalgame(mappings/alignment)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(amalgame(namespaces)).

:- setting(overlaps_persistent, boolean, false, 'Set to true if you want overlaps to survive server restarts').

find_overlap(ResultsSorted, [cached(true)]) :-
	rdf(_, rdf:type, amalgame:'OverlapAlignment'),
	!, % assume overlap stats have been computed already and can be gathered from the RDF
	findall(C:G, is_precomputed_overlap(G,C), Results),
	sort(Results, ResultsSorted).

find_overlap(ResultsSorted, [cached(false)]) :-
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, L1),
	print_message(informational, map(found, maps, total, L1)),
	find_overlaps(AllMaps, [], Overlaps),
	length(Overlaps, L2),
	print_message(informational, map(found, overlaps, total, L2)),
	count_overlaps(Overlaps, [], Results),
	sort(Results, ResultsSorted).

clear_overlaps :-
	forall(
		(   rdf_graph(Graph),
		    sub_atom(Graph,_,_,_,'amalgame_overlap')
		),
	       (   rdf_unload(Graph),
		   print_message(informational, map(cleared, overlap, Graph, 1))
	       )
	      ).

is_precomputed_overlap(Overlap, C) :-
	rdf(Overlap, rdf:type, amalgame:'OverlapAlignment'),
	rdf(Overlap, amalgame:count, literal(type(_,C))).

find_overlaps([], Doubles, Uniques) :- sort(Doubles, Uniques).
find_overlaps([Map|Tail], Accum, Out) :-
	find_graphs(Map, Graphs),
	sort(Graphs, Sorted),
	find_overlaps(Tail, [Sorted:Map|Accum], Out).

count_overlaps([], Accum, Results) :-
	assert_overlaps(Accum, [], Results).
count_overlaps([Graphs:Map|Tail], Accum, Results) :-
	setting(overlaps_persistent, Persistency),
	overlap_uri(Graphs, Overlap),
	(   selectchk(Count:Graphs, Accum, NewAccum)
	->  true
	;   Count = 0, NewAccum = Accum,
	    rdf_persistency(Overlap, Persistency)
	),
	Map = [E1, E2],
	(   Graphs=[G], has_map([E1, E2], edoal, Options, G)
	->  assert_cell(E1, E2, [graph(Overlap)|Options])
	;   assert_cell(E1, E2, [graph(Overlap), method(overlap)])
	),
	NewCount is Count + 1,
	count_overlaps(Tail, [NewCount:Graphs|NewAccum], Results).

overlap_uri(GraphList, URI) :-
	sort(GraphList, Sorted), assertion(GraphList=Sorted),
	term_hash(Sorted, Hash),
	rdf_equal(amalgame:'', NS),
	format(atom(URI), '~wamalgame_overlap_~w', [NS,Hash]),
	debug(uri, 'URI: ~w', [URI]).

assert_overlaps([], Accum, Accum).
assert_overlaps([C:G|Tail], Accum, Results) :-
	overlap_uri(G, URI),
	assert_overlap_members(URI, G),
	rdf_assert(URI, rdf:type, amalgame:'OverlapAlignment', URI),
	rdf_assert(URI, amalgame:count, literal(type('xsd:int', C)), amalgame),
	assert_overlaps(Tail, [C:URI|Accum], Results).

assert_overlap_members(_URI, []).
assert_overlap_members(URI, [G|T]) :-
	rdf_assert(URI, amalgame:member, G, URI),
	assert_overlap_members(URI, T).




