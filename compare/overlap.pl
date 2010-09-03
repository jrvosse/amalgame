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

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

:- use_module(amalgame(mappings/map)).
:- use_module(amalgame(mappings/alignment)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(amalgame(namespaces)).



find_overlap(ResultsSorted, [cached(true)]) :-
	rdf(_, amalgame:member, _),
	!, % assume overlap stats have been computed already and can be gathered from the RDF
	findall(C:G:E, is_precomputed_overlap(G,C,E), Results),
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
	       (   rdf_retractall(_,_,_,Graph),
		   print_message(informational, map(cleared, overlap, Graph, 1))
	       )
	      ).

is_precomputed_overlap(Overlap, C, [E1,E2]) :-
	rdf(Overlap, rdf:type, amalgame:'Overlap', amalgame),
	rdf(Overlap, amalgame:count, literal(type(_,C)), amalgame),
	rdf(Overlap, amalgame:entity1, E1, amalgame),
	rdf(Overlap, amalgame:entity2, E2, amalgame).

find_overlaps([], Doubles, Uniques) :- sort(Doubles, Uniques).
find_overlaps([Map|Tail], Accum, Out) :-
	find_graphs(Map, Graphs),
	sort(Graphs, Sorted),
	find_overlaps(Tail, [Sorted:Map|Accum], Out).

count_overlaps([], Accum, Results) :-
	assert_overlaps(Accum, [], Results).
count_overlaps([Graphs:Map|Tail], Accum, Results) :-
	(   selectchk(Count:Graphs:Example, Accum, NewAccum)
	->  true
	;   Count = 0, NewAccum = Accum, Example=Map
	),
	Map = [E1, E2],
	overlap_uri(Graphs, Overlap),
	assert_cell(E1, E2, [graph(Overlap), method(overlap)]),
	NewCount is Count + 1,
	count_overlaps(Tail, [NewCount:Graphs:Example|NewAccum], Results).

overlap_uri(GraphList, URI) :-
	sort(GraphList, Sorted), assertion(GraphList=Sorted),
	term_hash(Sorted, Hash),
	rdf_equal(amalgame:'', NS),
	format(atom(URI), '~wamalgame_overlap_~w', [NS,Hash]),
	debug(uri, 'URI: ~w', [URI]).

assert_overlaps([], Accum, Accum).
assert_overlaps([C:G:E|Tail], Accum, Results) :-
	E = [E1,E2],
	overlap_uri(G, URI),
	assert_overlap_members(URI, G),
	rdf_assert(URI, rdf:type, amalgame:'Overlap', amalgame),
	rdf_assert(URI, amalgame:count, literal(type('xsd:int', C)), amalgame),
	rdf_assert(URI, amalgame:entity1, E1, amalgame),
	rdf_assert(URI, amalgame:entity2, E2, amalgame),
	assert_overlaps(Tail, [C:URI:E|Accum], Results).

assert_overlap_members(_URI, []).
assert_overlap_members(URI, [G|T]) :-
	rdf_assert(URI, amalgame:member, G, amalgame),
	assert_overlap_members(URI, T).




