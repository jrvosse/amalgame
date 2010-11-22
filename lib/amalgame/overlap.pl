:- module(ag_overlap,
	  [
	   precomputed_overlaps/1,
	   compute_overlaps/0,
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
:- use_module(amalgame(mappings/opm)).

:- setting(overlaps_persistent, boolean, false, 'Set to true if you want overlaps to survive server restarts').

%%	precomputed_overlaps(-Overlaps) is semidet.
%
%	Fails if not a single overlap alignment has been computed yet.
%	Otherwise, it will return all precomputed overlaps in the list
%	Overlaps, which is a sorted list of the form
%	[Count1:OverlapURI1, Count2:OverlapURI2 ... ],
%	where the counts indicate the number of mappings contained in
%	the overlap subset corresponding to the URI.
%
precomputed_overlaps(Overlaps) :-
	rdf(_, rdf:type, amalgame:'OverlapAlignment'),
	!, % assume overlap stats have been computed already and can be gathered from the RDF
	findall(C:G, is_precomputed_overlap(G,C), Results),
	sort(Results, Overlaps).

%%	compute_overlaps is det.
%
%	Computes for each mapping between two concepts, which alignment
%	graphs have mappings between these two concepts. Based on this,
%	the entire set of mappings is partitioned, where each mapping
%	appears in (and only in) the subset corresponding to the set of
%	all graphs which also have mappings bewteen the same concepts.
%	So for example, if a mapping M1 in alignment graph G1 defines a
%	mapping between concepts C1 and C2, and mapping M2 in graph G2
%	and mapping M3 in graph G3 also define mappings between C1 and
%	C2, we create a single new mapping M' between C1 and C2 that
%	will put in the overlap graph corresponding to the overlap of
%	G1,G2,G3.
%
compute_overlaps :-
	% Find all maps in the store:
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, L1),
	print_message(informational, map(found, maps, total, L1)),

	% For each map, find in which graphs these mappings occur:
	find_overlaps(AllMaps, [], Overlaps),
	length(Overlaps, L2),
	print_message(informational, map(found, overlaps, total, L2)),

	% Count how many mappings are in each set:
	count_overlaps(Overlaps, [], _Results).

%%	clear_overlaps is det.
%
%	Clears all overlap graphs by unloading any graph of type
%	amalgame:OverlapAlignment.
%
clear_overlaps :-
	findall(Graph,
		( rdfs_individual_of(Graph, amalgame:'OverlapAlignment'),
		  rdf_graph(Graph)
		),
		OverlapGraphs),
	forall(member(Graph, OverlapGraphs),
	       (   rdf_unload(Graph),
		   print_message(informational, map(cleared, overlap, Graph, 1))
	       )
	      ).


is_precomputed_overlap(Overlap, Count) :-
	rdf(Overlap, rdf:type, amalgame:'OverlapAlignment'),
	rdf(Overlap, amalgame:count, literal(type(_,Count))).

find_overlaps([], Doubles, Uniques) :- sort(Doubles, Uniques).
find_overlaps([Map|Tail], Accum, Out) :-
	find_graphs(Map, Graphs),
	sort(Graphs, Sorted),
	find_overlaps(Tail, [Sorted:Map|Accum], Out).

count_overlaps([], Accum, Results) :-
	assert_overlaps(Accum, [], Results).
count_overlaps([Graphs:Map|Tail], Accum, Results) :-
	overlap_uri(Graphs, Overlap),
	(   selectchk(Count:Graphs, Accum, NewAccum)
	->  true
	;   Count = 0, NewAccum = Accum,
	    print_message(informational, map(created, 'overlap graph', Overlap, 1)),
	    setting(overlaps_persistent, Persistency),
	    rdf_persistency(Overlap, Persistency),
	    rdf_bnode(Process),
	    rdf_assert(Process, rdfs:label, literal('amalgame overlap calculator'), Overlap),
	    opm_was_generated_by(Process, Overlap, Overlap,
				 [was_derived_from(Graphs)])
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




