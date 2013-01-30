:- module(old_overlap,
	  [
	   precomputed_overlaps/1,
	   create_merge_graph/2,     %+InputGraphList, -OutputGraph(URI)
	   compute_overlaps/1,
	   clear_overlaps/0,
	   stratify_merge/1
	  ]
	 ).

/** <module> Amalgame compare mapping module: overlaps

This module compares mappings as they are found by different matchers.
It does so by computing the (lack of) overlap between the different
matchers. It assumes matchers assert mappings in different name graphs.

@author Jacco van Ossenbruggen and Victor de Boer
@license GPL
*/

:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).

:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/ag_provenance)).

:- setting(overlaps_persistent, boolean, false, 'Set to true if you want overlaps to survive server restarts').




%%	stratify_merge(+MergeGraph) is det.
%
%	extracts all alignment cells in a (merge) graph and generates
%	individual stratum graphs based on the exact number of methods
%	listed for the cells in the MergeGraph.
%
%	A stratum graph is asserted every time a new count is
%	encountered. Stratum graph names are the name of the
%	parent merge graph plus the count number
%
stratify_merge(MergeGraph):-
	findall(Cell,rdf(Cell, rdf:type, align:'Cell',MergeGraph),Cells),
	stratify_merge_cell(Cells,MergeGraph),
	true.

stratify_merge_cell([],_MergeGraph).
stratify_merge_cell([Cell|Rest],MergeGraph):-
	findall(Method,
		rdf(Cell, amalgame:'method',Method,MergeGraph),
		Methods),
	length(Methods,Count),
	strat_uri(Count,MergeGraph,StratURI),
	rdf(Cell, align:entity1,E1),
	rdf(Cell, align:entity2,E2),
	assert_cell(E1, E2, [graph(StratURI), method(Methods)]),
	stratify_merge_cell(Rest,MergeGraph).


%  Name is mergegraph name plus count, check if it already exists, if
%  not assert it
strat_uri(Count, MergeGraph, StratumGraphURI):-
	format(atom(StratumGraphURI), '~w_stratum_~w', [MergeGraph,Count]),
	(   rdf(StratumGraphURI,_,_)
	->  true;
	rdf_assert(StratumGraphURI, rdf:type, amalgame:'StratumAlignment', StratumGraphURI),
	print_message(informational, map(created, 'stratum graph', StratumGraphURI, 1)),
	setting(overlaps_persistent, Persistency),
        rdf_persistency(StratumGraphURI, Persistency),
	rdf_bnode(Process),
	rdf_assert(Process, rdfs:label, literal('amalgame stratum calculator'), StratumGraphURI),
	opm_was_generated_by(Process, StratumGraphURI,StratumGraphURI,
				 [was_derived_from([MergeGraph])])).


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

%%	compute_overlaps(+Request) is det.
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
compute_overlaps(Request) :-
	% Find all maps in the store:
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, L1),
	print_message(informational, map(found, maps, total, L1)),

	% For each map, find in which graphs these mappings occur:
	find_overlaps(AllMaps, [], Overlaps),
	length(Overlaps, L2),
	print_message(informational, map(found, overlaps, total, L2)),

	% Count how many mappings are in each set:
	count_overlaps(Request, Overlaps, [], _Results),

	% VIC: And create the minimal count overlap sets
	produce_overlap_counts(Overlaps,2).

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

count_overlaps(_, [], Accum, Results) :-
	assert_overlaps(Accum, [], Results).
count_overlaps(Request, [Graphs:Map|Tail], Accum, Results) :-
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
				 [was_derived_from(Graphs), request(Request)])
	),
	Map = [E1, E2],
	findall(Method,
		(   member(G, Graphs),
		    has_map(Map, edoal, Options, G),
		    memberchk(method(Method), Options)
		),
		Methods),
	assert_cell(E1, E2, [graph(Overlap), method(Methods)]),

	NewCount is Count + 1,
	count_overlaps(Request, Tail, [NewCount:Graphs|NewAccum], Results).

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





% VIC: calculates overlap-sets with minimum number of overlapping
% mappings

produce_overlap_counts(OverlapList,Min) :-
	findall( GraphList:Map, (
		       member(GraphList:Map,OverlapList),
		       length(GraphList,NoGraphs),
		       NoGraphs >= Min),
		 MappingList),
	(   MappingList \= []
	->
	print_message(informational, map(occurs_min(Min, MappingList))),
%	assert_overlap_count_graph(MappingList, Min),
	Min1 is Min + 1,
	produce_overlap_counts(OverlapList,Min1)
	;
	true).



% ---------- Merge stuff ----------------
%
%

%%	create_merge_graph(+InputGrapList:list, -MergeGraphURI:atom) is
%%	det.
%
%	asserts a graph which is the results of the merge (Union) of all
%	graphs in inputGraphList.

create_merge_graph(InputGraphList, MergeGraphURI):-
	merge_uri(InputGraphList,MergeGraphURI),

	findall(Map, map_iterator(Map,InputGraphList), AllMaps),
	%find_overlaps(AllMaps, [], Overlaps),
	sort(AllMaps,SortMaps),

	length(AllMaps,C),
	rdf_assert(MergeGraphURI, rdf:type, amalgame:'MergeAlignment', MergeGraphURI),
	rdf_assert(MergeGraphURI, amalgame:count, literal(type('xsd:int', C)), amalgame),

	print_message(informational, map(created, 'merge graph', MergeGraphURI, 1)),
	setting(overlaps_persistent, Persistency),
        rdf_persistency(MergeGraphURI, Persistency),
	rdf_bnode(Process),
	rdf_assert(Process, rdfs:label, literal('amalgame merge calculator'), MergeGraphURI),
	opm_was_generated_by(Process, MergeGraphURI,MergeGraphURI,
				 [was_derived_from(InputGraphList)]),

	assert_merges(SortMaps,InputGraphList,MergeGraphURI),
	true.


assert_merges([],_,_).
assert_merges([[E1,E2]|RestOverlaps],Graphs,MergeGraphURI):-

	findall(Method,(
		        has_map([E1,E2],_Format,Graph:_),
			member(Graph,Graphs),
			rdf(Bnode,align:entity1,E1,Graph),
			rdf(Bnode,align:entity2,E2,Graph),
			rdf(Bnode,amalgame:method,Method)
			),
		Methods),
	assert_cell(E1, E2, [graph(MergeGraphURI),method(Methods)]),
	assert_merges(RestOverlaps,Graphs,MergeGraphURI).

merge_uri(GraphList, URI):-
	sort(GraphList, Sorted), % assertion(GraphList=Sorted),
	term_hash(Sorted, Hash),
	rdf_equal(amalgame:'', NS),
	format(atom(URI), '~wamalgame_merge_~w', [NS,Hash]),
	debug(uri, 'URI: ~w', [URI]).
