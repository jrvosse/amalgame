:- module(compare,
	  [
	   clear_stats/0,
	   clear_nicknames/0,
	   nickname/2,
	   find_overlap/2,
	   find_alignment_graphs/2,
	   split_alignment/3,

	   % misc comparison predicates:
	   map_iterator/1,	   % -Map
	   has_map/3,              % +Map, -Format -Graph
	   find_graphs/2           % +Map, -GraphList

	  ]
	 ).

/** <module> Amalgame compare mapping module

This module compares mappings as they are found by different matchers.
It assumes matchers assert mappings in different name graphs.

@author Jacco van Ossenbruggen
@license GPL
*/

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

:- use_module('../edoal/edoal').
:- use_module('../namespaces').





%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps to be compared. Map is currently of the
%	form [C1, C2], simply meaning there is a mapping from C1 to C2.
%	What other information is available about this mapping depends
%	on the format it is stored in, see has_map/3 for details.
%
%	This is a stub implementation.
%	@tbd make this configurable over a web interface so that we can
%	restrict the source and target vocabulary.

map_iterator([E1,E2]) :-
	has_map([E1, E2], _, _).

%%	has_map(+Map, -Format, -Graph) is non_det.
%
%	Intended to be used to find graphs that contain Map, and in what
%	Format. Map can be stored in the triple store in several
%	formats. We currently support the following formats:
%
%	* edoal: Alignment map format (EDOAL)
%	* skos:  SKOS Mapping Relation
%       * dc:    dc:replaces
%       * owl:   owl:sameAs
%
%	@see EDOAL: http://alignapi.gforge.inria.fr/edoal.html

has_map([E1, E2], edoal, Graph) :-
	(   ground(E1)
	->  rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	;   ground(E2)
	->  rdf(Cell, align:entity2, E2, Graph),
	    rdf(Cell, align:entity1, E1, Graph)
	;   var(Graph)
	->  rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	;   rdf(Cell, align:entity1, E1),
	    rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	).

has_map([E1, E2], skos, Graph) :-
	rdf_has(E1, skos:mappingRelation, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], dc, Graph) :-
	rdf_has(E1, dcterms:replaces, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], owl, Graph) :-
	rdf_has(E1, owl:sameAs, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

%%	find_graphs(+Map, -Graphs) is det.
%
%	Find all Graphs that have a mapping Map.

find_graphs(Map, Graphs) :-
	findall(Graph,
		has_map(Map, _, Graph:_),
		Graphs).

count_alignments(Format, Graph, Count) :-
	findall(Map, has_map(Map, Format, Graph), Graphs),
	length(Graphs, Count),
	print_message(informational, map(found, maps, Graph, Count)),
	!.

count_alignments(_,_,-1).

find_overlap(ResultsSorted, [cached(true)]) :-
	rdf(_, amalgame:member, _),
	!, % assume overlap stats have been computed already and can be gathered from the RDF
	findall(C:G:E, is_overlap(G,C,E), Results),
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

is_overlap(Overlap, C, [E1,E2]) :-
	rdf(Overlap, rdf:type, amalgame:'Overlap', amalgame),
	rdf(Overlap, amalgame:count, literal(C), amalgame),
	rdf(Overlap, amalgame:entity1, E1, amalgame),
	rdf(Overlap, amalgame:entity2, E2, amalgame).

find_overlaps([], Doubles, Uniques) :- sort(Doubles, Uniques).
find_overlaps([Map|Tail], Accum, Out) :-
	find_graphs(Map, Graphs),
	find_overlaps(Tail, [Graphs:Map|Accum], Out).

count_overlaps([], Accum, Results) :-
	assert_overlaps(Accum, [], Results).
count_overlaps([Graphs:Map|Tail], Accum, Results) :-
	(   selectchk(Count:Graphs:Example, Accum, NewAccum)
	->  true
	;   Count = 0, NewAccum = Accum, Example=Map
	),
	NewCount is Count + 1,
	count_overlaps(Tail, [NewCount:Graphs:Example|NewAccum], Results).

assert_overlaps([], Accum, Accum).
assert_overlaps([C:G:E|Tail], Accum, Results) :-
	E = [E1,E2],
	term_hash(G, Hash),
	rdf_equal(amalgame:'', NS),
	format(atom(URI), '~wamalgame_overlap_~w', [NS,Hash]),
	debug(uri, 'URI: ~w', [URI]),
	assert_overlap_members(URI, G),
	rdf_assert(URI, rdf:type, amalgame:'Overlap', amalgame),
	rdf_assert(URI, amalgame:count, literal(C), amalgame),
	rdf_assert(URI, amalgame:entity1, E1, amalgame),
	rdf_assert(URI, amalgame:entity2, E2, amalgame),
	assert_overlaps(Tail, [C:URI:E|Accum], Results).

assert_overlap_members(_URI, []).
assert_overlap_members(URI, [G|T]) :-
	rdf_assert(URI, amalgame:member, G, amalgame),
	assert_overlap_members(URI, T).

clear_stats :-
	rdf_retractall(_, _, _, amalgame).
clear_nicknames :-
	rdf_retractall(_, _, _, amalgame_nicknames).

has_nickname(Graph,Nick) :-
	% work around bug in rdf/4
	% rdf(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
	rdf(Graph, amalgame:nickname, literal(Nick)).
nickname(Graph, Nick) :-
	has_nickname(Graph,Nick), !.
nickname(Graph, Nick) :-
	coin_nickname(Graph, Nick),
	rdf_assert(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
coin_nickname(_Graph, Nick) :-
	char_type(Nick, alpha),
	\+ has_nickname(_, Nick).


collect_props_from_rdf(Graph, Count, Props) :-
	rdf(Graph, amalgame:count, literal(Count), amalgame),
	findall([PropLn, Value],
		(   rdf(Graph, Prop, Value, amalgame),
		    rdf_global_id(amalgame:PropLn, Prop)
		),
		GraphProps
	       ),
	maplist(=.., Props, GraphProps).

find_alignment_graphs(SortedGraphs, [cached(true)]) :-
	rdf(_, rdf:type, amalgame:'Alignment', amalgame),
	!,
	findall(Count:Graph:Props,
		(   rdf(Graph, rdf:type, amalgame:'Alignment', amalgame),
		    collect_props_from_rdf(Graph, Count, Props)
		),
		Graphs
	       ),
	sort(Graphs, SortedGraphs).

find_alignment_graphs(SortedGraphs, [cached(fail)]) :-
	findall(Format:Graph,
		has_map(_, Format,Graph:_),
		DoubleGraphs),
	sort(DoubleGraphs, Graphs),
	findall(Count:Graph:Props,
		(   member(Format:Graph, Graphs),
		    find_align_props(Format, Graph, Props),
		    count_alignments(Format, Graph, Count)
		),
		CountedGraphs),
	sort(CountedGraphs, SortedGraphs),
	assert_alignments(SortedGraphs).

mapped_concepts(Format, Graph, MapCounts) :-
	findall(M1, has_map([M1, _], Format, Graph), M1s),
	findall(M2, has_map([_, M2], Format, Graph), M2s),
	sort(M1s, MappedSourceConcepts),
	sort(M2s, MappedTargetConcepts),
	length(MappedSourceConcepts, NrMappedSourceConcepts),
	length(MappedTargetConcepts, NrMappedTargetConcepts),
	MapCounts = [
		     mappedSourceConcepts(literal(NrMappedSourceConcepts)),
		     mappedTargetConcepts(literal(NrMappedTargetConcepts))
		    ].

find_align_props(edoal, Graph, Props) :-
	rdf(Alignment, rdf:type, align:'Alignment', Graph),
	rdf(Alignment, align:onto1, O1, Graph),
	rdf(Alignment, align:onto2, O2, Graph),
	Props1 = [
		 format(literal(edoal)),
		 alignment(Alignment),
		 source(O1),
		 target(O2)
		],
	!,
	mapped_concepts(edoal, Graph, MapCounts),
	append(Props1, MapCounts, Props).

find_align_props(Format, Graph, Props) :-
	Props1 = [
		 format(literal(Format)),
		 source(Source),
		 target(Target)
		],
	has_map([E1, E2], Format, Graph),
	(   rdf_has(E1, skos:inScheme, Source)
	->  true
	;   iri_xml_namespace(E1, Source)
	),
	(   rdf_has(E2, skos:inScheme, Target)
	->  true
	;   iri_xml_namespace(E2, Target)
	),
	!,

	mapped_concepts(Format, Graph, MapCounts),
	append(Props1, MapCounts, Props).

assert_alignments([]).
assert_alignments([Count:Graph:Props|Tail]) :-
	rdf_equal(amalgame:'', NS),
	rdf_assert(Graph, rdf:type, amalgame:'Alignment',   amalgame),
	rdf_assert(Graph, amalgame:count,  literal(Count),  amalgame),
	forall(member(M,Props),
	       (   M =.. [PropName, Value],
		   format(atom(URI), '~w~w', [NS,PropName]),
		   rdf_assert(Graph, URI, Value, amalgame)
	       )),
	assert_alignments(Tail).




split_alignment(SourceGraph, Condition, SplittedGraphs) :-
	findall(Map:Format, has_map(Map, Format, SourceGraph), Maps),
	reassert(Maps, SourceGraph, Condition, [], SplittedGraphs).

reassert([], _ , _, Graphs, Graphs).
reassert([Map:_Format|Tail], OldGraph, Condition, Accum, Results) :-
	target_graph(Map, OldGraph, Condition, NewGraph),
	Map = [E1,E2],
	Options = [graph(NewGraph),
		   method(OldGraph)
		  ],
	assert_cell(E1, E2, Options),
	reassert(Tail, OldGraph, Condition, Accum, Results).

target_graph([E1, E2], OldGraph, Condition, Graph) :-
	(   Condition = sourceType
	->  findall(Type, rdf(E1, rdf:type, Type), Types)
	;   findall(Type, rdf(E2, rdf:type, Type), Types)
	),
	sort(Types, STypes),
	rdf_equal(skos:'Concept', SkosConcept),
	(   selectchk(SkosConcept, STypes, GTypes)
	->  true
	;   GTypes = STypes
	),
	GTypes = [FirstType|_],
	format(atom(Graph), '~p_~p', [OldGraph, FirstType]).


prolog:message(map(found,What, From, Number)) -->
        [
          'Found ', Number, ' ', What, ' (', From, ') to process'
        ].

