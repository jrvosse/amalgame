:-module(ag_alignment,
	 [
	  is_alignment_graph/2,
	  find_graphs/2,
	  nickname/2,
	  split_alignment/3,

	  align_get_computed_props/2,
	  align_ensure_stats/1,
	  align_clear_stats/1,
	  align_recompute_stats/1
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(edoal).
:- use_module(map).

%%	is_alignment_graph(+Graph, ?Format) is semidet.
%       is_alignment_graph(-Graph, ?Format) is nondet.
%
%	Evaluates to true if Graph is an rdf_graph/1 that contains one
%	more alignment mappings for which has_map/3 evaluates to true.
%	@tbd: current implementation assumes all alignments are in one
%	format.

is_alignment_graph(Graph, Format) :-
	ground(Graph),!,
	% Note: behaves as has_map(_,_,Graph), but this is very expensive due to the rdf/4 bug
	align_ensure_stats(format(Graph)),
	rdfs_individual_of(Graph, amalgame:'Alignment'),
	rdf(Graph, amalgame:format, literal(Format), amalgame),
	rdf_graph(Graph).

is_alignment_graph(Graph, Format) :-
	var(Graph),
	rdfs_individual_of(Graph, amalgame:'Alignment'),
	rdf_graph(Graph),
	align_ensure_stats(format(Graph)),
	rdf(Graph, amalgame:format, literal(Format), amalgame).

%%	find_graphs(+Map, -Graphs) is det.
%
%	Find all Graphs that have a mapping Map.

find_graphs(Map, Graphs) :-
	findall(Graph,
		has_map(Map, _, Graph:_),
		Graphs).


%%	align_get_computed_props(+Graph, -Props) is det.
%
%	Collect all amalgame properties Props of Graph that have been
%	computed already.

align_get_computed_props(Graph, Props) :-
	findall([PropLn, Value],
		(   rdf(Graph, Prop, Value, amalgame),
		    rdf_global_id(amalgame:PropLn, Prop)
		),
		GraphProps
	       ),
	maplist(=.., Props, GraphProps).

%%	align_ensure_stats(+Type) is det.
%
%	Ensures that alignmets statistics of type Type have been
%	computed. The following types are currently supported:
%
%	* found: makes sure that the algorithm to find all alignment
%	graphs has been run.
%	* totalcount(Graph): idem for total number of alignments in Graph
%	* mapped(Graph): idem for total number of alignments in Graph
%	* source(Graph): idem for the source of the  alignments in Graph
%	* target(Graph): idem for the source of the  alignments in Graph
%	* mapped(Graph): idem for the numbers of mapped source and target concepts in Graph
%

align_ensure_stats(found) :-
	% fixme: This can be made a lot cheaper using rdf_graph once rdf/4 has been fixed ...
	findall(Graph:[format(literal(Format))], has_map(_,Format, Graph:_), GFs),
	sort(GFs, Graphs),
	length(Graphs, GraphsFound),
	print_message(informational, map(found, graphs, total, GraphsFound)),
	assert_alignment_props(Graphs).

align_ensure_stats(format(Graph)) :-
	rdf(Graph, amalgame:format, _, amalgame), !.
align_ensure_stats(format(Graph)) :-
	has_map(_,Format, Graph:_),!,
	assert_alignment_props([Graph:[format(literal(Format))]]),!.
align_ensure_stats(format(_)) :- !.


align_ensure_stats(totalcount(Graph)) :-
	(   rdf(Graph, amalgame:count, _, amalgame)
	->  true
	;   is_alignment_graph(Graph, Format),!,
	    count_alignment(Graph, Format, Count),
	    assert_alignment_props(Graph:[count(literal(type('http://www.w3.org/2001/XMLSchema#int',Count)))])
	),!.

align_ensure_stats(source(Graph)) :-
	(   rdf(Graph, amalgame:source, _, amalgame)
	->  true
	;   is_alignment_graph(Graph, Format),!,
	    find_source(Graph, Format, Source),
	    assert_alignment_props(Graph:[source(Source)])
	),!.

align_ensure_stats(target(Graph)) :-
	(   rdf(Graph, amalgame:target, _, amalgame)
	->  true
	;   is_alignment_graph(Graph, Format),!,
	    find_target(Graph, Format, Target),
	    assert_alignment_props(Graph:[target(Target)])
	),!.


align_ensure_stats(mapped(Graph)) :-
	(   rdf(Graph, amalgame:mappedSourceConcepts, _, amalgame)
	->  true
	;   is_alignment_graph(Graph, Format),!,
	    findall(M1, has_map([M1, _], Format, Graph), M1s),
	    findall(M2, has_map([_, M2], Format, Graph), M2s),
	    sort(M1s, MappedSourceConcepts),
	    sort(M2s, MappedTargetConcepts),
	    length(MappedSourceConcepts, NrMappedSourceConcepts),
	    length(MappedTargetConcepts, NrMappedTargetConcepts),
	    assert_alignment_props(Graph:[mappedSourceConcepts(literal(type('http://www.w3.org/2001/XMLSchema#int', NrMappedSourceConcepts))),
					  mappedTargetConcepts(literal(type('http://www.w3.org/2001/XMLSchema#int', NrMappedTargetConcepts)))
					 ])
	),!.

%%	align_clear_stats(+Type) is det.
%%	align_clear_stats(-Type) is nondet.
%
%	Clears all results that have been cached after running
%	ensure_stats(Type).

align_clear_stats(all) :-
	rdf_retractall(_, _, _, amalgame),
	rdf_retractall(_, _, _, amalgame_nicknames).

align_clear_stats(found) :-
	rdf_retractall(_, rdf:type, amalgame:'Alignment', amalgame),
	rdf_retractall(_, amalgame:format, _, amalgame).

align_clear_stats(nicknames) :-
	rdf_retractall(_, _, _, amalgame_nicknames).

%%	align_recompute_stats(+Type) is det.
%%	align_recompute_stats(-Type) is nondet.
%
%	Clears and recomputes statistics of type Type. See
%	ensure_stats/1 for a list of supported types.

align_recompute_stats(Type) :-
	align_clear_stats(Type),
	align_ensure_stats(Type).


count_alignment(Graph, Format, Count) :-
	findall(Map, has_map(Map, Format, Graph), Graphs),
	length(Graphs, Count),
	print_message(informational, map(found, maps, Graph, Count)),
	!.

find_source(Graph, Format, Source) :-
	has_map([E1, _], Format, Graph),!,
	(   rdf_has(E1, skos:inScheme, Source)
	->  true
	;   iri_xml_namespace(E1, Source)
	).

find_target(Graph, Format, Target) :-
	has_map([_, E2], Format, Graph), !,
	(   rdf_has(E2, skos:inScheme, Target)
	->  true
	;   iri_xml_namespace(E2, Target)
	).

assert_alignment_props([]).
assert_alignment_props([Head|Tail]) :-
	assert_alignment_props(Head),
	assert_alignment_props(Tail),!.

assert_alignment_props(Graph:Props) :-
	rdf_equal(amalgame:'', NS),
	rdf_assert(Graph, rdf:type, amalgame:'Alignment',   amalgame),
	forall(member(M,Props),
	       (   M =.. [PropName, Value],
		   format(atom(URI), '~w~w', [NS,PropName]),
		   rdf_assert(Graph, URI, Value, amalgame)
	       )).





has_nickname(Graph,Nick) :-
	% work around bug in rdf/4
	rdf(Graph, amalgame:nickname, literal(Nick)).
	% rdf(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
nickname(Graph, Nick) :-
	has_nickname(Graph,Nick), !.
nickname(Graph, Nick) :-
	coin_nickname(Graph, Nick),
	rdf_assert(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
coin_nickname(_Graph, Nick) :-
	char_type(Nick, alpha),
	\+ has_nickname(_, Nick),!.

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
