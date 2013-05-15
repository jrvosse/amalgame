:-module(ag_alignment,
	 [
	  align_stat/2,
	  align_get_computed_props/2,
	  align_ensure_stats/1,
	  align_clear_stats/1,
	  align_recompute_stats/1
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

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
	rdf_graph(Graph),
	graph_format(Graph, Format).

is_alignment_graph(Graph, Format) :-
	var(Graph),
	rdfs_individual_of(Graph, amalgame:'Alignment'),
	rdf_graph(Graph),
	graph_format(Graph, Format).

graph_format(Graph, Format) :-
	rdf(Graph, amalgame:format, literal(Format), amalgame), !.
graph_format(Graph, Format) :-
	(   has_map(_,Format, Graph), !
	->  true
	;   rdfs_individual_of(Graph, amalgame:'Alignment')
	->  Format = empty
	;   fail
	).

%%	align_get_computed_props(+Graph, -Props) is det.
%
%	Collect all amalgame properties Props of Graph that have been
%	computed already.

align_get_computed_props(Graph, Props) :-
	findall([PropLn, Value],
		(   rdf(Graph, Prop, Value),
		    rdf_global_id(amalgame:PropLn, Prop)
		),
		GraphProps
	       ),
	maplist(=.., Props, GraphProps).

align_stat(format(Format), Graph) :-
	rdf(Graph, amalgame:format, Format).
align_stat(count(Count), Graph) :-
	rdf(Graph, amalgame:count, literal(type(_, Count))).
align_stat(source(Source), Graph) :-
	rdf(Graph, void:subjectsTarget, Source).
align_stat(target(Target), Graph) :-
	rdf(Graph, void:objectsTarget, Target).
align_stat(smapped(N), Graph) :-
	rdf(Graph, amalgame:mappedSourceConcepts, literal(type(_,N))).
align_stat(tmapped(N), Graph) :-
	rdf(Graph, amalgame:mappedTargetConcepts, literal(type(_,N))).

%%	align_ensure_stats(+Type) is det.
%
%	Ensures that alignmets statistics of type Type have been
%	computed. The following types are currently supported:
%
%	* found: makes sure that the algorithm to find all alignment
%	graphs has been run.
%	* count(Graph): idem for total number of alignments in Graph
%	* source(Graph): idem for the source of the  alignments in Graph
%	* target(Graph): idem for the target of the  alignments in Graph
%	* mapped(Graph): idem for the numbers of mapped source and target concepts in Graph
%

align_ensure_stats(found) :-
	align_gc_stats,
	forall(rdf_graph(Graph), classify_graph_type(Graph)).

align_ensure_stats(all(Graph)) :-
	align_ensure_stats(format(Graph)),
	align_ensure_stats(count(Graph)),
	align_ensure_stats(source(Graph)),
	align_ensure_stats(target(Graph)),
	align_ensure_stats(mapped(Graph)).


align_ensure_stats(format(Graph)) :-
	rdf(Graph, amalgame:format, _), !.
align_ensure_stats(format(Graph)) :-
	(   has_map(_,Format, Graph:_)
	->  true
	;   Format = empty
	),
	!,
	rdf_assert(Graph, amalgame:format, literal(Format), amalgame),!.

align_ensure_stats(format(_)) :- !.


align_ensure_stats(count(Graph)) :-
	rdf(Graph, amalgame:count, _),!.
align_ensure_stats(count(Graph)) :-
	(   is_alignment_graph(Graph, Format)
	->  count_alignment(Graph, Format, Count)
	;   Count = 0
	),
	rdf_equal(xsd:int, Int),
	rdf_assert(Graph, amalgame:count, literal(type(Int, Count)), amalgame),!.

align_ensure_stats(source(Graph)) :-
	rdf(Graph, void:subjectsTarget, _),!.
align_ensure_stats(source(Graph)) :-
	(   is_alignment_graph(Graph, Format)
	->  find_source(Graph, Format, Source)
	;   Source = nill
	),
	rdf_assert(Graph, void:subjectsTarget, Source, amalgame),
	!.

align_ensure_stats(target(Graph)) :-
	rdf(Graph, void:objectsTarget, _),!.
align_ensure_stats(target(Graph)) :-
	(   is_alignment_graph(Graph, Format)
	->  find_target(Graph, Format, Target)
	;   Target = nill
	),
	rdf_assert(Graph, void:objectsTarget, Target, amalgame),
	!.

align_ensure_stats(mapped(Graph)) :-
	rdf(Graph, amalgame:mappedSourceConcepts, _),!.
align_ensure_stats(mapped(Graph)) :-
	(   is_alignment_graph(Graph, Format)
	->  findall(M1, (has_map([M1, _], Format, Graph), nonvar(M1)), M1s),
	    findall(M2, (has_map([_, M2], Format, Graph), nonvar(M2)), M2s),
	    sort(M1s, MappedSourceConcepts),
	    sort(M2s, MappedTargetConcepts),
	    length(MappedSourceConcepts, NrSubjects),
	    length(MappedTargetConcepts, NrObjects)
	;   NrSubjects = 0,
	    NrObjects = 0
	),
	rdf_equal(xsd:int, Int),
	rdf_assert(Graph, amalgame:mappedSourceConcepts, literal(type(Int, NrSubjects)), amalgame),
	rdf_assert(Graph, amalgame:mappedTargetConcepts,  literal(type(Int, NrObjects)), amalgame),!.

%%	align_clear_stats(+Type) is det.
%%	align_clear_stats(-Type) is nondet.
%
%	Clears all results that have been cached after running
%	ensure_stats(Type).

align_clear_stats(all) :-
	print_message(informational, map(cleared, statistics, 1, amalgame)),
	(   rdf_graph(amalgame)
	->  rdf_unload_graph(amalgame)
	;   true
	),
	print_message(informational, map(cleared, nicknames, 1, amalgame)),
	(   rdf_graph(amalgame_nicknames)
	->  rdf_unload_graph(amalgame_nicknames)
	;   true
	).



align_clear_stats(found) :-
	rdf_retractall(_, rdf:type, amalgame:'Alignment', amalgame),
	rdf_retractall(_, amalgame:format, _, amalgame).

align_clear_stats(nicknames) :-
	rdf_retractall(_, _, _, amalgame_nicknames).

align_clear_stats(graph(Graph)) :-
	rdf_retractall(Graph, _,_,amalgame),
	rdf_retractall(Graph, _,_,amalgame_nicknames).


%%	align_recompute_stats(+Type) is det.
%%	align_recompute_stats(-Type) is nondet.
%
%	Clears and recomputes statistics of type Type. See
%	ensure_stats/1 for a list of supported types.

align_recompute_stats(Type) :-
	align_clear_stats(Type),
	align_ensure_stats(Type).

align_gc_stats :-
	findall(Graph, rdf(Graph,_,_,amalgame), Graphs),
	align_gc_stats(Graphs),
	!.

align_gc_stats([]).
align_gc_stats([Graph|Tail]):-
	align_gc_stats(Graph),
	align_gc_stats(Tail).
align_gc_stats(Graph) :-
	(   rdf_graph(Graph)
	->  true
	;   align_clear_stats(graph(Graph))
	).

count_alignment(Graph, Format, Count) :-
	findall(Map, has_map(Map, Format, Graph), Maps),
	sort(Maps, UniqueMaps),
	length(UniqueMaps, Count),
	print_message(informational, map(found, maps, Graph, Count)),
	!.

find_source(Graph, edoal, Source) :-
	rdf(_, align:onto1, Source, Graph),
	rdf_is_resource(Source),
	!.

find_source(Graph, Format, Source) :-
	has_map([E1, _], Format, Graph),!,
	(   rdf_has(E1, skos:inScheme, Source)
	->  true
	;   iri_xml_namespace(E1, Source)
	).
find_source(_, _, null).

find_target(Graph, edoal, Target) :-
	rdf(_, align:onto2, Target, Graph),
	rdf_is_resource(Target),
	!.

find_target(Graph, Format, Target) :-
	has_map([_, E2], Format, Graph),
	nonvar(E2),!,
	(   rdf_has(E2, skos:inScheme, Target)
	->  true
	;   iri_xml_namespace(E2, Target)
	).
find_target(_, _, null).


classify_graph_type(Graph) :-
	rdfs_individual_of(Graph, amalgame:'Alignment'), !.

classify_graph_type(Graph) :-
	rdfs_individual_of(Graph, amalgame:'NoAlignmentGraph'), !.

classify_graph_type(Graph) :-
	has_map(_, Format, Graph),!,
	rdf_assert(Graph, rdf:type, amalgame:'LoadedMapping', amalgame),
	rdf_assert(Graph, amalgame:format, literal(Format), amalgame).

classify_graph_type(Graph) :-
	rdf_assert(Graph, rdf:type, amalgame:'NoAlignmentGraph', amalgame).
