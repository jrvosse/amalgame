:- module(ag_publish,[
		     save_mappings/3
		     ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/map)).

save_mappings(Strategy, Dir, Options) :-
	provenance_graph(Strategy, ProvGraph),
	void_graph(Strategy, VoidGraph),
	(   rdf_graph(VoidGraph) -> rdf_unload(VoidGraph); true),

	make_new_directory(Dir),
	file_base_name(Strategy, StrategyB),
	file_base_name(ProvGraph, ProvGraphB),
	absolute_file_name(StrategyB,  StratFile, [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(ProvGraphB, ProvFile,  [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(void,      VoidFile,  [relative_to(Dir), extensions([ttl])]),

	select_mappings_to_be_saved(Strategy, Mappings, Options),
	forall(member(Mapping, Mappings),
	       save_mapping(Mapping, Strategy, ProvGraph, [dir(Dir)|Options])),

	rdf_save_turtle(StratFile, [graph(Strategy)|Options]),
	rdf_save_turtle(ProvFile,  [graph(ProvGraph)|Options]),
	rdf_save_turtle(VoidFile,  [graph(VoidGraph)|Options]),
	rdf_unload(VoidGraph).

void_graph(Strategy, VoidGraph) :-
	ground(Strategy),
	atomic_concat(Strategy, '_void', VoidGraph).

make_new_directory(D) :-
	(   exists_directory(D)
	->  atomic_concat(D, '/*.ttl', WildCard),
	    expand_file_name(WildCard, L),
	    forall(member(F,L), delete_file(F)),
	    delete_directory(D)
	;   true
	),
	make_directory(D).

add_relation_if_needed(Mapping, Options) :-
	rdf_graph(Mapping),
	option(default_relation(Default), Options),
	findall(Cell, (
		      rdf(Mapping, align:map, Cell, Mapping),
		      \+ rdf(Cell, align:relation, _, Mapping)
		      ), Cells),
	findall(Cell:Relation,
		(   member(Cell,Cells),
		    find_relation(Mapping, Cell, Default, Relation)
		), CellRelationPairs),
	forall(member(C:R, CellRelationPairs),
	       rdf_assert(C, align:relation, R, Mapping)
	      ).

find_relation(Mapping, Cell, Default, Relation) :-
	rdf(Cell, align:entity1, S, Mapping),
	rdf(Cell, align:entity2, T, Mapping),
	has_correspondence(align(S,T, P), Mapping),
	flatten(P, Pflat),
	option(relation(Relation), Pflat, Default), !.

save_mapping(Id, Strategy, ProvGraph, Options) :-
	(   \+ rdf_graph(Id)
	->  expand_mapping(Strategy, Id, Mapping),
	    materialize_mapping_graph(Mapping, [graph(Id)|Options])
	;   add_relation_if_needed(Id, Options)
	),
	rdf_equal(xsd:int, Int),

	void_graph(Strategy, Void),
	rdf_statistics(triples_by_file(Id, NrOfTriples)),
	assert_metadata(Id, Strategy, Void),
	rdf_assert(Id, void:vocabulary,   amalgame:'', Void),
	rdf_assert(Id, void:vocabulary,   void:'', Void),
	rdf_assert(Id, rdf:type,          void:'Linkset', Void),
	rdf_assert(Id, void:triples, literal(type(Int,NrOfTriples)), Void),

	rdf_assert(Id, amalgame:strategy, Strategy, Void),
	rdf_assert(Id, amalgame:opm,      ProvGraph, Void),

	file_base_name(Id, Base),
	option(dir(Dir), Options, tmpdir),
	absolute_file_name(Base,  Name, [relative_to(Dir), extensions([ttl])]),
	rdf_save_turtle(Name, [graph(Id)|Options]).

assert_metadata(Id, Strategy, Graph) :-
	findall(rdf(Id,P,O),
		is_metadata_triple(Id, P, O, Strategy),
		Triples),
	expand_bnode_objects(Triples, Expanded),
	forall(member(rdf(S,P,O), Expanded), rdf_assert(S,P,O,Graph)).

expand_bnode_objects([],[]).
expand_bnode_objects([rdf(S,P,O)|Tail], [rdf(S,P,O)|Expanded]) :-
	expand_bnode_objects(Tail, ExpandedTail),
	(   rdf_is_bnode(O)
	->  Bnode = O,
	    findall(rdf(Bnode, P1, O1), rdf(Bnode, P1, O1), BnodeTriples),
	    expand_bnode_objects(BnodeTriples, ExpandedBnode),
	    append(ExpandedBnode, ExpandedTail, Expanded)
	;   Expanded = ExpandedTail
	).

is_metadata_triple(S,P,O,Graph) :-
	rdf_has(S,opmv:wasGeneratedBy, Process, RP),
	rdf(S,RP,Process,Graph),
	rdf(Process, opmv:wasPerformedBy, O),
	rdf_equal(dcterms:creator, P).
is_metadata_triple(S,P,literal(type(T,N)), _Graph) :-
	rdf_has(S, amalgame:mappedSourceConcepts, literal(type(T,N))),
	rdf_equal(P, void:distinctSubjects).
is_metadata_triple(S,P,literal(type(T,N)), _Graph) :-
	rdf_has(S, amalgame:mappedTargetConcepts, literal(type(T,N))),
	rdf_equal(P, void:distinctObjects).

select_mappings_to_be_saved(Strategy, Mappings, Options) :-
	option(status(Status), Options, all),
	(   Status == all
	->  findall(Mapping,
		    (	rdfs_individual_of(Mapping, amalgame:'Mapping'),
			rdf(Mapping, rdf:type, _, Strategy)
		    ),
		    Mappings)
	;   findall(Mapping,
		    (	rdfs_individual_of(Mapping, amalgame:'Mapping'),
			rdf(Mapping, rdf:type, _, Strategy),
			rdf(Mapping, amalgame:status, Status)
		    ), Mappings)
	).


