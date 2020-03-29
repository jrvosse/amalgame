:- module(ag_publish,[
		     save_results/3
		     ]).

:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_turtle_write)).

:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_evaluation)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/vocabulary)).

:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/edoal)).

save_results(Strategy, Dir, Options) :-
	delete_empty_eval_graphs(Strategy), % good time for some cleanup...

	provenance_graph(Strategy, ProvGraph),
	void_graph(Strategy, VoidGraph),
	(   rdf_graph(VoidGraph) -> rdf_unload_graph(VoidGraph); true),

	make_new_directory(Dir),
	file_base_name(Strategy, StrategyB),
	file_base_name(ProvGraph, ProvGraphB),
	absolute_file_name(StrategyB,  StratFile, [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(ProvGraphB, ProvFile,  [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(void,       VoidFile,  [relative_to(Dir), extensions([ttl])]),

	rdf_save_canonical_turtle(StratFile, [graph(Strategy)|Options]),

	assert_master_void(Strategy, AllMappingsURI, VoidGraph),

	rdf_equal(void:'Dataset', VoidDataset),
	rdf_equal(void:'Linkset', VoidLinkset),

	select_schemes_to_be_saved(Strategy, Schemes, Options),
	forall(member(Scheme, Schemes),
	       save_scheme(Scheme, [strategy(Strategy),
				    all_mappings(AllMappingsURI),
				    dir(Dir),
				    type(VoidDataset),
				    prov(ProvGraph)|Options])
	      ),

	select_mappings_to_be_saved(Strategy, Mappings, Options),
	forall(member(Mapping, Mappings),
	       save_mapping(Mapping, [strategy(Strategy),
				      all_mappings(AllMappingsURI),
				      type(VoidLinkset),
				      dir(Dir),
				      prov(ProvGraph)|Options])
	      ),


	rdf_save_canonical_turtle(ProvFile,  [graph(ProvGraph)|Options]),
	(   Mappings \= []
	->  rdf_save_canonical_turtle(VoidFile, [graph(VoidGraph)|Options])
	;   true
	),
	rdf_unload_graph(VoidGraph).

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

add_relation_where_needed(Mapping, Default) :-
	findall(Cell, (
		      rdf(Mapping, align:map, Cell, Mapping),
		      \+ rdf(Cell, align:relation, _, Mapping)
		      ), Cells),
	findall(Cell:Relation,
		(   member(Cell,Cells),
		    find_relation(Mapping, Cell, Default, Relation)
		), CellRelationPairs),
	forall(member(C:R, CellRelationPairs),
	       (   rdf_assert(C, align:relation, R, Mapping),
		   debug(publish, 'Asserting relation ~p', [R])
	       )
	      ).

find_relation(Mapping, Cell, Default, Relation) :-
	rdf(Cell, align:entity1, S, Mapping),
	rdf(Cell, align:entity2, T, Mapping),
	has_correspondence(align(S,T, P), Mapping),
	flatten(P, Pflat),
	option(relation(Relation), Pflat, Default), !,
	Relation \= none.

assert_master_void(Strategy, URI, Graph) :-
	rdf_display_label(Strategy, Label),
	format(atom(Title), '~w full link set', [Label]),
	rdf_has(Strategy, amalgame:publish_ns, NS),
	rdf_graph_property(Strategy, hash(GraphHash)),
	variant_sha1(term(Strategy, Title, GraphHash), VoidHash),
	atom_concat(NS, VoidHash, URI),
	rdf_assert(URI, rdf:type, void:'Linkset', Graph),
	rdf_assert(URI, amalgame:hasPlan,  Strategy, Graph),
	rdf_assert(URI, dcterms:title, Title@en, Graph),
	(   rdf_has(Strategy, rdfs:comment, Comment)
	->  rdf_assert(URI, dcterms:description, Comment, Graph)
	;   true
	).

assert_void(Id,Options) :-
	option(format(Format), Options, simple),
	option(strategy(Strategy), Options),
	option(prov(ProvGraph), Options, debugprov),
	option(all_mappings(All), Options, []),
	option(type(Type), Options, amalgame:'Mapping'),

	void_graph(Strategy, Void),
	(   rdf_statistics(triples_by_graph(Id, NrOfTriples)) -> true; NrOfTriples=0),
	assert_metadata(Id, Strategy, Void),
	rdf_assert(All, void:subset,      Id,  Void),
	rdf_assert(Id, void:vocabulary,   amalgame:'', Void),
	rdf_assert(Id, void:vocabulary,   skos:'', Void),
	rdf_assert(Id, void:vocabulary,   align:'', Void),
	(   Format \= edoal
	->  rdf_assert(Id, void:vocabulary,   void:'', Void),
	    rdf_assert(Id, void:vocabulary,   prov:'', Void),
	    rdf_assert(Id, void:vocabulary,   dcterms:'', Void)
	;   true
	),
	rdf_assert(Id, rdf:type,          Type,  Void),
	rdf_assert(Id, void:triples,      NrOfTriples, Void),

	rdf_assert(Id, amalgame:hasPlan,  Strategy, Void),
	rdf_assert(Id, amalgame:prov,	  ProvGraph, Void).

default_mapping_relation(Id, Default, Options) :-
	(   rdf(Id, amalgame:default_relation, Default)
	->  true
	;   option(default_relation(Default), Options, skos:closeMatch)
	).

%%	prepare_mapping(Id, Strategy, Options)
%
%	materialize mapping Id if not materialized.

prepare_mapping(Id, Strategy, Options) :-
	(   \+ rdf_graph(Id)
	->  expand_node(Strategy, Id, Mapping),
	    (	Mapping = [_|_]
	    ->	default_mapping_relation(Id, Default, Options),
		augment_relations(Strategy, Mapping, Augmented, [default_relation(Default)]),
		materialize_mapping_graph(Augmented, [graph(Id) | Options])
	    ;	true % empty mapping, do nothing
	    )
	;   true % already materialized in a previous run, do nothing
	).

prepare_scheme(Id, Strategy, Options) :-
	(   \+ rdf_graph(Id)
	->  expand_node(Strategy, Id, Scheme),
	    (	empty_assoc(Scheme)
	    ->	true % empty scheme, do nothing
	    ;	materialize_scheme_graph(Scheme, [graph(Id) | Options])
	    ;	true % empty mapping, do nothing
	    )
	;   true % already materialized in a previous run, do nothing
	).

save_mapping(Id, Options) :-
	option(strategy(Strategy), Options),
	prepare_mapping(Id, Strategy, Options),
	assert_void(Id, Options),

	(   rdf(_, amalgame:evidenceGraph, _, Id)
	->  Ext = trig,
	    findall(G, rdf(_, amalgame:evidenceGraph, G, Id), EvidenceGraphs)
	;   Ext = ttl
	),
	file_base_name(Id, Base),
	option(dir(Dir), Options, tmpdir),
	atomic_concat(edoal_, Base, EdoalBase),
	absolute_file_name(Base,       Filename, [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(EdoalBase, EdoalName, [relative_to(Dir), extensions([Ext])]),
	option(format(Format), Options, simple),
	(   (Format == edoal ; Format == both)
	->  (   Ext = ttl
	    ->  rdf_save_canonical_turtle(EdoalName, [graph(Id)|Options])
	    ;   rdf_save_trig(EdoalName, [graphs([Id|EvidenceGraphs])|Options])
	    )
	;   true
	),
	(   (Format == simple ; Format == both)
	->  save_flat_triples(Filename, Id, Options)
	;   true
	).

save_scheme(Id, Options) :-
	option(strategy(Strategy), Options),
	prepare_scheme(Id, Strategy, Options),
	assert_void(Id, Options),

	file_base_name(Id, Base),
	option(dir(Dir), Options, tmpdir),
	absolute_file_name(Base,  Filename, [relative_to(Dir), extensions([ttl])]),
	rdf_save_canonical_turtle(Filename, [graph(Id)|Options]).

save_flat_triples(Filename, Id, Options) :-
	option(strategy(Strategy), Options),
	atomic_concat(Id, '_flat_triples', SimpleGraph),
	rdf_unload_graph(SimpleGraph),
	assert_metadata(Id, Strategy, SimpleGraph),
	edoal_to_triples(Id, SimpleGraph, Options),
	rdf_save_canonical_turtle(Filename, [graph(SimpleGraph)|Options]),
	rdf_unload_graph(SimpleGraph).

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
	rdf_has(S,prov:wasGeneratedBy, Process, RP),
	rdf(S,RP,Process,Graph),
	rdf(Process, prov:wasAssociatedWith, O),
	rdf_equal(dcterms:creator, P).
is_metadata_triple(S,P,N^^T, _Graph) :-
	rdf_has(S, amalgame:mappedSourceConcepts, N^^T),
	rdf_equal(P, void:distinctSubjects).
is_metadata_triple(S,P,N^^T, _Graph) :-
	rdf_has(S, amalgame:mappedTargetConcepts, N^^T),
	rdf_equal(P, void:distinctObjects).
is_metadata_triple(S,P,O,Graph) :-
	rdf_has(S, rdfs:label, O, RP),
	rdf(S,RP,O,Graph),
	rdf_equal(dcterms:title, P).

select_mappings_to_be_saved(Strategy, Mappings, Options) :-
	option(status(Status), Options, all),
	(   Status == all
	->  findall(Mapping,
		    (	rdfs_individual_of(Mapping, amalgame:'Mapping'),
			rdf(Mapping, rdf:type, _, Strategy),
			node_stats(Strategy, Mapping, Stats, [compute(true)]),
			\+ option(totalCount(0), Stats)
		    ),
		    Mappings)
	;   findall(Mapping,
		    (	rdfs_individual_of(Mapping, amalgame:'Mapping'),
			rdf(Mapping, rdf:type, _, Strategy),
			rdf(Mapping, amalgame:status, Status),
			node_stats(Strategy, Mapping, Stats, [compute(true)]),
			\+ option(totalCount(0), Stats)
		    ), Mappings)
	).


select_schemes_to_be_saved(Strategy, Schemes, Options) :-
	option(status(Status), Options, all),
	(   Status == all
	->  findall(Scheme,
		    (	amalgame_alignable_scheme(Scheme),
			rdf(Scheme, rdf:type, _, Strategy),
			node_stats(Strategy, Scheme, Stats, [compute(true)]),
			\+ option(totalCount(0), Stats)
		    ),
		    Schemes)
	;   findall(Scheme,
		    (	amalgame_alignable_scheme(Scheme),
			rdf(Scheme, rdf:type, _, Strategy),
			rdf(Scheme, amalgame:status, Status),
			node_stats(Strategy, Scheme, Stats, [compute(true)]),
			\+ option(totalCount(0), Stats)
		    ), Schemes)
	).
