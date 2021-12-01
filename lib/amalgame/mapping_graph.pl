:- module(ag_mapping_graph,
	  [has_correspondence/2,    % align/3, MappingGraph URI
	   has_correspondence_chk/2,
	   remove_correspondence/2, % align/3, MappingGraph URI

	   map_nickname/3,             % +Strategy, +MappingGraph, ?Nickname
	   map_localname/3,             % +Strategy, +MappingGraph, ?Localname
	   nickname_clear_cache/0,
	   mapping_vocab_sources/4,    % +MappingURI, +Strategy, -Source, -Target
	   augment_relations/4,

	   mapping_relation/2,
	   materialize_mapping_graph/3, % Strategy, +List, +Optios
	   supported_map_relations/1, % ?URIList
	   status_option/1
	  ]
	 ).

/** <module> Amalgame mapping_graph module

This module contains predicates to deal with correspondences stored in
an rdf amed graph.

@author Jacco van Ossenbruggen
@license LGPL
*/

:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/rdf_util)).
:- use_module(library(amalgame/ag_reference)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/vocabulary)).

:- dynamic
	nickname_cache/3.

:- rdf_meta
	status_option(r).

%%	status_option(-Status)
%
%	List of status types.

status_option(amalgame:final).
status_option(amalgame:intermediate).
status_option(amalgame:discarded).
status_option(amalgame:reference).


%%	has_correspondence(?C, +G) is nondet.
%
%	Is true if C unifies with a correspondence C in named graph G.

has_correspondence(align(E1, E2, P), Graph) :-
	has_map([E1, E2], _, Properties, Graph),
	append(Properties, Pflat),
	(   memberchk(evidenceGraph(_), Pflat)
	->  inline_evidence_graphs(Properties, Properties1)
	;   Properties1 = Properties
	),
	(   memberchk(method(_), Pflat)
	->  P = Properties1
	;   Properties1 = [P1|Rest],
	    append([method("preloaded"), graph(Graph)], P1, P2),
	    P = [P2|Rest]
	).

has_correspondence_chk(align(E1,E2,_P), Graph):-
	has_map([E1,E2],_,Graph),!.

%%	remove_correspondence(+C, +G) is semidet.
%
%	removes the first correspondence C from named graph G.
%	G is assumed to be a RDF graph with EDOAL cells.

remove_correspondence(align(E1, E2, Prov), Graph) :-
	ground(Graph),
	ground(E1),
	ground(E2),
	has_edoal_map_([E1, E2], Cell, Graph),
	has_correspondence(align(E1, E2, Prov), Graph),
	!,
	rdf_remove_resource(Cell, Graph).


:- rdf_meta
	mapping_relation(+, r).

%%	mapping_relation(+Id, +URI)
%
%	Available mapping relations.

mapping_relation(exact,     skos:exactMatch).
mapping_relation(close,     skos:closeMatch).
mapping_relation(narrower,  skos:narrowMatch).
mapping_relation(broader,   skos:broadMatch).
mapping_relation(related,   skos:relatedMatch).
mapping_relation(replaces,  dcterms:replaces).
mapping_relation(isreplby,  dcterms:isReplacedBy).
mapping_relation(unrelated, evaluator:unrelated).
mapping_relation(unsure,    evaluator:unsure).

% mapping_relation(skos, skos:mappingRelation).
% mapping_relation(owl_sameAs,  owl:sameAs).


supported_map_relations(List) :-
	findall(Relation,
		(   mapping_relation(_, Super),
		    rdfs_subproperty_of(Relation, Super),
		    \+ rdf_equal(skos:mappingRelation, Relation)
		),
		List).


%%	has_map(+Map, ?Format, ?Properties, -Graph) is nondet.
%%%	has_map(+Map, ?Format, -Graph) is nondet.
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

has_map([E1, E2], edoal, Properties, Graph) :-
	has_edoal_map_([E1, E2], Cell, Graph),
	findall(Bnode, rdf(Cell, amalgame:evidence, Bnode, Graph), Bnodes),
	findall(Prov,
		(   member(Bnode, [Cell|Bnodes]),
		    findall(Term,
			    (	rdf(Bnode, Prop, Value, Graph),
				\+ rdf_equal(align:entity1, Prop),
				\+ rdf_equal(align:entity2, Prop),
				\+ rdf_equal(rdf:type, Prop),
				prop_to_term(Prop, Value, Term)
			    ),
			    Prov)
		),
		Properties).

has_map([E1, E2], Format, [[method("preloaded"),
			    relation(RealProp),
			    graph(Graph)]], Graph) :-
	mapping_relation(Format,MappingProp),
	(   ground(E1), ground(E2)
	->  rdf_has(E1, MappingProp, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, MappingProp),
	    rdf(E1, RealProp, E2, Graph)
	).

has_map(Map, edoal, Graph) :-
	has_edoal_map_(Map, _Cell, Graph).

has_map([E1, E2], Format, Graph) :-
	Format \== edoal,
	mapping_relation(Format,MappingProp),
	(   (ground(E1); ground(E2))
	->  rdf_has(E1, MappingProp, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, MappingProp),
	    rdf(E1, RealProp, E2, Graph)
	).


has_edoal_map_([E1,E2], Cell, Graph) :-
	(   ground(E1)
	->  rdf(Cell, align:entity1, E1, Graph),
	    (	ground(E2)
	    ->	rdf(Cell, align:entity2, E2, Graph)
	    ;	rdf(Cell, align:entity2, E2, Graph)
	    ->	true
	    ;	true
	    )
	;   ground(E2) % We know that E1 is var.
	->  rdf(Cell, align:entity2, E2, Graph),
	    (	rdf(Cell, align:entity1, E1, Graph)
	    ->	true
	    ;	true
	    )
	).

has_edoal_map_([E1,E2], Cell, Graph) :-
	var(E1), var(E2),
	rdf(Cell, align:entity1, E1, Graph),
	rdf(Cell, align:entity2, E2, Graph).

has_edoal_map_([E1,E2], Cell, Graph) :-
	var(E1), var(E2),
	rdf(Cell, align:entity1, E1, Graph),
	\+ rdf(Cell, align:entity2, _, Graph).


prop_to_term(Prop, Value, Term) :-
	rdf_global_id(NS:Local, Prop),
	(   NS:Local = align:measure
	->  (literal(type(_,LRealValue)) = Value;literal(LRealValue) = Value),
	    term_to_atom(RealValue, LRealValue)
	;   NS:Local = align:relation
	->  atom_to_skos_relation(Value, RealValue)
	;   rdf_is_literal(Value),
	    rdf_lexical_form(Value, RealValue^^_Type)
	->  true
	;   literal(RealValue) = Value
	->  true
	;   RealValue = Value
	),
	Term =.. [Local, RealValue],!.

atom_to_skos_relation(literal('='), R) :- rdf_equal(skos:exactMatch, R),!.
atom_to_skos_relation(literal('<'), R) :- rdf_equal(skos:broadMatch, R),!.
atom_to_skos_relation(literal('>'), R) :- rdf_equal(skos:narrowMatch, R),!.
atom_to_skos_relation(literal(_), R) :- rdf_equal(skos:relatedMatch, R),!.
atom_to_skos_relation(URL, URL) :- !.

%%	prolog:message(+Term)// is det.
%
%	Used to print various progress messages.
%	Term = map(found, What, From, Number)).

prolog:message(map(found, What, From, Number)) -->
        [ 'Found ', Number, ' ', What, ' (', From, ') to process.' ].



%%      materialize_alignment_graph(+Mappings, +Options)
%
%       Assert Mappings as (EDOAL) triples in the store.

materialize_mapping_graph(Strategy, Input, Options) :-
        option(graph(Graph), Options, test),
	mapping_vocab_sources(Graph, Strategy, SourceVocab, TargetVocab),

        (   rdf_graph(Graph)
        ->  rdf_unload_graph(Graph)
        ;   true
        ),

	assert_alignment(Graph, [method(Strategy), ontology1(SourceVocab), ontology2(TargetVocab)|Options]),
        (   memberchk(align(_,_,_), Input)
        ->  mat_alignment_graph(Input, Options)
        ;   true
        ).

mat_alignment_graph([], _).
mat_alignment_graph([align(S,T,P)|As], Options) :-
        (   flatten(P, Pflat), member(relation(R), Pflat)
	->  Relation = relation(R), NewProv=[]
	;   option(default_relation(R), Options)
	->  Relation = relation(R), NewProv=[method(default_relation), Relation]
	;   Relation = foo(bar), NewProv=[]
	),
        assert_cell(S, T, [prov([NewProv|P]), Relation |Options]),
        mat_alignment_graph(As, Options).


%%	map_nickname(+Strategy, +Graph, ?Nickname) is det.
%
%	Unifies Nickname with the nickname of Graph in Strategy.
%	Creates Nickname if Graph does not have one yet.

map_nickname(Strategy, Graph, Nick) :-
	rdf(Graph,  amalgame:nickname, Nick^^xsd:string, Strategy),!.
map_nickname(Strategy, Graph, Nick) :-
	nickname_cache(Strategy, Graph, Nick), !.
map_nickname(Strategy, Graph, Nick) :-
	create_nickname(Strategy, Graph, '', Nick).

map_localname(Strategy, Node, Local) :-
	rdf(Strategy, amalgame:publish_ns, NS, Strategy),
	sub_atom(Node, 0, L, _, NS),
	sub_atom(Node, L, _, 0, Local).

create_nickname(Strategy, Graph, Postfix, Nickname) :-
	char_type(Nick, alpha),
	char_type(Nick, ascii),
	format(atom(Nickname), '~w~w', [Nick, Postfix]),
	\+ rdf(_,  amalgame:nickname, Nickname^^xsd:string, Strategy),
	\+ nickname_cache(Strategy, _, Nickname),
	!,
	assert(nickname_cache(Strategy, Graph, Nickname)),
	rdf_assert(Graph, amalgame:nickname, Nickname^^xsd:string, Strategy).

create_nickname(Strategy, Graph, _Postfix, Nickname) :-
	% all nicknames with or without Postfix are taken, generate new prefix:
	gensym('',Postfix),
	create_nickname(Strategy, Graph, Postfix, Nickname).


nickname_clear_cache :-
	retractall(nickname_cache(_,_,_)).

augment_relations(Strategy, Mapping, Augmented, Options) :-
	reference_mappings(Strategy, References),
	augment_relation(Mapping, References, Augmented, Options).


augment_relation([], _, [], _) :- !.
augment_relation(M, [], M, []):- !.
augment_relation([align(S,T,Prov)|Tail], [], NewResults, Options) :-
	!,
	(   option(default_relation(DefaultRelation), Options)
	->  Default = [method(default_relation), relation(DefaultRelation)],
	    NProv = [Default|Prov]
	;   NProv = Prov
	),
	NewResults = [align(S,T,NProv)|Results],
	augment_relation(Tail , [], Results, Options).

augment_relation(Mappings, Reference, NewResults, Options) :-
	Mappings = [Head|Tail],
	Reference = [RHead|RTail],
	Head = align(S, T, Prov),
	RHead = align(SR, TR, RProv),
	compare(Comp, align(S,T), align(SR,TR)),
	(   Comp == =
	->  member(Manual, RProv),
	    member(method(MethodL), Manual),
	    literal_text(MethodL, Method),
	    (	Method == "manual_evaluation"
	    ;	Method == "preloaded"
	    ),
	    option(relation(_Rel), Manual),
	    NProv0 = [Manual|Prov], sort(NProv0, NProv),
	    NewResults =  [align(S,T,NProv)|Results],
	    NewMappings = Tail,
	    NewRef= RTail
	;   Comp == <
	->  (	option(default_relation(DefaultRelation), Options)
	    ->	Default = [method(default_relation), relation(DefaultRelation)],
		NProv = [Default|Prov]
	    ;	NProv = Prov
	    ),
	    NewResults = [align(S,T,NProv)|Results],
	    NewMappings = Tail,
	    NewRef = Reference
	;   NewResults = Results,
	    NewMappings = Mappings,
	    NewRef = RTail
	),
	augment_relation(NewMappings ,NewRef, Results, Options), !.


augment_relation(NewMappings ,NewRef, Results, Options) :-
	gtrace,
	NewRef = NewMappings,
	Results = Options.

%%	mapping_vocab_sources(+MappingURI, +Strategy, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_vocab_sources(Manual, Strategy, SV, TV) :-
	(   rdf_has(Manual, amalgame:evaluationOf, Strategy)
	;   rdfs_individual_of(Manual, amalgame:'LoadedMapping')
	),
	!,
	has_correspondence_chk(align(SC,TC,_), Manual),
        have_vocab_sources(SC, TC, SV, TV).

mapping_vocab_sources(URL, Strategy, S, T) :-
	rdf_has(URL, amalgame:wasGeneratedBy, Process, RealProp),
	rdf(URL, RealProp, Process, Strategy),
	!,
	(   rdf(Process, amalgame:source, S0, Strategy),
	    rdf(Process, amalgame:target, T0, Strategy)
	->  vocab_source(S0, Strategy, S),
	    vocab_source(T0, Strategy, T)
	;   rdf(Process, amalgame:input, Input, Strategy)
	->  mapping_vocab_sources(Input, Strategy, S, T)
	).

have_vocab_sources(SourceConcept, TargetConcept, SVoc, TVoc) :-
	strategy_vocabulary(Strategy, SVoc),
	vocab_member(SourceConcept, scheme(SVoc)),
	strategy_vocabulary(Strategy, TVoc),
	vocab_member(TargetConcept, scheme(TVoc)),
	!.

have_vocab_sources(_S,_T, undef, undef) :-
	!.


vocab_source(V, Strategy, S) :-
	rdf_has(V, amalgame:wasGeneratedBy, Process, RealProp1),
	rdf(V, RealProp1, Process, Strategy),
	rdf_has(Process, amalgame:input, Input, RealProp2),
	rdf(Process, RealProp2, Input, Strategy),
	!,
	vocab_source(Input, Strategy, S).
vocab_source(V, _S, V).
