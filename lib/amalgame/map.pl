:- module(ag_map,
	  [
	   has_correspondence/2,    % align/3, MappingGraph URI
	   has_correspondence_chk/2,
	   remove_correspondence/2, % align/3, MappingGraph URI
	   correspondence_source/2,
	   correspondence_target/2,
	   correspondence_evidence/2,
	   nickname/3,             % +Strategy, +MappingGraph, ?Nickname
	   nickname_clear_cache/0,

	   augment_relations/5,

	   mapping_relation/2,
	   materialize_mapping_graph/2, % +List, +Options
	   merge_provenance/2,     % +List, -Merged
	   compare_align/4,        % +Type, ?Order, A1, A2
	   same_source/4,          % +List, +Source, -Same, -Rest
	   same_target/4,          % +List, +Target, -Same, -Rest
	   supported_map_relations/1 % ?URIList
	  ]
	 ).

/** <module> Amalgame correspondences (map) module

This module contains predicates to deal with correspondences while
abstracting from the underlying formats. This should converge into a
set of functions around sorted lists with
align(Source,Target,EvidenceList) terms.

@author Jacco van Ossenbruggen
@license LGPL
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/ag_evaluation)).

:- dynamic
	nickname_cache/3.

%%	correspondence_source(?C,?S) is det.
%
%	Unifies S with the source of correspondence C.

correspondence_source(align(S,_,_), S).

%%	correspondence_target(?C,?T) is det.
%
%	Unifies T with the target of correspondence C.
%
correspondence_target(align(_,T,_), T).

%%	correspondence_evidence(?C,?E) is det.
%
%	Unifies E with the evidence list of correspondence C.

correspondence_evidence(align(_,_,E), E).

%%	same_source(+List, +Source, -Same, -Rest) is det.
%
%	Same contains all alignments from List that have Source as a
%	source, Rest contains all alignments with a different source.
%	List, Same and Rest are assumed to be the usual lists of
%	amalgame's align(S,T,P), sorted on S.

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,  same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

%%	same_target(+List, +Target, -Same, -Rest) is det.
%
%	Same contains all alignments from List that have Target as a
%	target, Rest contains all alignments with a different target.
%	List, Same and Rest are assumed to be the usual lists of
%	amalgame's align(S,T,P), sorted on T.

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,  same_target(As, T, Same, Rest).
same_target(As, _S, [], As).

%%	has_correspondence(?C, +G) is nondet.
%
%	Is true if C unifies with a correspondece C in named graph G.

has_correspondence(align(E1, E2, P), Graph) :-
	has_map([E1, E2], _, Properties, Graph),
	append(Properties, Pflat),
	(   memberchk(evidenceGraph(_), Pflat)
	->  inline_evidence_graphs(Properties, Properties1)
	;   Properties1 = Properties
	),
	(   memberchk(method(_), Pflat)
	->  P = Properties1
	;   P = [[method(preloaded), graph(Graph)]|Properties1]
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
	remove_resource(Cell, Graph).


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
mapping_relation(unrelated, 'http://purl.org/vocabularies/amalgame/evaluator#unrelated').
mapping_relation(unsure,    'http://purl.org/vocabularies/amalgame/evaluator#unsure').

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
		(   member(Bnode, Bnodes),
		    findall(Term,
			    (	rdf(Bnode, Prop, Value, Graph),
				prop_to_term(Prop, Value, Term)
			    ),
			    Prov)
		),
		Properties).

has_map([E1, E2], Format, [[method(preloaded),
			    relation(RealProp)]], Graph) :-
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
	;   NS:Local = amalgame:method
	->  literal(Literal) = Value,
	    term_to_atom(RealValue, Literal)
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
prolog:message(map(cleared, What, From, Number)) -->
        [ 'Cleared ', Number, ' ', What, ' (', From, ').' ].
prolog:message(map(created, What, From, _Number)) -->
        [ 'Created ', What, ' (', From, ').' ].
prolog:message(map(occurs_min(Min, MappingList))) -->
	{ length(MappingList,MLL) },
	[ 'Occurs in min ~w mapping graphs: ~w'-[Min, MLL] ].


%%      compare_align(Type, Order, A1, A2) is det.
%
%       compare alignment A1 and A2 on the standard order of the url of
%       their source or target concept, depending on Type.

compare_align(source, Order, align(S1,T1,P1), align(S2,T2,P2)) :-
        (   compare(FirstOrder, S1, S2), FirstOrder \== '='
        ->  FirstOrder = Order
        ;   compare(SecondOrder, T1, T2), SecondOrder \== '='
        ->  SecondOrder = Order
        ;   compare(Order, P1, P2)
        ).
compare_align(target, Order, align(S1,T1,P1), align(S2,T2,P2)) :-
        (   compare(FirstOrder, T1, T2), FirstOrder \== '='
        ->  FirstOrder = Order
        ;   compare(SecondOrder, S1, S2), SecondOrder \== '='
        ->  SecondOrder = Order
        ;   compare(Order, P1, P2)
        ).


%%      merge_provenance(+AlignIn, -AlignOut)
%
%       Collects all provenance for similar source target pairs.
%       AlignIn is a sorted list of align/3 terms.

merge_provenance([], []).
merge_provenance([align(S, T, P)|As], Gs) :-
        group_provenance(As, S, T, P, Gs).

group_provenance([align(S,T,P)|As], S, T, P0, Gs) :-
        !,
        append(P, P0, P1),
        group_provenance(As, S, T, P1, Gs).
group_provenance(As, S, T, P, [align(S, T, Psorted)|Gs]) :-
        sort(P, Psorted),
        merge_provenance(As, Gs).


%%      materialize_alignment_graph(+Alignments, +Options)
%
%       Assert Alignments as triples in the store.

materialize_mapping_graph(Input, Options) :-
        option(graph(Graph), Options, test),

        (   rdf_graph(Graph)
        ->  rdf_unload_graph(Graph)
        ;   true
        ),
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


%%	nickname(+Strategy, +Graph, ?Nickname) is det.
%
%	Unifies Nickname with the nickname of Graph in Strategy.
%	Creates Nickname if Graph does not have one yet.

nickname(Strategy, Graph, Nick) :-
	rdf(Graph,  amalgame:nickname, literal(Nick), Strategy),!.
nickname(Strategy, Graph, Nick) :-
	nickname_cache(Strategy, Graph, Nick), !.
nickname(Strategy, Graph, Nick) :-
	create_nickname(Strategy, Graph, '', Nick).

create_nickname(Strategy, Graph, Postfix, Nickname) :-
	char_type(Nick, alpha),
	char_type(Nick, ascii),
	format(atom(Nickname), '~w~w', [Nick, Postfix]),
	\+ rdf(_,  amalgame:nickname, literal(Nickname), Strategy),
	\+ nickname_cache(Strategy, _, Nickname),
	!,
	assert(nickname_cache(Strategy, Graph, Nickname)),
	rdf_assert(Graph, amalgame:nickname, literal(Nickname), Strategy).

create_nickname(Strategy, Graph, _Postfix, Nickname) :-
	% all nicknames with or without Postfix are taken, generate new prefix:
	gensym('',Postfix),
	create_nickname(Strategy, Graph, Postfix, Nickname).


nickname_clear_cache :-
	retractall(nickname_cache(_,_,_)).

augment_relations(Strategy, Id, Mapping, Augmented, Options) :-
	(   evaluation_graph_chk(Strategy, Id, Prev)
	->  expand_node(Strategy, Prev, PreviousEvaluation)
	;   PreviousEvaluation = []
	),
	augment_relation(Mapping, PreviousEvaluation, Augmented, Options).


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
	    (	member(method(manual_evaluation), Manual)
	    ;	member(method(preloaded), Manual)
	    ),
	    option(relation(_Rel), Manual),
	    NProv = [Manual|Prov],
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
	augment_relation(NewMappings ,NewRef, Results, Options).
