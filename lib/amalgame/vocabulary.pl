:- module(vocab,
	  [ vocab_member/2,
	    all_vocab_members/2,
	    amalgame_alignable_schemes/1,
	    amalgame_alignable_scheme/1,
	    amalgame_vocabulary_languages/1,
	    materialize_scheme_graph/2
	  ]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(skos/util)).
:- use_module(ag_stats).
:- use_module(ag_strategy).
:- use_module(expand_graph). % for virtual vocab schemes
:- use_module(rdf_util).


:- setting(amalgame:consider_all_labeled_resources, boolean, false,
	   'Enable if you want to be able to align arbitrary rdfs:labeled concepts in arbitrary named graphs').

%%	vocab_member(?C, +VocabDef)
%
%	True if C is part of Vocabulary.
%
%	@param VocabDef is a URI of a skos:ConceptScheme or a definitionamlgame_alignable_schemeamlgame_alignable_schemeamlgame_alignable_scheme
%	of a subset thereof.

vocab_member(C, vocspec(Spec)) :-
	!,
	vocab_member(C, Spec).
vocab_member(C, not(Def)) :-
	!,
	ground(C),
	ground(Def),
	\+ vocab_member(C, Def).

vocab_member(E, and(G1,G2)) :-
	!,
	vocab_member(E,G1),
	vocab_member(E,G2).

vocab_member(E, alignable(Alignable)) :-
	!,
	rdf(Alignable, amalgame:class, Class),
	rdf(Alignable, amalgame:graph, Graph),
	!,
	vocab_member(E, graph(Graph)),
	vocab_member(E, type(Class)).

vocab_member(E, scheme(Scheme)) :-
	!,
	node_stats(_Strategy, Scheme, Stats, []),
	(   option(virtual(true), Stats)
	->  vocab_member(E, vscheme(Scheme))
	;   vocab_member(E, rscheme(Scheme))
	).

vocab_member(E, vscheme(Scheme)) :-
	!,
	expand_vocab(Scheme, VocSpec),
	!,
	(   var(E)
	->  gen_assoc(E, VocSpec, _)
	;   get_assoc(E, VocSpec, _)
	).

vocab_member(E, rscheme(Scheme)) :-
	!,
	skos_in_scheme(Scheme, E).

vocab_member(E, type(Class)) :-
	!,
	rdfs_individual_of(E, Class).
vocab_member(E, graph(G)) :-
	!,
	rdf(E, rdf:type, _, G).

vocab_member(E, propvalue(any, Value)) :- !,
	rdf(E, _Property, Value).
vocab_member(E, propvalue(Property, any)) :- !,
	rdf(E, Property, _Value).
vocab_member(E, propvalue(Property, Value)) :- !,
	rdf(E, Property, Value).
vocab_member(E, subtree(Root)) :-
	!,
	skos_descendant_of(Root, E).
vocab_member(E, is_mapped(Options)) :-
	!,
	ground(E),
	option(snd_input(Mappings), Options),
	option(type(Type), Options),
	option(strategy(Strategy), Options),
	all_mapped(Strategy, Type, Mappings, Concepts, _L),
	!,
	get_assoc(E, Concepts, _).

vocab_member(F, 'http://sws.geonames.org/') :-
	!,
	rdfs_individual_of(F, 'http://www.geonames.org/ontology#Feature').
vocab_member(E, Scheme) :-
	atom(Scheme),
	skos_is_vocabulary(Scheme),
	!,
	vocab_member(E, scheme(Scheme)).

vocab_member(E, Assoc) :-
	Assoc = t(_,_,_,_,_),
	(   ground(E)
	->  get_assoc(E, Assoc, _)
	;   gen_assoc(E, Assoc, _)
	).
vocab_member(E, Scheme) :-
	atom(Scheme),
	rdfs_individual_of(Scheme, amalgame:'Alignable'),
	!,
	vocab_member(E, alignable(Scheme)).

/*
vocab_member(I, EDMGraph) :-
	atom(EDMGraph),
	rdf(_, 'http://www.europeana.eu/schemas/edm/country',_, EDMGraph),
	!,
	rdf(I, 'http://www.europeana.eu/schemas/edm/country', _, EDMGraph).
*/
vocab_member(E, Class) :-
	atom(Class),
	rdfs_individual_of(Class, rdfs:'Class'),
	!,
	rdfs_individual_of(E, Class).


expand_vocab(Scheme, VocSpec) :-
	rdf_has(Scheme, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Scheme, OutputType, Process, Strategy),
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy'),
	expand_node(Strategy, Scheme, VocSpec).

get_amb_concepts(Property, Lang, Stats, Concepts) :-
	get_dict('@properties', Stats, PropsStats),
	get_dict(Property, PropsStats, PropStats),
	get_dict(Lang, PropStats, LocalStats),
	get_dict(ambiguousConcepts, LocalStats, Concepts).

all_vocab_members(label(Type, Scheme, Property, Lang, Stats), Concepts) :-
	!,
	get_amb_concepts(Property, Lang, Stats, Ambiguous),
	(   Type == ambig
	->  Concepts = Ambiguous
	;   Type == uniq
	->  all_vocab_members(Scheme, G1s),
	    ord_subtract(G1s, Ambiguous, Concepts)
	;   Type = none
	->  all_vocab_members(Scheme, G1s),
	    findall(C, (member(C, G1s), \+ rdf_has(C, Property, _)), Concepts)
	).

all_vocab_members(and(G1,not(G2)), Concepts) :-
	!,
	all_vocab_members(G1, G1s),
	all_vocab_members(G2, G2s),
	sort(G1s, G1Sorted),
	sort(G2s, G2Sorted),
	ord_subtract(G1Sorted, G2Sorted, Concepts),
	!.
all_vocab_members(and(G1,G2), Concepts) :-
	!,
	all_vocab_members(G1, G1s),
	all_vocab_members(G2, G2s),
	sort(G1s, G1Sorted),
	sort(G2s, G2Sorted),
	ord_intersection(G1Sorted, G2Sorted, Concepts),
	!.
all_vocab_members(is_mapped(Options), Concepts) :-
	option(snd_input(Mappings), Options),
	option(type(Type), Options),
	option(strategy(Strategy), Options),
	maplist(all_mapped(Strategy, Type), Mappings, _Assocs, Mapped),
	append(Mapped, Concepts0),
	sort(Concepts0, Concepts).

all_vocab_members(vscheme(Scheme), Concepts) :-
	!,
	expand_vocab(Scheme, VocSpec),
	!,
	all_vocab_members(VocSpec, Concepts0),
	sort(Concepts0, Concepts).

all_vocab_members(VocSpec, Concepts) :-
	findall(C, vocab_member(C, VocSpec), Concepts0),
	sort(Concepts0, Concepts).


materialize_scheme_graph(Assoc, Options) :-
	option(graph(Graph), Options, test),
        (   rdf_graph(Graph)
        ->  rdf_unload_graph(Graph)
        ;   true
        ),
	assoc_to_keys(Assoc, Concepts),
	forall(member(C, Concepts),
	       materialize_concept(C, Graph)
	      ).

materialize_concept(Concept, Graph) :-
	rdf_assert(Concept, skos:inScheme, Graph, Graph).

%!	amalgame_alignable_scheme(Scheme) is nondet.
%
%	Scheme is unified with a derived (implicitly) defined
%	skos:ConceptScheme or an explicitly defined, non-empty
%	skos:ConceptScheme.

amalgame_alignable_scheme(S) :-
	explicit_scheme(S).

amalgame_alignable_scheme(S) :-
	derived_scheme(S).

amalgame_non_empty_scheme(S) :-
	amalgame_alignable_scheme(S),
	skos_in_scheme_chk(S,_).

%%	amalgame_alignable_schemes(-Schemes) is det.
%
%	Schemes is unified with a sorted list of urls of
%       alignable objects.
%
%	Sorting is based on case insensitive scheme labels.

amalgame_alignable_schemes(Schemes) :-
	findall(S, amalgame_non_empty_scheme(S), All0),
	sort(All0, All),
	maplist(scheme_label, All, Labeled),
	keysort(Labeled, Sorted),
	pairs_values(Sorted, Schemes).

derived_scheme(Scheme) :-
	skos_in_scheme(Scheme, _Concept),
	\+ skos_is_vocabulary(Scheme).

explicit_scheme(S) :-
        skos_is_vocabulary(S).

skos_in_scheme_chk(Scheme, Concept) :-
	skos_in_scheme(Scheme, Concept), !.
scheme_label(URI, Key-URI) :-
	rdf_graph_label(URI, CasedKey),
	downcase_atom(CasedKey, Key).



amalgame_vocabulary_languages(Languages) :-
	findall(Strategy-Schemes,
		strategy_vocabularies(Strategy, Schemes),
		Pairs),
	maplist(lang_used, Pairs, Langs),
	append(Langs, Languages0),
	sort(Languages0, Languages).

lang_used(Strategy-Vocs, Languages) :-
	maplist(lang_used(Strategy), Vocs, Langs),
	append(Langs, Languages0),
	sort(Languages0, Languages).

lang_used(Strategy, Voc, Languages) :-
	debug(ag_stats, 'Computing languages used in ~p for ~p', [Voc, Strategy]),
	node_stats(Strategy, Voc, Stats, [compute(labelprop)]),
	option(languages(Languages), Stats, []).

skos_util:skos_is_vocabulary(Graph) :-
	setting(amalgame:consider_all_labeled_resources, true),
	rdf_graph(Graph),
	once(rdf_has(Concept, rdfs:label, _, _, Graph)),
	\+ skos_in_scheme(_ConceptScheme, Concept, Graph).
skos_util:skos_in_scheme(Graph, Concept) :-
	setting(amalgame:consider_all_labeled_resources, true),
	rdf_graph(Graph),
	rdf_has(Concept, rdfs:label, _, _, Graph),
	\+ skos_in_scheme(_ConceptScheme, Concept, Graph).

