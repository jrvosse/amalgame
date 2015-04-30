:- module(uniq_label_voc_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public specifier/5.
:- public selecter/5.

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/ag_stats)).

amalgame_module(amalgame:'UniqLabelVocSelect').

parameter(property, uri, Default,
	  'RDF property by which to select the concepts') :-
	rdf_equal(skos:prefLabel, Default).
parameter(language, oneof(L), Default,
	  'Language of source label') :-
	amalgame_vocabulary_languages(L),
	(   member(en, L)
	->  Default = en
	;   L = [Default|_]
	).

specifier(VocSpec, Sel, Dis, Und, Options) :-
	option(property(Property), Options),
	option(language(Language), Options),
	VocSpec = rscheme(Scheme),
	node_stats(_Strategy, Scheme, Stats, [compute(deep)]),
	Sel =  label(uniq,  Scheme, Property, Language, Stats),
	Dis =  label(ambig, Scheme, Property, Language, Stats),
	Und =  label(none,  Scheme, Property, Language, Stats).

selecter(VocSpec, SelConcepts, DisConcepts, UndConcepts, Options) :-
	specifier(VocSpec, SelSpec, DisSpec, UndSpec, Options),
	all_vocab_members(SelSpec, SelConcepts),
	all_vocab_members(DisSpec, DisConcepts),
	all_vocab_members(UndSpec, UndConcepts).
