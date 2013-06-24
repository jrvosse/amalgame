:- module(am_vocstats,
          [
	   is_vocabulary/2,
	   voc_property/2,
	   voc_clear_stats/1
          ]).

:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/vocabulary)).

/** <module> Compute and cache vocabulary-oriented properties and statistics.

Currently supported statistical properties include:
* version(Literal)
* format(oneof([skos,skosxl,null]))
* numberOfConcepts(xsd:int)
* numberOfPrefLabels(xsd:int)
* numberOfAltLabels(xsd:int)
* numberOfMappedConcepts(xsd:int)
* languages(list)
* languages(label_property, list)

@author Jacco van Ossenbruggen
*/


:- dynamic
	voc_stats_cache/2.

:- rdf_meta
	voc_property(r, -),
	voc_languages(r,-),
	voc_languages(r,r,-),
	voc_languages_used(r,r,-).

voc_property(Voc, P) :-
	rdf_global_term(P, PG),
	(   voc_stats_cache(Voc, PG)
	->  true
	;   voc_ensure_stats(Voc, PG)
	).

assert_voc_prop(Voc, M) :-
	assert(voc_stats_cache(Voc, M)).


voc_clear_stats(all) :-
	retractall(voc_stats_cache(_,_)),
	print_message(informational, map(cleared, 'vocabulary statistics', all_vocs, all)).

voc_clear_stats(Voc) :-
	retractall(voc_stats_cache(Voc, _)),
	print_message(informational, map(cleared, 'vocabulary statistics', Voc, all)).


is_vocabulary(Voc, Format):-
	ground(Voc),!,
	(   voc_stats_cache(Voc, format(Format))
	->  true
	;   voc_ensure_stats(Voc, format(Format))
	).

is_vocabulary(Voc, Format):-
	var(Voc),
	findall(V-F, voc_ensure_stats(V,format(F)), _),
	!,
	voc_stats_cache(Voc, format(Format)).

voc_ensure_stats(Voc, format(Format)) :-
	rdfs_individual_of(Voc, skos:'ConceptScheme'),
	\+ rdf(Voc, amalgame:wasGeneratedBy, _),
	(   voc_stats_cache(Voc, format(Format))
	->  true
	;   voc_find_format(Voc, Format),
	    assert(voc_stats_cache(Voc, format(Format)))
	).

voc_ensure_stats(Voc, version(Version)) :-
	(   rdf_has(Voc, owl:versionInfo, literal(Version))
	->  true
	;   rdf(Voc, amalgame:wasGeneratedBy, _)
	->  Version = amalgame_generated
	;   assert_voc_version(Voc, Version)
	->  true
	;   debug(info, 'Failed to ensure version stats for ~', [Voc])
	),!.


voc_ensure_stats(Voc, numberOfConcepts(Count)) :-
	(   count_concepts(Voc, Count) -> true ; Count = 0),
	assert_voc_prop(Voc, numberOfConcepts(Count)).

voc_ensure_stats(Voc, numberOfPrefLabels(Count)) :-
	(   count_prefLabels(Voc, Count) -> true ; Count = 0),
	assert_voc_prop(Voc, numberOfPrefLabels(Count)).

voc_ensure_stats(Voc, numberOfAltLabels(Count)) :-
	(   count_altLabels(Voc, Count) -> true ; Count = 0),
	assert_voc_prop(Voc,numberOfAltLabels(Count)).

voc_ensure_stats(Voc, numberOfMappedConcepts(Count)) :-
	(   count_mapped_concepts(Voc, Count) -> true ; Count = 0),
	assert_voc_prop(Voc, numberOfMappedConcepts(Count)).
voc_ensure_stats(Voc, languages(L)) :-
	(   voc_languages_used(Voc, L) -> true ; L = []),
	assert(voc_stats_cache(Voc, languages(L))).
voc_ensure_stats(Voc, languages(P,L)) :-
	(   voc_languages_used(Voc, P, L) -> true ; L = []),
	assert(voc_stats_cache(Voc, languages(P,L))).

%%	assert_voc_version(+Voc, +TargetGraph) is det.
%
%	Assert version of Voc using RDF triples in named graph TargetGraph.

assert_voc_version(Voc, Version) :-
	(   rdf(Voc, amalgame:subSchemeOf, SuperVoc)
	->  assert_subvoc_version(Voc, SuperVoc, Version)
	;   assert_supervoc_version(Voc, Version)
	).

assert_subvoc_version(Voc, SuperVoc, Version) :-
	rdf_has(SuperVoc, owl:versionInfo, Version),
	assert(voc_stats_cache(Voc, version(Version))).

assert_supervoc_version(Voc, Version) :-
	rdf(_, skos:inScheme, Voc, SourceGraph:_),!,
	prov_get_entity_version(Voc, SourceGraph, Version),
	assert(voc_stats_cache(Voc, version(Version))).


count_concepts(Voc, Count) :-
	findall(Concept,
		vocab_member(Concept, Voc),
		Concepts),
	length(Concepts, Count),
	print_message(informational, map(found, 'Concepts', Voc, Count)).

count_prefLabels(Voc, Count) :-
	findall(Label,
		(   vocab_member(Concept, Voc),
		    (	rdf_has(Concept, skos:prefLabel, literal(Label))
		    ;	rdf_has(Concept, skosxl:prefLabel, Label)
		    )
		),
		Labels),
	length(Labels, Count),
	print_message(informational, map(found, 'SKOS preferred labels', Voc, Count)).

count_altLabels(Voc, Count) :-
	findall(Label,
		(   vocab_member(Concept, Voc),
		    (	rdf_has(Concept, skos:altLabel, literal(Label))
		    ;	rdf_has(Concept, skosxl:altLabel, Label)
		    )
		),
		Labels),
	length(Labels, Count),
	print_message(informational, map(found, 'SKOS alternative labels', Voc, Count)).

count_mapped_concepts(Voc, Count) :-
	findall(C,
		(   vocab_member(C, Voc),
		    (	has_correspondence_chk(align(C, _, _), _)
		    ;	has_correspondence_chk(align(_, C, _), _)
		    )
                ),
		Concepts),
	sort(Concepts, Sorted),
	length(Sorted, Count),
	print_message(informational, map(found, 'SKOS mapped concepts', Voc, Count)).

voc_languages_used(all, Langs) :-
	findall(L,
		(   rdfs_individual_of(Voc, skos:'ConceptScheme'),
		    voc_languages_used(Voc, L)
		),
		Ls),
	flatten(Ls, Flat),
	sort(Flat, Langs).

voc_languages_used(Voc, Langs) :-
	(   setof(Lang, language_used(Voc, Lang), Langs)
	->  true
	;   Langs = []
	).

voc_languages_used(Voc, Prop, Langs) :-
	(   setof(Lang, language_used(Voc, Prop, Lang), Langs)
	->  true
	;   Langs = []
	).

language_used(Voc, Lang) :-
	rdf_has(Concept, skos:inScheme, Voc),
	rdf(Concept, _, literal(lang(Lang, _))),
	ground(Lang).

language_used(Voc, Prop, Lang) :-
	rdf_has(Concept, skos:inScheme, Voc),
	rdf_has(Concept, Prop, literal(lang(Lang, _))),
	ground(Lang).

voc_find_format(Voc, Format) :-
	ground(Voc),
	(   rdf_has(Concept, skos:inScheme, Voc)
	->  (   rdf_has(Concept, skosxl:prefLabel, _)
	    ->  Format = skosxl
	    ;   rdf_has(Concept, skos:prefLabel, _)
	    ->  Format = skos
	    ;   rdf_has(Concept, skos:altLabel, _)
	    ->  Format = skos
	    ;   Format = null           % no concepts with known labels
	    )
	;   Format = null		% no concepts in the scheme
	).

