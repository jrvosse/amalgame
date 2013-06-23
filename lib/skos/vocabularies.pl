:- module(am_skosvocs,
          [
	   voc_languages/3,
	   voc_languages/2,
	   voc_clear_stats/1,
	   voc_ensure_stats/1
          ]).

:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/ag_provenance)).

/** <module> Compute and store vocabulary-oriented statistics as RDF.

Currently supported statistical properties include:
* numberOfConcepts(xsd:int)
* numberOfPrefLabels(xsd:int)
* numberOfAltLabels(xsd:int)
* numberOfMappedConcepts(xsd:int)

Side effect: These statistics will also be asserted as RDF
triples to the 'amalgame_vocs' named graph, using similarly named
properties with the 'amalgame:' namespace prefix. These asserted
triples will be used in subsequent calls for efficiency reasons.

See also http_clear_cache/1.

@author Jacco van Ossenbruggen
*/


:- dynamic
	voc_stats_cache/2.

:- rdf_meta
	voc_languages(r,-),
	voc_languages(r,r,-),
	voc_languages_used(r,r,-).

is_vocabulary(Voc, Format):-
        ground(Voc),!,
        voc_format(Voc, Format).
is_vocabulary(Voc, Format) :-
	var(Voc),
	rdf(Voc, amalgame:vocformat, literal(Format), amalgame_vocs).

voc_format(Voc, Format) :-
        rdf(Voc, amalgame:vocformat, literal(Format), amalgame_vocs), !.
voc_format(Voc, Format) :-
	rdfs_individual_of(Voc, skos:'ConceptScheme'),
	(   rdf_has(Concept, skos:inScheme, Voc)
	->  (   rdf_has(Concept, skosxl:prefLabel, _)
	    ->  Format = skosxl
	    ;   rdf_has(Concept, skos:prefLabel, _)
	    ->  Format = skos
	    ;	rdf_has(Concept, skos:altLabel, _)
	    ->	Format = skos
	    ;   Format = null           % no concepts with known labels
	    )
	;   Format = null		% no concepts in the scheme
	),
	!.

voc_clear_stats(all) :-
	(   rdf_graph(amalgame_vocs)
	->  rdf_unload_graph(amalgame_vocs),
	    print_message(informational, map(cleared, 'vocabulary statistics', amalgame_vocs, all))
	;   true
	).


voc_clear_stats(Graph) :-
	rdf_retractall(Graph, _, _, amalgame_vocs),
	print_message(informational, map(cleared, 'vocabulary statistics', Graph, all)).


%%	voc_ensure_stats(+Type) is det.
%
%	Ensures that the statistical properties of Type are asserted in
%	the amalgame graph.

voc_ensure_stats(found) :-
        findall(Voc,
		(   rdfs_individual_of(Voc, skos:'ConceptScheme')
		),
		Vocs
	       ),
	forall(member(Voc, Vocs),
	       (
	       voc_format(Voc, Format),
	       rdf_assert(Voc, amalgame:vocformat, literal(Format), amalgame_vocs)
	       )
	      ).

voc_ensure_stats(all) :-
	voc_ensure_stats(found),
	findall(V, is_vocabulary(V, _Format), Vocs),!,
	length(Vocs, N),
	print_message(informational,
		      map(found, 'SKOS vocabularies (ConceptSchemes)', repository, N)),
	forall(member(V, Vocs),voc_ensure_stats(all(V))).

voc_ensure_stats(all(V)) :-
	voc_ensure_stats(version(V)),
	voc_ensure_stats(numberOfConcepts(V)),
	voc_ensure_stats(numberOfPrefLabels(V)),
	voc_ensure_stats(numberOfAltLabels(V)),
	voc_ensure_stats(numberOfMappedConcepts(V)).

voc_ensure_stats(version(Voc)) :-
	(   rdf_has(Voc, owl:versionInfo, literal(_))
	->  true
	;   rdf(Voc, amalgame:wasGeneratedBy, _)
	->  true
	;   assert_voc_version(Voc, amalgame_vocs)
	->  true
	;   debug(info, 'Failed to ensure version stats for ~', [Voc])
	),!.


voc_ensure_stats(numberOfConcepts(Voc)) :-
	(   rdf(Voc,amalgame:numberOfConcepts, literal(type(_, Count)))
	->  true
	;   count_concepts(Voc, Count),
	    assert_voc_props(Voc:[numberOfConcepts(literal(type('http://www.w3.org/2001/XMLSchema#int', Count)))])
	),!.

voc_ensure_stats(numberOfPrefLabels(Voc)) :-
	(   rdf(Voc,amalgame:numberOfPrefLabels, literal(type(_, Count)))
	->  true
	;   count_prefLabels(Voc, Count),
	    assert_voc_props(Voc:[numberOfPrefLabels(literal(type('http://www.w3.org/2001/XMLSchema#int', Count)))])
	),!.

voc_ensure_stats(numberOfAltLabels(Voc)) :-
	(   rdf(Voc,amalgame:numberOfAltLabels, literal(type(_, Count)))
	->  true
	;   count_altLabels(Voc, Count),
	    assert_voc_props(Voc:[numberOfAltLabels(literal(type('http://www.w3.org/2001/XMLSchema#int', Count)))])
	),!.

voc_ensure_stats(numberOfMappedConcepts(Voc)) :-
	(   rdf(Voc,amalgame:numberOfMappedConcepts, literal(type(_, Count)))
	->  true
	;   count_mapped_concepts(Voc, Count),
	    assert_voc_props(Voc:[numberOfMappedConcepts(literal(type('http://www.w3.org/2001/XMLSchema#int', Count)))])
	),!.

voc_languages(Voc, L) :-
	(   voc_stats_cache(Voc, languages(L))
	->  true
	;   voc_languages_used(Voc, L),
	    assert(voc_stats_cache(Voc, languages(L)))
	).

voc_languages(Voc, P, L) :-
	(   voc_stats_cache(Voc, P-languages(L))
	->  true
	;   voc_languages_used(Voc, P, L),
	    assert(voc_stats_cache(Voc, P-languages(L)))
	).
%%	assert_voc_version(+Voc, +TargetGraph) is det.
%
%	Assert version of Voc using RDF triples in named graph TargetGraph.

assert_voc_version(Voc, TargetGraph) :-
	(   rdf(Voc, amalgame:subSchemeOf, SuperVoc)
	->  assert_subvoc_version(Voc, SuperVoc, TargetGraph)
	;   assert_supervoc_version(Voc, TargetGraph)
	).

assert_subvoc_version(Voc, SuperVoc, TargetGraph) :-
	voc_ensure_stats(version(SuperVoc)),
	rdf_has(SuperVoc, owl:versionInfo, Version),
	rdf_assert(Voc,   owl:versionInfo, Version, TargetGraph).

assert_supervoc_version(Voc, TargetGraph) :-
	rdf(_, skos:inScheme, Voc, SourceGraph:_),!,
	prov_assert_entity_version(Voc, SourceGraph, TargetGraph).


assert_voc_props([]).
assert_voc_props([Head|Tail]) :-
	assert_voc_props(Head),
	assert_voc_props(Tail),!.

assert_voc_props(Voc:Props) :-
	rdf_equal(amalgame:'', NS),
	(   rdfs_individual_of(Voc, skos:'ConceptScheme')
	->  true
	;   rdf_assert(Voc, rdf:type, skos:'ConceptScheme', amalgame_vocs)
	),
	forall(member(M,Props),
	       (   M =.. [PropName, Value],
		   format(atom(URI), '~w~w', [NS,PropName]),
		   rdf_assert(Voc, URI, Value, amalgame_vocs)
	       )).

count_concepts(Voc, Count) :-
	voc_format(Voc, Format),
	(   Format == skos
	;   Format == skosxl
	),
	findall(Concept,
		rdf_has(Concept, skos:inScheme, Voc),
		Concepts),
	length(Concepts, Count),
	print_message(informational, map(found, 'SKOS Concepts', Voc, Count)).

count_concepts(Voc, Count) :-
	voc_format(Voc, owl),
	rdf(Voc, rdf:type, owl:'Ontology', Graph),
	findall(Concept,
		rdf(Concept, rdf:type, owl:'Class', Graph),
		Concepts),
	length(Concepts, Count),
	print_message(informational, map(found, 'SKOS Concepts', Voc, Count)).

count_concepts(Voc, 0) :- voc_format(Voc, null).

count_prefLabels(Voc, Count) :-
	findall(Label,
		(   rdf_has(Concept, skos:inScheme, Voc),
		    (	rdf_has(Concept, skos:prefLabel, literal(Label))
		    ;	rdf_has(Concept, skosxl:prefLabel, Label)
		    )
		),
		Labels),
	length(Labels, Count),
	print_message(informational, map(found, 'SKOS preferred labels', Voc, Count)).

count_altLabels(Voc, Count) :-
	findall(Label,
		(   rdf_has(Concept, skos:inScheme, Voc),
		    (	rdf_has(Concept, skos:altLabel, literal(Label))
		    ;	rdf_has(Concept, skosxl:altLabel, Label)
		    )
		),
		Labels),
	length(Labels, Count),
	print_message(informational, map(found, 'SKOS alternative labels', Voc, Count)).

count_mapped_concepts(Voc, Count) :-
	findall(C,
		(   rdf_has(C, skos:inScheme, Voc),
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
