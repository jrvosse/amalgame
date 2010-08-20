:- module(am_skosvocs,
          [skos_statistics/1
          ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdfs)).

%%	skos_statistics(-Stats) is det.
%
%	Return a list of all skos concept schemes with statistics.
%	Stats is a list of the form URI:VocStat, where URI is the URI of
%	the scheme, and VocStat is a list with statistics on that
%	scheme.  Currently supported stats include:
%	* numberOfConcepts(N)
%	* numberOfPrefLabels(N)
%	* numberOfAltLabels(N)
%
%	Side effect: These statistics will also be asserted as RDF
%	triples to the 'amalgame' named graph, using similarly named
%	properties with the 'amalgame:' namespace prefix. These asserted
%	triples will be used in subsequent calls for efficiency reasons.
%
%	See also http_clear_cache/1.
%
skos_statistics(Stats) :-
	findall(Scheme,
		rdfs_individual_of(Scheme, skos:'ConceptScheme'),
		Schemes),
	skos_vocs_stats(Schemes, [], Stats).

strip_sort_value(_:V:S, V:S).

skos_vocs_stats([], Unsorted, Results) :-
	sort(Unsorted, Sorted),
	maplist(strip_sort_value, Sorted, Results).

skos_vocs_stats([Voc|Tail], Accum, Stats) :-
	skos_voc_stats(Voc, SortValue, VocStats),
	skos_vocs_stats(Tail, [SortValue:Voc:VocStats|Accum], Stats).

skos_voc_stats(Voc, Count, Stats) :-
	rdf(Voc,amalgame:numberOfConcepts,   literal(Count), amalgame),
	rdf(Voc,amalgame:numberOfPrefLabels, literal(PCount), amalgame),
	rdf(Voc,amalgame:numberOfAltLabels,  literal(ACount), amalgame),
	Stats = [numberOfConcepts(Count),
		 numberOfPrefLabels(PCount),
		  numberOfAltLabels(ACount)
		].

skos_voc_stats(Voc, Count, Stats) :-
	count_concepts(Voc,   Count),
	count_prefLabels(Voc, PCount),
	count_altLabels(Voc,  ACount),
	rdf_assert(Voc,amalgame:numberOfConcepts, literal(Count), amalgame),
	rdf_assert(Voc,amalgame:numberOfPrefLabels, literal(PCount), amalgame),
	rdf_assert(Voc,amalgame:numberOfAltLabels, literal(ACount), amalgame),
	Stats = [numberOfConcepts(Count),
		 numberOfPrefLabels(PCount),
		 numberOfAltLabels(ACount)
		].

count_concepts(Voc, Count) :-
	findall(Concept,
		rdf(Concept, skos:inScheme, Voc),
		Concepts),
	length(Concepts, Count).

count_prefLabels(Voc, Count) :-
	findall(Label,
		(   rdf(Concept, skos:inScheme, Voc),
		    rdf_has(Concept, skos:prefLabel, literal(Label))
		),
		Labels),
	length(Labels, Count).

count_altLabels(Voc, Count) :-
	findall(Label,
		(   rdf(Concept, skos:inScheme, Voc),
		    rdf_has(Concept, skos:altLabel, literal(Label))
		),
		Labels),
	length(Labels, Count).













