:- module(am_skosvocs,
          [show_schemes/2
          ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

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

show_schemes -->
	{
	 skos_statistics(Schemes)
	},
	html(
	     table([
		    id(skosvoctable)],
		   [
		    tr([
			th('IRI'),
			th('Name'),
			th('# Concepts'),
			th('# prefLabels'),
			th('# altLabels'),
			th('Example concept')
		       ]),
		    \show_schemes(Schemes,0)
		   ])
	    ).

show_schemes([], Total) -->
	html(tr([id(finalrow)],
		[
		 td(''),
		 td('Total'),
		 td([style('text-align: right')],Total),
		 td('')
		])).
show_schemes([H:Stats|Tail], Number) -->
	{
	 http_link_to_id(list_resource, [r(H)], VLink),
	 member(numberOfConcepts(Count), Stats),
	 member(numberOfPrefLabels(PCount), Stats),
	 member(numberOfAltLabels(ACount), Stats),
	 NewNumber is Number + Count,
	 label_property(P),
	 rdf_has(H, P, Value),
	 text_of_literal(Value, Label),
	 (   rdf(Example, skos:inScheme, H)
	 ->  http_link_to_id(list_resource, [r(Example)], ELink)
	 ;   Example = '-', ELink = ''
	 )
	},
	html(tr([
		 td(a([href(VLink)],\turtle_label(H))),
		 td(a([href(VLink)]), Label),
		 td([style('text-align: right')],Count),
		 td([style('text-align: right')],PCount),
		 td([style('text-align: right')],ACount),
		 td(a([href(ELink)],\turtle_label(Example)))
		])),
	show_schemes(Tail, NewNumber).












