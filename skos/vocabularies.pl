:- module(am_skosvocs,
          [
          ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

:- http_handler(amalgame(list_skos_vocs),     http_list_skos_vocs,     []).

http_list_skos_vocs(_Request) :-
	skos_statistics(Schemes),
	reply_html_page(cliopatria(default),
			[title('SKOS vocabularies'),
			 style('#skosvoctable { border-collapse: collapse; border: solid #CCCCCC; }'),
			 style('#skosvoctable td, th { border-left: solid #CCCCCC; }'),
			 style('#finalrow td { border-top: solid #AAAAAA; }')
			],
			[ h4('SKOS concept schemes in the RDF store'),
			  table([id(skosvoctable)],
				[
				 tr([
				     th('IRI'),
				     th('Name'),
				     th('# Concepts'),
				     th('Example concept')
				    ]),
				 \show_schemes(Schemes,0)
				])
			]).

skos_statistics(Schemes) :-
	findall(Scheme,
		rdfs_individual_of(Scheme, skos:'ConceptScheme'),
		Schemes),
	skos_vocs_stats(Schemes).

skos_vocs_stats([]).
skos_vocs_stats([Voc|Tail]) :-
	(   rdf(Voc, amalgame:numberOfConcepts, _)
	->  true
	;   count_concepts(Voc, Count),
	    rdf_assert(Voc,amalgame:numberOfConcepts, literal(Count))
	),
	skos_vocs_stats(Tail).

count_concepts(Voc, Count) :-
	findall(Concept,
		rdf(Concept, skos:inScheme, Voc),
		Concepts),
	length(Concepts, Count).

show_schemes([], Total) -->
	html(tr([id(finalrow)],
		[
		 td(''),
		 td('Total'),
		 td([style('text-align: right')],Total),
		 td('')
		])).
show_schemes([H|Tail], Number) -->
	{
	 http_link_to_id(list_resource, [r(H)], VLink),
	 rdf(H, amalgame:numberOfConcepts, literal(Count)),
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
		 td(a([href(ELink)],\turtle_label(Example)))
		])),
	show_schemes(Tail, NewNumber).












