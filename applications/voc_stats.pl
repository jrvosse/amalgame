:- module(voc_stats,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

:- use_module(amalgame(skos/vocabularies)).

:- http_handler(amalgame(list_skos_vocs),     http_list_skos_vocs,     []).
:- http_handler(amalgame(compute_voc_stats),  http_compute_voc_stats,  []).

%%	http_list_skos_vocs(+Request) is det.
%
%	HTTP handler returning an HTML page listing of all skos vocabularies with statistics.

http_list_skos_vocs(_Request) :-
	reply_html_page(cliopatria(default),
			[title('SKOS vocabularies'),
			 style('#skosvoctable { border-collapse: collapse; border: solid #CCCCCC; }'),
			 style('#skosvoctable td, th { border: solid #CCCCCC; }'),
			 style('#finalrow td { border-top: solid #AAAAAA; }')
			],
			[ h4('SKOS concept schemes in the RDF store'),
			  \show_schemes
			]).

http_compute_voc_stats(Request) :-
	http_parameters(Request, [voc(all, [])]),
	findall(V, rdfs_individual_of(V, skos:'ConceptScheme'), Vocs),!,
	forall(member(V, Vocs),
	       (   voc_ensure_stats(numberOfConcepts(V)),
		   voc_ensure_stats(numberOfPrefLabels(V)),
		   voc_ensure_stats(numberOfAltLabels(V)),
		   voc_ensure_stats(numberOfMappedConcepts(V))
	       )
	      ),
	http_redirect(moved, location_by_id(http_list_skos_vocs), Request).

http_compute_voc_stats(Request) :-
	http_parameters(Request,
			[voc(Graph, []),
			 stat(Stats, [list(atom)])
			]),
	forall(member(Stat, Stats),
	       (   Type =.. [Stat, Graph],
		   voc_ensure_stats(Type)
	       )
	      ),
	http_redirect(moved, location_by_id(http_list_skos_vocs), Request).


show_schemes -->
	{
	 findall(Voc, rdfs_individual_of(Voc, skos:'ConceptScheme'), Schemes),
	 length(Schemes, Count),
	 http_link_to_id(http_clear_cache, [], CacheLink),
	 http_link_to_id(http_compute_voc_stats, [voc(all)], ComputeLink),
	 Note = ['These are cached results, ',
		 a([href(CacheLink)], 'clear cache'), ' or ',
		 a([href(ComputeLink)], 'compute all'), ' missing statistics.' ]
	},
	html([
	      div(Note),
	      div([Count, ' SKOS concept schemes have been uploaded:']),
	      table([
		     id(skosvoctable)],
		    [
		     tr([td('Nr'),
			 th('IRI'),
			 th('Name'),
			 th('# Concepts'),
			 th('# prefLabels'),
			 th('# altLabels'),
			 th('# mapped'),
			 th('%'),
			 th('Example concept'),
			 th('Copyrights & licenses')
			]),
		     \show_schemes(Schemes, 1, [0, 0, 0,0])
		    ])
	     ]).

show_schemes([], _, [C, P, A, M]) -->
	html(tr([id(finalrow)],
		[
		 td(''), td(''),
		 td('Total'),
		 td([style('text-align: right')],C),
		 td([style('text-align: right')],P),
		 td([style('text-align: right')],A),
		 td([style('text-align: right')],M),
		 td(''),td(''), td('')
		])).
show_schemes([Voc|Tail], Nr, [C,P,A,M]) -->
	{
	 http_link_to_id(http_compute_voc_stats,
			 [voc(Voc),
			  stat(numberOfConcepts),
			  stat(numberOfPrefLabels),
			  stat(numberOfAltLabels),
			  stat(numberOfMappedConcepts)
			 ],
			 MissingLink),
	 MissingValue = a([href(MissingLink)],'?'),
	 NewNr is Nr + 1,
	 voc_get_computed_props(Voc, Props),
	 (   memberchk(numberOfConcepts(literal(type(_,  CCount))), Props)
	 ->  NewC is C + CCount
	 ;   NewC = C, CCount = MissingValue
	 ),
	 (   memberchk(numberOfPrefLabels(literal(type(_,PCount))), Props)
	 ->  NewP is P + PCount
	 ;   NewP = P, PCount = MissingValue
	 ),
	 (   memberchk(numberOfAltLabels(literal(type(_, ACount))), Props)
	 ->  NewA is A + ACount
	 ;   NewA = A, ACount = MissingValue
	 ),
	 (   memberchk(numberOfMappedConcepts(literal(type(_, MCount))), Props)
	 ->  NewM is M + MCount,
	     Perc is 100*(MCount/CCount),
	     format(atom(MPercent), '(~2f%)', [Perc])
	 ;   NewM = M, MCount = MissingValue
	 ),
	 (rdf_has(Example, skos:inScheme, Voc)
	 ->  true
	 ;   Example = '-'
	 ),
	 (rdf_has(Voc, dcterms:rights, RightsO)
	 ->  text_of_literal(RightsO, Rights)
	 ;   Rights = '-'
	 )
	},
	html(tr([td(Nr),
		 td(\rdf_link(Voc, [resource_format(plain)])),
		 td(\rdf_link(Voc, [resource_format(label)])),
		 td([style('text-align: right')],CCount),
		 td([style('text-align: right')],PCount),
		 td([style('text-align: right')],ACount),
		 td([style('text-align: right')],MCount),
		 td([style('text-align: right')],MPercent),
		 td(\rdf_link(Example, [resource_format(label)])),
		 td(Rights)
		])),
	show_schemes(Tail, NewNr, [NewC, NewP, NewA, NewM]).
