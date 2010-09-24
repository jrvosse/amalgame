:- module(voc_stats,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(components(messages)).


:- use_module(amalgame(skos/vocabularies)).

:- http_handler(amalgame(list_skos_vocs),       http_list_skos_vocs,     []).
:- http_handler(amalgame(compute_voc_stats),    http_compute_voc_stats,  []).
:- http_handler(amalgame(clear_voc_stats),      http_clear_voc_stats,    []).
:- http_handler(amalgame(partition_voc),        http_partition_voc,      []).
:- http_handler(amalgame(delete_partition_voc),	http_delpart_voc,        []).

%%	http_list_skos_vocs(+Request) is det.
%
%	HTTP handler returning an HTML page listing of all skos vocabularies with statistics.

http_list_skos_vocs(_Request) :-
	reply_html_page(cliopatria(default),
			[title('SKOS vocabularies')
			],
			[ h4('SKOS concept schemes in the RDF store'),
			  \show_schemes
			]).

http_compute_voc_stats(Request) :-
	http_parameters(Request, [voc(all, [])]),
	authorized(write(amalgame_cache, write)),
	call_showing_messages(voc_ensure_stats(all),
			      [head(title('Amalgame: calculating vocabulary stats'))]).

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


%%	http_clear_voc_stats(?Request) is det.
%
%	Clears named graphs with cached amalgame results.

http_clear_voc_stats(_Request):-
	authorized(write(amalgame_cache, clear)),
	call_showing_messages(voc_clear_stats,
			      [head(title('Amalgame: clearing caches'))]).

http_partition_voc(Request) :-
	authorized(write(default, partition(sample))),
	http_parameters(Request,
			[voc(Graph, [])
			]),
	voc_partition(Graph, Partitioning),
	reply_html_page(cliopatria(default),
			[title('Amalgame: partitioned vocabulary')
			],
			[ h4('Amalgame: partitioned vocabulary (mapped vs unmapped concepts).'),
			  table([
				 id(skosvoctable)],
				[
				 tr([th('Nr'),
				     th('Name'),
				     th('# Concepts'),
				     th('# prefLabels'),
				     th('# altLabels'),
				     th('# not mapped'),
				     th('# mapped'),
				     th('%'),
				     th('Example concept'),
				     th('License')
				    ]),
				 \show_schemes(Partitioning, 1, [0, 0, 0, 0, 0])
				])
			]).

http_delpart_voc(_Request):-
	authorized(write(amalgame_cache, clear)),
	call_showing_messages(voc_delpart,
			      [head(title('Amalgame: deleting partioning results'))]).

show_schemes -->
	{
	 findall(Voc, rdfs_individual_of(Voc, skos:'ConceptScheme'), Schemes),
	 http_link_to_id(http_clear_voc_stats, [], CacheLink),
	 http_link_to_id(http_compute_voc_stats, [voc(all)], ComputeLink),
	 http_link_to_id(http_delpart_voc, [], DelPartLink)
	},
	html([
	      table([
		     id(skosvoctable)],
		    [
		     tr([th('Nr'),
			 th('Name'),
			 th('# Concepts'),
			 th('# prefLabels'),
			 th('# altLabels'),
			 th('# not mapped'),
			 th('# mapped'),
			 th('%'),
			 th('Example concept'),
			 th('License')
			]),
		     \show_schemes(Schemes, 1, [0, 0, 0, 0, 0])
		    ]),
	      ul([], [
		      li([a([href(ComputeLink)], 'compute'), ' missing statistics.']),
		      li(a([href(CacheLink)], 'clear vocabulary statistics cache')),
		      li(a([href(DelPartLink)], 'delete partitioning results'))
		     ])
	     ]).

show_schemes([], _, [C, P, A, M , U]) -->
	html(tr([class(finalrow)],
		[
		 td(''),
		 td('Total'),
		 td([style('text-align: right')],C),
		 td([style('text-align: right')],P),
		 td([style('text-align: right')],A),
		 td([style('text-align: right')],U),
		 td([style('text-align: right')],M),
		 td(''),td(''), td('')
		])).
show_schemes([Voc|Tail], Nr, [C,P,A,M,U]) -->
	{
	 http_link_to_id(http_compute_voc_stats,
			 [voc(Voc),
			  stat(numberOfConcepts),
			  stat(numberOfPrefLabels),
			  stat(numberOfAltLabels),
			  stat(numberOfMappedConcepts)
			 ],
			 MissingLink),
	 http_link_to_id(http_partition_voc, [voc(Voc)], SplitLink),
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
	     (	 CCount = 0
	     ->	 MPercent = '-'
	     ;	 Perc is 100*(MCount/CCount),
		 format(atom(MPercent), '(~2f%)', [Perc])
	     ),
	     UCount is CCount - MCount, NewU is U + UCount
	 ;   NewM = M, NewU = U, MCount = MissingValue, UCount = MissingValue
	 ),
	 (   MCount = CCount; MCount = 0
	 ->  Split = ''
	 ;   Split = a([href(SplitLink), title('Partition into mapped/unmapped concepts')], ' (partition) ')
	 ),
	 (rdf_has(Example, skos:inScheme, Voc)
	 ->  true
	 ;   Example = '-'
	 ),
	 (rdf_has(Voc, dcterms:rights, RightsO)
	 ->  (   RightsO = literal(_)
	     ->  literal_text(RightsO, RightsT),
		 truncate_atom(RightsT, 30, RightsTrunc),
		 http_link_to_id(list_resource, [r(Voc)], LinkToVoc),
		 Rights=a([href(LinkToVoc)], RightsTrunc)
	     ;	 Rights=a([href(RightsO)],'License')
	     )
	 ;   Rights = '-'
	 )
	},
	html(tr([td(Nr),
		 td(\rdf_link(Voc, [resource_format(label)])),
		 td([style('text-align: right')],CCount),
		 td([style('text-align: right')],PCount),
		 td([style('text-align: right')],ACount),
		 td([style('text-align: right')],[Split, UCount]),
		 td([style('text-align: right')],MCount),
		 td([style('text-align: right')],MPercent),
		 td(\rdf_link(Example, [resource_format(label)])),
		 td(Rights)
		])),
	show_schemes(Tail, NewNr, [NewC, NewP, NewA, NewM, NewU]).
