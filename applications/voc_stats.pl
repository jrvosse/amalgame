:- module(voc_stats,
	  [
	   show_schemes//0
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
:- use_module(applications(browse)).

:- use_module(amalgame(skos/vocabularies)).

:- http_handler(amalgame(list_skos_vocs),       http_list_skos_vocs,     []).
:- http_handler(amalgame(list_skos_voc),        http_list_skos_voc,      []).
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

http_list_skos_voc(Request) :-
	http_parameters(Request,
			[voc(Scheme, [])
			]),
	reply_html_page(cliopatria(default),
			[title('SKOS concept scheme')
			],
			[ \show_scheme(Scheme)
			]).

http_compute_voc_stats(Request) :-
	http_parameters(Request, [voc(all, [])]),
	authorized(write(amalgame_cache, write)),
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_ensure_stats(all),
			      [head(title('Amalgame: calculating vocabulary stats')),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]).

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
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_clear_stats,
			      [head(title('Amalgame: clearing caches')),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]).

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
				 \voctable_header,
				 \show_schemes(Partitioning, 1, [0, 0, 0, 0, 0])
				])
			]).

http_delpart_voc(_Request):-
	authorized(write(amalgame_cache, clear)),
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_delete_derived,
			      [head(title('Amalgame: deleting partioning results')),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]
			     ).

show_schemes -->
	{
	 \+  rdfs_individual_of(_Voc, skos:'ConceptScheme'),!,
	 http_link_to_id(load_library_ontology_form, [], LoadLink)
	},
	html(p([class('error novocs')],
		 ['Warning: No SKOS concept schemes have been ',
		  a([href(LoadLink)],'loaded'),
		  ' yet.'
		 ])
	    ).

show_schemes -->
	{
	 findall(Voc, rdfs_individual_of(Voc, skos:'ConceptScheme'), SchemesDoubles),
	 sort(SchemesDoubles, Schemes),
	 http_link_to_id(http_clear_voc_stats, [], CacheLink),
	 http_link_to_id(http_compute_voc_stats, [voc(all)], ComputeLink)
	},
	html([
	      table([
		     class(ag_skosvoctable)],
		    [
		     \voctable_header,
		     \show_schemes(Schemes, 1, [0, 0, 0, 0, 0])
		    ]),
	      ul([class(ag_voc_actions)],
		 [
		  li([a([href(ComputeLink)], 'compute'), ' missing statistics.']),
		  li(a([href(CacheLink)], 'clear overlap statistics cache')),
		  \li_del_derived
		 ])
	     ]).

li_del_derived -->
	{
	 \+ rdfs_individual_of(_, amalgame:'DerivedConceptScheme')
	},!.

li_del_derived -->
	{
	 rdfs_individual_of(_, amalgame:'DerivedConceptScheme'),
	 http_link_to_id(http_delpart_voc, [], DelPartLink)
	},
	html(li(a([href(DelPartLink)], 'delete derived concept schemes'))).

voctable_header -->
	html([tr([th([class(nr)],        'Nr'),
		  th([class(name)],	'Name'),
		  th([class(count)],     '# Concepts'),
		  th([class(preflabels)],'# prefLabels'),
		  th([class(altlabels)], '# altLabels'),
		  th([class(notmapped)], '# not mapped'),
		  th([class(mapped)],    '# mapped'),
		  th([class(pmapped)],   '%'),
		  th([class(example)],   'Example concept'),
		  th([class(license)],	'License')
		 ])
	     ]).

show_schemes([], _, [C, P, A, M , U]) -->
	html(tr([class(finalrow)],
		[
		 td([class(nr)], ''),
		 td([class(name)], 'Total'),
		 td([class(count), style('text-align: right')],C),
		 td([class(preflabels), style('text-align: right')],P),
		 td([clas(altlabels), style('text-align: right')],A),
		 td([class(notmapped), style('text-align: right')],U),
		 td([class(mapped), style('text-align: right')],M),
		 td([class(pmapped)], ''),
		 td([class(example)], ''),
		 td([class(license)], '')
		])).
show_schemes([Voc|Tail], Nr, [C,P,A,M,U]) -->
	{
	 findall(Label,
		 (   rdfs_individual_of(Voc, Class),
		     rdf_global_id(_NS:Label, Class)
		 ),
		 VocTypes),
	 sort(VocTypes, VocTypesUnique),
	 (   rdf(Voc, amalgame:subSchemeOf, _SuperScheme)
	 ->  SubSuper = subscheme
	 ;   SubSuper = superscheme
	 ),
	 atomic_list_concat([SubSuper|VocTypesUnique], ' ', VocTypesAtom),
	 http_link_to_id(http_compute_voc_stats,
			 [voc(Voc),
			  stat(numberOfConcepts),
			  stat(numberOfPrefLabels),
			  stat(numberOfAltLabels),
			  stat(numberOfMappedConcepts)
			 ],
			 MissingLink),
	 http_link_to_id(http_list_skos_voc, [voc(Voc)], VocLink),
	 rdf_display_label(Voc, VocName),
	 VocLabel = a([href(VocLink)],[VocName]),
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
	html(tr([class(VocTypesAtom)],[td([class(nr)], Nr),
		 td([class(name)],[VocLabel]),
		 td([class(count), style('text-align: right')],CCount),
		 td([class(preflabels), style('text-align: right')],PCount),
		 td([class(altlabels),  style('text-align: right')],ACount),
		 td([class(notmapped), style('text-align: right')],UCount),
		 td([class(mapped), style('text-align: right')],MCount),
		 td([class(pmapped), style('text-align: right')],MPercent),
		 td([class(example)],\rdf_link(Example, [resource_format(nslabel)])),
		 td([class(license)], Rights)
		])),
	show_schemes(Tail, NewNr, [NewC, NewP, NewA, NewM, NewU]).

show_scheme(Voc) -->
	{
	 voc_ensure_stats(all(Voc)),
	 rdf_display_label(Voc, VocLabel),
	 findall(Graph, rdf(_, skos:inScheme, Voc, Graph:_), GraphsDoubles),
	 sort(GraphsDoubles, Graphs)
	},
	(   { Graphs = [Graph] }
	->  html([
	      h1([class(align_overview_header)],
		 ['Vocabulary actions & details: ', VocLabel]),
		  % div([id(ag_graph_info)], \graph_info(Graph)),
		  div([id(ag_graph_as_resource), class(component)],
		      \graph_as_resource(Graph, [])),
		  div([id(ag_graph_basic_actions), class(component)],
		   [
		    'Basic actions on (skos:inScheme) graph: ', Graph,
		    \graph_actions(Graph)
		   ])
	     ])
	;   html([
		  h1([class(align_overview_header)], ['Warning: multiple graphs problem']),
		  p([], ['Thesaurus has skos:inScheme triples in multiple files. ',
			 'Please merge into one to access graph-based actions.'])
		 ])
	),
	html([
	      %p('Create a new graph from this one: '),
	      p('Other action: '),
	      ul([
		  \li_partition(Voc)
		 ]),
	      div([id(ag_voc_as_resource), class(component)],
		  [
		   \list_resource(Voc, [])
		  ])
	     ]).


li_partition(Voc) -->
	{
	 rdf(Voc, amalgame:numberOfConcepts, literal(type(_,CCount))),
	 rdf(Voc, amalgame:numberOfMappedConcepts, literal(type(_,MCount))),
	 http_link_to_id(http_partition_voc, [voc(Voc)], SplitLink),
	 Partition = a([href(SplitLink)],
		       'Partition into mapped/unmapped concepts')
	},
	(   { (CCount = MCount; MCount =0) }
	->  html([])
	;   html(li(Partition))
	).

