:- module(align_stats, []). % No exports, HTTP entry points only

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(version)).


:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(components(messages)).

:- use_module(amalgame(compare/overlap)).
:- use_module(amalgame(mappings/alignment)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(amalgame(mappings/map)).


:- http_handler(amalgame(clear_alignments),   http_delete_alignment_graphs, []).
:- http_handler(amalgame(clear_alignstats),   http_clear_alignstats,    []).
:- http_handler(amalgame(compute_stats),      http_compute_stats,       []).
:- http_handler(amalgame(find_overlap),       http_list_overlap,        []).
:- http_handler(amalgame(list_alignment),     http_list_alignment,      []).
:- http_handler(amalgame(list_alignments),    http_list_alignments,     []).
:- http_handler(amalgame(split_alignment),    http_split_alignment,     []).
:- http_handler(amalgame(skos_export),        http_skos_export,         []).
:- http_handler(amalgame(sample_alignment),   http_sample_alignment,	[]).

%%	http_list_alignments(+Request) is det.
%
%	HTTP handler returning list of all alignments in HTML.

http_list_alignments(_Request) :-
	reply_html_page(cliopatria(default),
			[title('Alignments')
			],
			[ h4('Amalgame: Alignments in the RDF store'),
			  \show_alignments
			]).

%%	http_list_alignment(+Request) is det.
%
%	HTTP handler returning list of all alignments in HTML.

http_list_alignment(Request) :-
	http_parameters(Request, [graph(Graph, [])]),
	reply_html_page(cliopatria(default),
			[title('Amalgame: alignment manipulation')
			],
			[ h4('Alignment manipulations'),
			  \show_alignment_overview(Graph)
			]).

http_split_alignment(Request) :-
	http_parameters(Request,
			[graph(Graph, []),
			 condition(Condition, [])
			]),
	split_alignment(Graph, Condition, OutGraphs),
	forall(member(Out, OutGraphs),
	       align_ensure_stats(all(Out))),
	reply_html_page(cliopatria(default),
			[title('Amalgame: alignment splitted')
			],
			[ h4('Alignment splitted'),
			  div('Original alignment:'),
			  table([class(aligntable)],
				[tr([
				     th('Abr'),	th('Source'), th('# mapped'), th('Target'), th('# mapped'), th('Format'), th('# maps'), th('Named Graph URI')
				    ]),
				 \show_alignments([Graph], 0)
				]),
			  div('splitted into'),
			  table([class(aligntable)],
				[tr([
				     th('Abr'),	th('Source'), th('# mapped'), th('Target'), th('# mapped'), th('Format'), th('# maps'), th('Named Graph URI')
				    ]),
				 \show_alignments(OutGraphs, 0)
				])
			]).

http_compute_stats(Request) :-
	http_link_to_id(http_list_alignments, [], Link),
	http_parameters(Request, [graph(all, [])]),
	Title = 'Amalgame: computing key alignment statistics',
	call_showing_messages(compute_stats,
			      [head(title(Title)),
			       header(h4(Title)),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['alignment overview']),
					   ' to inspect results.']))
			      ]).

http_compute_stats(Request) :-
	http_parameters(Request,
			[graph(Graph, []),
			 stat(Stats, [list(atom)])
			]),
	forall(member(Stat, Stats),
	       (   Type =.. [Stat, Graph],
		   align_ensure_stats(Type)
	       )
	      ),
	http_redirect(moved, location_by_id(http_list_alignments), Request).

compute_stats :-
	findall(G, is_alignment_graph(G,_), Graphs),!,
	forall(member(G, Graphs),
	       (   align_ensure_stats(count(G)),
		   align_ensure_stats(mapped(G)),
		   align_ensure_stats(source(G)),
		   align_ensure_stats(target(G))
	       )
	      ).


%%	http_list_overlap(+Request) is det.
%
%	HTTP handler generating a page with mapping overlap statistics.

http_list_overlap(_Request) :-
	reply_html_page(cliopatria(default),
			[
			 title('Amalgame: alignment overlap')
			],
			[
			 div([class(lfloat)],
			     [
			      h4('Alignments in the RDF store'),
			      \show_alignments
			     ]),
			 div([class(lfloat)],
			     [
			      h4('Alignment overlap'),
			      \show_overlap
			     ])
			]).

%%	http_clear_alignstats(?Request) is det.
%
%	Clears named graphs with cached amalgame results.

http_clear_alignstats(_Request):-
	authorized(write(amalgame_cache, clear)),
	Title = 'Amalgame: clearing caches',
	http_link_to_id(http_compute_stats, [graph(all)], RecomputeLink),
	http_link_to_id(http_list_alignments, [graph(all)], ListLink),

	call_showing_messages(clear_alignstats,
			      [head(title(Title)),
			       header(h4(Title)),
			       footer(div([h4('Cleared all caches'),
					   p('You now may want to proceed by:'),
					   ul(
					      [
					       li(['returning to the ', a([href(ListLink)], 'alignment overview page')]),
					       li(['recomputing ', a([href(RecomputeLink)],'all key stats')])
					      ])
					  ]))
			      ]).



http_delete_alignment_graphs(_Request) :-
	authorized(write(amalgame_cache, clear)),
	authorized(write(default, unload(_))),
	call_showing_messages(delete_alignment_graphs,
			      [head(title('Amalgame: deleting graphs'))]).


http_skos_export(Request) :-
	http_parameters(Request, [graph(Graph, [description('URI of source graph to export from')]),
				  name(TargetGraph, [default(default_export_graph)]),
				  min(Min, [between(0.0, 1.0), description('Minimal confidence level')]),
				  max(Max, [between(0.0, 1.0), description('Maximal confidence level')]),
				  relation(MapRelation,
					   [default('http://www.w3.org/2004/02/skos/core#closeMatch')])
				 ]),

	(rdf_graph(TargetGraph) -> rdf_unload(TargetGraph); true),
	edoal_to_triples(Request, Graph, TargetGraph, [relation(MapRelation), min(Min), max(Max)]),
	http_link_to_id(list_graph, [graph(TargetGraph)], ListGraph),
	http_redirect(moved, ListGraph, Request).


http_sample_alignment(Request) :-
	authorized(write(default, create(sample))),
	http_parameters(Request, [graph(Graph, [length > 0]),
				  size(Size, [nonneg]),
				  name(Name, [length > 0]),
				  method(Method, [])
				 ]),
	sample(Request, Method, Graph, Name, Size),
	http_link_to_id(http_list_alignment, [graph(Name)], LinkToSample),
	reply_html_page(cliopatria(default),
			title('Amalgame sampled alignment'),
			div(['Sampled alignment ', Graph,
			     ' into named graph ', a([href(LinkToSample)], Name),
			     ' of size ', Size, ' (', Method, ')' ])
		       ).

sample(Request, Method, Graph, Name, Size) :-
	(   rdf_graph(Name)
	->  rdf_unload(Name),
	    rdf_retractall(Name,_,_,amalgame)
	;   true
	),
	get_time(T), format_time(atom(Time), '%a, %d %b %Y %H:%M:%S %z', T),
	logged_on(User, 'anonymous'),
	git_component_property('ClioPatria', version(CP_version)),
	git_component_property('amalgame',   version(AG_version)),
	format(atom(Version), 'Made using Amalgame ~w/Cliopatria ~w', [AG_version, CP_version]),
	http_current_host(Request, Hostname, Port, [global(true)]),
	memberchk(request_uri(ReqURI), Request),
	memberchk(protocol(Protocol), Request),
	format(atom(ReqUsed), '~w://~w:~w~w', [Protocol,Hostname,Port,ReqURI]),
	rdf_bnode(Provenance),
	rdf_assert(Provenance, rdf:type, amalgame:'Provenance', Name),
	rdf_assert(Provenance, dcterms:title, literal('Provenance: about this sample'), Name),
	rdf_assert(Provenance, dcterms:source, Graph, Name),
	rdf_assert(Provenance, dcterms:date, literal(Time), Name),
	rdf_assert(Provenance, dcterms:creator, literal(User), Name),
	rdf_assert(Provenance, owl:versionInfo, literal(Version), Name),
	rdf_assert(Provenance, amalgame:request, literal(ReqUsed), Name),
	rdf_assert(Provenance, amalgame:sampleSize, literal(type(xsd:int, Size)), Name),
	rdf_assert(Provenance, amalgame:sampleMethod, literal(Method), Name),
	rdf_assert(Name, amalgame:provenance, Provenance, Name),
	rdf_assert(Name, rdf:type, amalgame:'SampleAlignment', Name),

	align_get_computed_props(Graph, SourceProps),
	findall(member(M),
		member(member(M), SourceProps),
		Members),
	assert_alignment_props(Name, Members, Name),

	findall(Map, has_map(Map, _, Graph), Maps),
	length(Maps, Length),

	randset(Size, Length, RandSet),
	assert_from_list(Method, Name, Graph, 1, RandSet, Maps).

assert_from_list(_,_,_,_,[], _).
assert_from_list(Method, Name, Graph, Nr, [Rand|RandSet], [[E1,E2]|Maps]) :-
	(   Rand = Nr
	->  has_map([E1,E2], _, Options, Graph),!,
	    (	Method = randommaps
	    ->	AltMaps = [E1-E2-Options]
	    ;	Method = random_alt_in_graph
	    ->	findall(E1-E2a-MapOptions,
			has_map([E1,E2a], _, MapOptions, Graph),
			AltSourceMaps),
		findall(E1a-E2-MapOptions,
			has_map([E1a,E2], _, MapOptions, Graph),
			AltTargetMaps),
		append(AltSourceMaps, AltTargetMaps, AltMapsDoubles),
		sort(AltMapsDoubles, AltMaps)
	    ;	Method = random_alt_all
	    ->	findall(E1-E2a-[source(G)|MapOptions],
			(   has_map([E1,E2a], _, MapOptions, G:_),
			    rdfs_individual_of(G, amalgame:'LoadedAlignment')
			),
			AltSourceMaps),
		findall(E1a-E2-[source(G)|MapOptions],
			(   has_map([E1a,E2], _, MapOptions, G:_),
			    rdfs_individual_of(G, amalgame:'LoadedAlignment')
			),
			AltTargetMaps),
		append(AltSourceMaps, AltTargetMaps, AltMapsDoubles),
		sort(AltMapsDoubles, AltMaps)
	    ),
	    assert_map_list(AltMaps, Name),
	    NewRandSet = RandSet,
	    NewMaps = Maps
	;   NewRandSet = [Rand|RandSet],
	    NewMaps = [[E1,E2]|Maps]
	),
	NewNr is Nr + 1,
	assert_from_list(Method, Name, Graph, NewNr, NewRandSet, NewMaps).

assert_map_list([],_).
assert_map_list([H|T], Graph) :-
	H=E1-E2-Options,
	(   has_map([E1,E2], edoal, Graph)
	->  true
	;   assert_cell(E1,E2, [graph(Graph), alignment(Graph) | Options])
	),
	assert_map_list(T,Graph).

clear_alignstats :-
	align_clear_stats(all),
	clear_overlaps.

delete_alignment_graphs :-
	align_ensure_stats(found),
	findall(Graph, is_alignment_graph(Graph, _Format), Graphs),
	forall(member(Graph, Graphs),
	       (
		   print_message(informational, map(cleared, graph, 1, Graph)),
		   rdf_unload(Graph)
	       )
	      ),
	align_clear_stats(all).


show_alignment_overview(Graph) -->
	{
	 align_ensure_stats(source(Graph)),
	 align_ensure_stats(target(Graph)),
	 align_ensure_stats(count(Graph)),
	 align_ensure_stats(mapped(Graph)),
	 align_ensure_stats(format(Graph)),

	 http_link_to_id(http_evaluator, [], EvalLink),
	 http_link_to_id(list_graph, [graph=Graph], GraphLink),
	 http_link_to_id(http_split_alignment, [graph=Graph], SplitLink),
	 http_link_to_id(http_skos_export, [], ExportLink),
	 http_link_to_id(http_sample_alignment, [], SampleLink),

	 rdf_equal(skos:closeMatch, DefaultRelation),
	 supported_map_relations(MapRelations),
	 ExportOption = div(form([action(ExportLink)],
				      [input([type(submit), class(submit),
					      value('Export maps')
					     ],[]),
				       ' to graph ',
				       input([type(text), class(target),
					      name(name), value(exportedgraph),
					      size(10)],[]),
				       input([type(hidden),
					      name(graph),
					      value(Graph)],[]),
				      ' with confidence level between : ',
				       input([type(text),
					      name(min),
					      value('0.0'),
					      size(3)
					     ],[]),
				       ' and ',
				       input([type(text),
					      name(max),
					      value('1.0'),
					      size(3)
					     ],[]),
				       ' and default map relation: ',
				       select([name(relation)],
					      [\show_mapping_relations(MapRelations, DefaultRelation)]),
				       ' to a single triple format'
				      ]
				     ))

	},
	html([p(['Alignment graph: ', a([href(Graph)], Graph)]),
	      %table(tr([th(property), th(value)]), \show_align_props(Graph, Props)),
	      p('Actions: '),
	      ul([
		  li(a([href(GraphLink)], 'View/download graph'))
		 ]),
	      p('Create new graphs from this one: '),
	      div([id(alignoperations)],
		  [
		   form([action(EvalLink)],
			[input([type(hidden),  name(graph), value(Graph)],[]),
			 input([class(submit), type(submit), value('Evaluate')]),
			 ' to graph ',
			 input([type(text), class(target), name(target), size(10),
				value(evaluation_results)], [])
			]
		     ),
		  div(form([action(SampleLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([class(submit), type(submit), value('Sample')],[]),
		      ' to graph ',
		      input([type(text), name(name), value(sample), size(10), class(target)], []),
		      ' sample size N=',
		      input([type(text), name(size), value(25), size(4)],[]),
		      ' with method ',
		      select([name(method)],
			    [option([selected(selected), value(randommaps)],['N random mappings']),
			     option([value(random_alt_in_graph)],['N random mapped concepts, with alternative mappings in same graph']),
			     option([value(random_alt_all)], ['N random mapped concepts, with alternative mappings from all loaded graphs'])
			    ])
		     ])
		     ),
		   ExportOption,
		   div(form([action(SplitLink)],
			   [
			    input([type(hidden), name(graph), value(Graph)],[]),
			    input([type(submit), class(submit), value('Partition')],[]),
			    ' on ',
			    select([name(condition)],
				   [
				    option([selected(selected), value(sourceType)],['Source type']),
				    option([value(targetType)],['Target type'])
				   ])
			   ]))
		  ])
	     ]
	    ).

show_mapping_relations([],_) --> !.
show_mapping_relations([H|T], Selected) -->
	{
	  (   H=Selected
	  ->  SelectedAttr = selected(selected)
	  ;   SelectedAttr = true
	  )
	},
	html(option([SelectedAttr, name(relation), value(H)], \turtle_label(H))),
	show_mapping_relations(T, Selected).


show_alignment(Graph) -->
	{
	 nickname(Graph, Nick),
	 http_link_to_id(http_list_alignment, [graph(Graph)], VLink)
	},
	html(a([href(VLink),title(Graph)],[Nick, ' '])).

show_graph(Graph) -->
	{
	 http_link_to_id(http_list_alignment, [graph(Graph)], VLink)
	},
	html(a([href(VLink)],\turtle_label(Graph))).

show_countlist([], Total) -->
	html(tr([class(finalrow)],
		[td(''),
		 td([style('text-align: right')], Total),
		 td('Total (unique alignments)')
		])).

show_countlist([Count:O:Example|T], Number) -->
	{
	  NewNumber is Number + Count
	},
	html(tr([
		 td(\show_overlap_graphs(O)),
		 td([style('text-align: right')],Count),
		 \show_example(Example)
		])),
	show_countlist(T,NewNumber).



show_example([E1, E2]) -->
	html([td(\rdf_link(E1)),
	      td(\rdf_link(E2))
	     ]).

show_overlap_graphs(Overlap) -->
	{
	 findall(Nick,
		 (   rdf(Overlap, amalgame:member, M),
		     nickname(M,Nick)
		 ), Graphs),
	 sort(Graphs, Sorted),
	 atom_chars(Nicks, Sorted),
	 http_link_to_id(http_list_alignment, [graph(Overlap)], Olink)
	},
	html([a([href(Olink)], Nicks)]).

show_alignments -->
	{
	 align_ensure_stats(found),
	 findall(Graph,
		 (   is_alignment_graph(Graph,_),
		     \+ rdfs_individual_of(Graph, amalgame:'OverlapAlignment')
		 ),
		 AllGraphs),
	 sort(AllGraphs, Graphs),
	 http_link_to_id(http_clear_alignstats, [], CacheLink),
	 http_link_to_id(http_delete_alignment_graphs, [], ClearAlignLink),
	 Note = ['These are cached results, ',
		 a([href(CacheLink)], 'clear cache'), ', ',
		 a([href(ClearAlignLink)], 'clear all alignments from repository (!)')
		]
	},
	html([div([id(cachenote)], Note),
	      table([id(aligntable)],
		    [tr([
			 th('Abr'),
			 th('Source'),
			 th('# mapped'),
			 th('Target'),
			 th('# mapped'),
			 th('Format'),
			 th('# maps'),
			 th('Named Graph URI')

		       ]),
		    \show_alignments(Graphs,0)
		   ])
	     ]).

show_alignments([],Total) -->
	{
		 http_link_to_id(http_compute_stats, [graph(all)], ComputeLink)
	},
	html(tr([class(finalrow)],
		[td(''),
		 td(''),
		 td(''),
		 td(''),
		 td(''),
		 td(''),
		 td([style('text-align: right')],Total),
		 td(a([href(ComputeLink), title('Click to compute missing stats')], 'Total (double counting)'))
		])).

show_alignments([Graph|Tail], Number) -->
	{
	 http_link_to_id(http_compute_stats,
			 [graph(Graph),
			  stat(count),
			  stat(source),
			  stat(target),
			  stat(mapped)
			 ],
			 MissingLink),
	 MissingValue = a([href(MissingLink)],'?'),
	 is_alignment_graph(Graph, Format),
	 align_get_computed_props(Graph, Props),
	 (   memberchk(count(literal(type(_,Count))), Props)
	 ->  NewNumber is Number + Count
	 ;   NewNumber = Number, Count = MissingValue
	 ),
	 (   memberchk(alignment(A), Props)
	 ->  http_link_to_id(list_resource, [r(A)], AlignLink),
	     FormatLink = a([href(AlignLink)], Format)
	 ;   FormatLink = Format
	 ),
	 (   memberchk(source(SourceGraph), Props)
	 ->  Source = \rdf_link(SourceGraph, [resource_format(label)])
	 ;   Source = MissingValue
	 ),
	 (   memberchk(target(TargetGraph), Props)
	 ->  Target = \rdf_link(TargetGraph, [resource_format(label)])
	 ;   Target = MissingValue
	 ),
	 (   memberchk(mappedSourceConcepts(MSC), Props)
	 ->  SourcesMapped = literal(type(_,MSC))
	 ;   SourcesMapped = MissingValue
	 ),
	 (   memberchk(mappedTargetConcepts(MTC), Props)
	 ->  TargetsMapped = literal(type(_,MTC))
	 ;   TargetsMapped = MissingValue
	 )
	},
	html(tr([
		 td(\show_alignment(Graph)),
		 td(Source),
		 td([style('text-align: right')],SourcesMapped),
		 td(Target),
		 td([style('text-align: right')],TargetsMapped),
		 td(FormatLink),
		 td([style('text-align: right')],Count),
		 td(\show_graph(Graph))
		])),
	show_alignments(Tail, NewNumber).

show_overlap -->
	{
	 find_overlap(CountList, [cached(Cached)]),
	 (   Cached
	 ->  http_link_to_id(http_clear_alignstats, [], CacheLink),
	     Note = ['These are results from the cache, ', a([href(CacheLink)], 'clear cache'), ' to recompute']
	 ;   Note = ''
	 )
	},
	html([
	      div([id(cachenote)], Note),
	      table([id(aligntable)],
		    [
		     tr([th('Overlap'),th('# maps'), th([colspan(2)],'Example')]),
		     \show_countlist(CountList,0)
		    ]
		  )
	     ]).


show_align_props(_Graph, []) --> !.

show_align_props(Graph, [PropValue|Tail]) -->
	{
	 PropValue =.. [Prop, Value],
	 rdf_equal(amalgame:'', NS),
	 format(atom(PropURI), '~w~w', [NS, Prop])
	},
	html(tr([
		 td(\rdf_link(PropURI)),
		 td(\rdf_link(Value))
		])),
	show_align_props(Graph, Tail).

