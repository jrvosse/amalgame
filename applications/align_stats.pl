:- module(align_stats, []). % No exports, HTTP entry points only

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

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
	style(Style),
	reply_html_page(cliopatria(default),
			[title('Alignments'),
			 Style
			],
			[ h4('Amalgame: Alignments in the RDF store'),
			  \show_alignments
			]).

%%	http_list_alignment(+Request) is det.
%
%	HTTP handler returning list of all alignments in HTML.

http_list_alignment(Request) :-
	style(Style),
	http_parameters(Request, [graph(Graph, [])]),
	reply_html_page(cliopatria(default),
			[title('Amalgame: alignment overview'),
			 Style
			],
			[ h4('Alignment overview'),
			  \show_alignment_overview(Graph)
			]).

http_split_alignment(Request) :-
	http_parameters(Request,
			[graph(Graph, []),
			 condition(Condition, [])
			]),
	style(Style),
	split_alignment(Graph, Condition, OutGraphs),
	align_clear_stats(found),
	reply_html_page(cliopatria(default),
			[title('Amalgame: alignment splitted'),
			 Style
			],
			[ h4('Alignment splitted'),
			  div([],OutGraphs)
			]).

http_compute_stats(Request) :-
	http_parameters(Request, [graph(all, [])]),
	call_showing_messages(compute_stats,
			      [head(title('Amalgame: computing alignment statistics'))]).

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
	style(Style),
	reply_html_page(cliopatria(default),
			[
			 title('Amalgame: alignment overlap'),
			 Style
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
	call_showing_messages(clear_alignstats,
			      [head(title('Amalgame: clearing caches'))]).



http_delete_alignment_graphs(_Request) :-
	authorized(write(amalgame_cache, clear)),
	authorized(write(default, unload(_))),
	call_showing_messages(delete_alignment_graphs,
			      [head(title('Amalgame: deleting graphs'))]).


http_skos_export(Request) :-
	http_parameters(Request, [graph(Graph, []),
				  relation(MapRelation, [default('http://www.w3.org/2004/02/skos/core#closeMatch')])
				 ]),
	format(atom(SkosGraph), '~p_skos', [Graph]),
	(rdf_graph(SkosGraph) -> rdf_unload(SkosGraph); true),
	edoal_to_skos(Graph, SkosGraph, [relation(MapRelation)]),
	http_link_to_id(list_graph, [graph(SkosGraph)], ListGraph),
	http_redirect(moved, ListGraph, Request).

http_sample_alignment(Request) :-
	authorized(write(default, create(sample))),
	http_parameters(Request, [graph(Graph, [length > 0]),
				  size(Size, [nonneg]),
				  name(Name, [length > 0]),
				  method(Method, [])
				 ]),
	sample(Method, Graph, Name, Size),
	http_link_to_id(http_list_alignment, [graph(Name)], LinkToSample),
	reply_html_page(cliopatria(default),
			title('Amalgame sampled alignment'),
			div(['Sampled alignment ', Graph,
			     ' into named graph ', a([href(LinkToSample)], Name),
			     ' of size ', Size, ' (', Method, ')' ])
		       ).
sample(Method, Graph, Name, Size) :-
	(   rdf_graph(Name)
	->  rdf_unload(Name),
	    rdf_retractall(Name,_,_,amalgame)
	;   true
	),
	rdf_assert(Name, rdf:type, amalgame:'Sample', Name),
	rdf_assert(Name, amalgame:sampleSize, literal(type(xsd:int, Size)), Name),
	rdf_assert(Name, amalgame:sourceGraph, Graph, Name),
	rdf_assert(Name, amalgame:method, literal(Method), Name),
	findall(Map, has_map(Map, _, Graph), Maps),
	length(Maps, Length),

	randset(Size, Length, RandSet),
	assert_from_list(Method, Name, Graph, 1, RandSet, Maps).

spyme.

assert_from_list(_,_,_,_,[], _).
assert_from_list(Method, Name, Graph, Nr, [Rand|RandSet], [[E1,E2]|Maps]) :-
	(   Rand = Nr
	->  has_map([E1,E2], _, Options, Graph),!,
	    spyme,
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
	    ->	findall(E1-E2a-MapOptions,
			has_map([E1,E2a], _, MapOptions, _),
			AltSourceMaps),
		findall(E1a-E2-MapOptions,
			has_map([E1a,E2], _, MapOptions, _),
			AltTargetMaps),
		append(AltSourceMaps, AltTargetMaps, AltMaps)
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
	;   assert_cell(E1,E2, [graph(Graph)|Options])
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

%%	style(-Style) is det.
%
%	Common style for HTML pages generated by this module
%
style(style(
	    [type('text/css')],
	    ['#aligntable { padding: .3%; border: solid grey} ',
	     '#nicktable  { padding: .3%; border: dashed grey; margin-left: 5% } ',
	     '#totals td  { border-top: solid grey; font-weight: bold } ',
	     '#finalrow td { border-top: solid #AAAAAA; } ',
	     '.lfloat { float: left; margin-right: 2% }'
	    ])
     ).


show_alignment_overview(Graph) -->
	{
	 align_ensure_stats(source(Graph)),
	 align_ensure_stats(target(Graph)),
	 align_ensure_stats(count(Graph)),
	 align_ensure_stats(mapped(Graph)),
	 align_ensure_stats(format(Graph)),

	 http_link_to_id(http_evaluator, [graph=Graph], EvalLink),
	 http_link_to_id(list_graph, [graph=Graph], GraphLink),
	 http_link_to_id(http_split_alignment,
			 [graph=Graph, condition=sourceType], STLink),
	 http_link_to_id(http_split_alignment,
			 [graph=Graph, condition=targetType], TTLink),
	 http_link_to_id(http_skos_export, [graph(Graph)], ExportLink),
	 http_link_to_id(http_sample_alignment, [graph(Graph)], SampleLink),

	 align_get_computed_props(Graph, Props),
	 memberchk(count(literal(type(_, Count))), Props),
	 memberchk(format(literal(Format)), Props),
	 memberchk(source(Source), Props),
	 memberchk(target(Target), Props),
	 memberchk(mappedSourceConcepts(literal(type(_,MSC))), Props),
	 memberchk(mappedTargetConcepts(literal(type(_,MTC))), Props),

	 (   Format == skos
	 ->  SkosExportLink = ''
	 ;   SkosExportLink = li(a([href(ExportLink)], 'Export to SKOS'))
	 )
	},
	html([p(['Alignment graph: ', a([href(Graph)], Graph)]),
	      p('Key alignment statistics: '),
	      table([id(aligntable)],[
		     tr([td('format:'),
			 td(Format),
			 td('Total mappings: '),
			 td([style('text-align:right')],[Count])
			]),
		     tr([td('Source voc:'),
			 td(\rdf_link(Source)),
			 td('Source concepts mapped:'),
			 td([style('text-align:right')],[MSC])
			]),
		     tr([td('Target voc:'),
			 td(\rdf_link(Target)),
			 td('Target concepts mapped:'),
			 td([style('text-align:right')],[MTC])
			])
		    ]
		   ),
	      p('Actions: '),
	      ul([
		  li(a([href(GraphLink)], 'View/download graph')),
		  SkosExportLink,
		  li(a([href(EvalLink)], 'Evaluate all mappings in graph')),
		  li(form([action(SampleLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([type(submit), value('Create')],[]),
		      ' sample of size N=',
		      input([type(text), name(size), value(25), size(4)],[]),
		      ' in named graph ',
		      input([type(text), name(name), value(sample)], []),
		      ' with method ',
		      select([name(method)],
			    [option([selected(selected), value(randommaps)],['N random mappings']),
			     option([value(random_alt_in_graph)],['N random mapped concepts, with alternative mappings in same graph']),
			     option([value(random_alt_all)], ['N random mapped concepts, with alternative mappings from all loaded graphs'])
			    ])
		     ])
		    ),
		  li(a([href(STLink)], 'Split on source type')),
		  li(a([href(TTLink)], 'Split on target type'))
		 ])
	     ]
	    ).

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
	html(tr([id(finalrow)],
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
		     rdf(M, amalgame:nickname, literal(Nick), amalgame_nicknames)
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
		     \+ rdfs_individual_of(Graph, amalgame:'Overlap')
		 ),
		 AllGraphs),
	 sort(AllGraphs, Graphs),
	 http_link_to_id(http_clear_alignstats, [], CacheLink),
	 http_link_to_id(http_compute_stats, [graph(all)], ComputeLink),
	 http_link_to_id(http_delete_alignment_graphs, [], ClearAlignLink),
	 Note = ['These are cached results, ',
		 a([href(CacheLink)], 'clear cache'), ', ',
		 a([href(ComputeLink)], 'compute all'), ' missing statistics, or ',
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
	html(tr([id(finalrow)],
		[td(''),
		 td(''),
		 td(''),
		 td(''),
		 td(''),
		 td(''),
		 td([style('text-align: right')],Total),
		 td('Total (double counting)')
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
		     tr([th('Overlap'),th('# maps'), th('Example')]),
		     \show_countlist(CountList,0)
		    ]
		  )
	     ]).

