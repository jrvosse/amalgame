:- module(align_stats, [
			show_alignment_overview//1
		       ]). % No exports, HTTP entry points only

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
:- use_module(components(graphviz)).
:- use_module(applications(browse)).

:- use_module(amalgame_apps(vocabularies/vocabularies)).
:- use_module(amalgame(compare/overlap)).
:- use_module(amalgame(mappings/alignment)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(amalgame(mappings/map)).
:- use_module(amalgame(mappings/opm)).

:- http_handler(amalgame(clear_alignments),   http_delete_alignment_graphs, []).
:- http_handler(amalgame(clear_alignstats),   http_clear_alignstats,    []).
:- http_handler(amalgame(compute_stats),      http_compute_stats,       []).
:- http_handler(amalgame(find_overlap),       http_list_overlap,        []).
:- http_handler(amalgame(compute_overlap),    http_compute_overlaps,    []).
:- http_handler(amalgame(list_alignment),     http_list_alignment,      []).
:- http_handler(amalgame(list_alignments),    http_list_alignments,     []).
:- http_handler(amalgame(split_alignment),    http_split_alignment,     []).
:- http_handler(amalgame(skos_export),        http_skos_export,         []).
:- http_handler(amalgame(sample_alignment),   http_sample_alignment,	[]).
:- http_handler(amalgame(select_from_graph),  http_select_from_graph,	[]).


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
			title('Amalgame: alignment manipulation'),
			\show_alignment_overview(Graph)
			).

http_split_alignment(Request) :-
	http_parameters(Request,
			[graph(Graph, []),
			 condition(Condition, [])
			]),
	split_alignment(Request, Graph, Condition, OutGraphs),
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
http_compute_overlaps(_Request) :-
	authorized(write(repository, clear)),
	http_link_to_id(http_list_overlap, [], Link),
	Title = 'Amalgame: computing overlap statistics',
	call_showing_messages(compute_overlaps,
			      [head(title(Title)),
			       header(h4(Title)),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['overlap overview']),
					   ' to inspect results.']))
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
	align_ensure_stats(found),
	findall(G, is_alignment_graph(G,_), Graphs),!,
	forall(member(G, Graphs), align_ensure_stats(all(G))).

%%	http_list_overlap(+Request) is det.
%
%	HTTP handler generating a page with mapping overlap statistics.

http_list_overlap(_Request) :-
	reply_html_page(cliopatria(default),
			[
			 title('Amalgame: alignment overlap')
			],
			[
			 div([class(alignlist)],
			     [
			      \show_alignments
			     ]),
			 div([class(overlaplist)],
			     [
			      h4('Alignment overlap'),
			      \show_overlap
			     ])
			]).

%%	http_clear_alignstats(?Request) is det.
%
%	Clears named graphs with cached amalgame results.

http_clear_alignstats(Request):-
	authorized(write(amalgame_cache, clear)),
	http_parameters(Request,
			[what(What, [oneof(all, overlaps), default(all)])
			]),

	Title = 'Amalgame: clearing caches',
	clear_actions(ClearActions),
	call_showing_messages(clear_alignstats(What),
			      [head(title(Title)),
			       header(h4(Title)),
			       footer(div([h4('Cleared all caches')|
					   ClearActions
					  ]))
			      ]).

clear_actions(ClearActions) :-
	http_link_to_id(http_compute_stats, [graph(all)], RecomputeLink),
	http_link_to_id(http_list_alignments, [graph(all)], ListLink),
	http_link_to_id(http_list_overlap, [], OverlapLink),
	ClearActions = [
			p('You now may want to proceed by:'),
			ul(
			   [
			    li(['returning to the ', a([href(ListLink)], 'alignment overview page')]),
			    li(['returning to the ', a([href(OverlapLink)], 'overlap page')]),
			    li(['recomputing ', a([href(RecomputeLink)],'all key stats')])
			   ])
		       ].




http_delete_alignment_graphs(Request) :-
	authorized(write(amalgame_cache, clear)),
	authorized(write(default, unload(_))),
	clear_actions(ClearActions),
	rdf_equal(amalgame:'Alignment', DefaultType),
	http_parameters(Request,
			[type(Type, [default(DefaultType), description('What type of graphs to delete, defaults to all amalgame:Alignment, deleting all alignments')])]),
	rdf_display_label(Type, TypeLabel),
	http_link_to_id(list_resource, [r(Type)], TypeLink),
	call_showing_messages(delete_alignment_graphs(Type),
			      [header(div([h4(['Amalgame: deleted graphs of type ', a([href(TypeLink)],TypeLabel)])])),
			       footer(div([h4('Graphs deleted')|ClearActions]))
			      ]).


http_skos_export(Request) :-
	http_parameters(Request, [graph(Graph, [description('URI of source graph to export from')]),
				  name(TargetGraph, [default(default_export_graph)]),
				  relation(MapRelation,
					   [default('http://www.w3.org/2004/02/skos/core#closeMatch')])
				 ]),

	(rdf_graph(TargetGraph) -> rdf_unload(TargetGraph); true),
	edoal_to_triples(Request, Graph, TargetGraph, [relation(MapRelation)]),
	http_link_to_id(http_list_alignment, [graph(TargetGraph)], ListGraph),
	http_redirect(moved, ListGraph, Request).


http_sample_alignment(Request) :-
	authorized(write(default, create(sample))),
	http_parameters(Request, [graph(Graph, [length > 0]),
				  size(Size, [nonneg]),
				  name(Name, [length > 0]),
				  method(Method, [])
				 ]),
	sample(Request, Method, Graph, Name, Size),
	http_link_to_id(http_list_alignment, [graph(Name)], ListGraph),
	http_redirect(moved, ListGraph, Request).

http_select_from_graph(Request) :-
	http_parameters(Request, [graph(Graph, [description('URI of source graph to export from')]),
				  target(TargetGraph, [default(default_export_graph)]),
				  condition(Condition, [description('Selection type'), oneof(confidence, one_to_one, unique_label)]),
				  min(Min, [default(0.0), between(0.0, 1.0), description('Minimal confidence level')]),
				  max(Max, [default(1.0), between(0.0, 1.0), description('Maximal confidence level')])
				 ]),

	(rdf_graph(TargetGraph) -> rdf_unload(TargetGraph); true),
	(   Condition = confidence
	->  edoal_select(Request, Graph, TargetGraph, [min(Min), max(Max)])
	;   select_from_alignment(Request, Graph, Condition, TargetGraph)
	),
	reply_html_page(cliopatria(default),
			title('Amalgame selection results'),
			\show_alignment_overview(TargetGraph)).

sample(Request, Method, Graph, Name, Size) :-
	(   rdf_graph(Name)
	->  rdf_unload(Name),
	    rdf_retractall(Name,_,_,amalgame)
	;   true
	),
	rdf_assert(Name, rdf:type, amalgame:'SampleAlignment', Name),

	http_current_host(Request, Hostname, Port, [global(true)]),
	memberchk(request_uri(ReqURI), Request),
	memberchk(protocol(Protocol), Request),
	format(atom(ReqUsed), '~w://~w:~w~w', [Protocol,Hostname,Port,ReqURI]),
	rdf_bnode(Process),
	rdf_assert(Process, rdfs:label, literal('Amalgame sample process'), Name),
	rdf_assert(Process, amalgame:request, literal(ReqUsed), Name),
	rdf_assert(Process, amalgame:sampleSize, literal(type(xsd:int, Size)), Name),
	rdf_assert(Process, amalgame:sampleMethod, literal(Method), Name),
	opm_was_generated_by(Process, Name, Name, [was_derived_from([Graph])]),

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
	    NewRandSet = RandSet
	;   NewRandSet = [Rand|RandSet]
	),
	NewNr is Nr + 1,
	assert_from_list(Method, Name, Graph, NewNr, NewRandSet, Maps).

assert_map_list([],_).
assert_map_list([H|T], Graph) :-
	H=E1-E2-Options,
	(   has_map([E1,E2], edoal, Graph)
	->  true
	;   assert_cell(E1,E2, [graph(Graph), alignment(Graph) | Options])
	),
	assert_map_list(T,Graph).

clear_alignstats(all) :-
	align_clear_stats(all),
	clear_overlaps.
clear_alignstats(overlaps) :-
	clear_overlaps.

delete_alignment_graphs(Type) :-
	align_ensure_stats(found),
	findall(Graph, is_alignment_graph(Graph, _Format), Graphs),
	forall((member(Graph, Graphs),
		rdfs_individual_of(Graph, Type)
	       ),
	       (
		   print_message(informational, map(cleared, graph, 1, Graph)),
		   rdf_unload(Graph)
	       )
	      ).

show_alignment_overview(Graph) -->
	{
	 align_ensure_stats(source(Graph)),
	 align_ensure_stats(target(Graph)),
	 align_ensure_stats(count(Graph)),
	 align_ensure_stats(mapped(Graph)),
	 align_ensure_stats(format(Graph)),
	 rdf_display_label(Graph, GraphLabel)
	},
	html([
	      h1([class(align_overview_header)], ['Alignment actions & details: ', GraphLabel]),
	      div([id(ag_graph_info)], \graph_info(Graph)),
	      div([class(graphviz), style('width: 80%')],
		  [\graphviz_graph(cliopatria:context_graph(Graph),
			       [ object_attributes([width('100%')]),
				 wrap_url(cpa_browse:resource_link),
				 graph_attributes([rankdir('RL'), fontsize('16.00'), fontname('Helvetica')]),
				 shape_hook(cpa_browse:shape(Graph))
			       ])
		  ]),
	      div([id(ag_graph_basic_actions)],
		   [
		    'Basic actions: ',
		    \graph_actions(Graph)
		   ]),
	      p('Create a new graph from this one: '),
	      ul([id(alignoperations)],
		  [
		   \li_eval_graph(Graph),
		   \li_sample_graph(Graph),
		   \li_select_from_graph(Graph),
		   \li_one_to_one(Graph),
		   \li_select_unique_labels(Graph),
		   \li_export_graph(Graph)
		  ]
		),
	      p('Create multiple new graphs from this one: '),
	      ul([id(alignoperations)],
		 [
		   \li_partition_graph(Graph)
		 ]
		),
	      div([id(ag_graph_as_resource)],
		  \graph_as_resource(Graph, []))

	     ]).

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

show_voc(Graph) -->
	{
	 http_link_to_id(http_list_skos_voc, [voc(Graph)], VLink)
	},
	html(a([href(VLink)],\turtle_label(Graph))).

show_countlist([], Total) -->
	html(tr([class(finalrow)],
		[td(''),
		 td([style('text-align: right')], Total),
		 td('Total (unique alignments)')
		])).

show_countlist([Count:Overlap|T], Number) -->
	{
	  NewNumber is Number + Count
	},
	html(tr([
		 td(\show_overlap_graphs(Overlap)),
		 td([style('text-align: right')],Count),
		 \show_example(Overlap)
		])),
	show_countlist(T,NewNumber).



show_example(Overlap) -->
	{
	 has_map([E1, E2], edoal, Overlap)
	},
	html([td(\rdf_link(E1, [resource_format(nslabel)])),
	      td(\rdf_link(E2, [resource_format(nslabel)]))
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
	 rdf_equal(amalgame:'DerivedAlignment', DerivedAlignments),
	 http_link_to_id(http_clear_alignstats, [what(all)], CacheLink),
	 http_link_to_id(http_delete_alignment_graphs, [], ClearAllAlignLink),
	 http_link_to_id(http_delete_alignment_graphs, [type(DerivedAlignments)], ClearDerivedLink),
	 http_link_to_id(http_compute_stats, [graph(all)], ComputeLink)
	},
	html([
	      table([class(aligntable)],
		    [tr([
			 th([class(nick)],'Abr'),
			 th([class(src)],'Source'),
			 th([class(src_mapped)],'# mapped'),
			 th([class(target)],'Target'),
			 th([class(target_mapped)],'# mapped'),
			 th([class(format)],'Format'),
			 th([class(count)],'# maps'),
			 th([class(graph)],'Named Graph URI')
			]),
		     \show_alignments(Graphs,0)
		    ]),
	      ul([class(ag_align_actions)],
		 [
		  li([a([href(ComputeLink)], 'Compute'), ' all missing statistics.']),
		  li([a([href(CacheLink)], 'Clear'), ' vocabulary statistics and overlap cache']),
		  li([a([href(ClearDerivedLink)], 'Delete derived'),  ' alignments from the repository']),
		  li([a([href(ClearAllAlignLink)], 'Delete all (!)'),  ' alignments from the repository'])
		  %\li_del_derived
		 ])
	     ]).

show_alignments([],Total) -->
	{
	 http_link_to_id(http_list_alignments, [], Link)
	},
	html(tr([class(finalrow)],
		[td([class(nick)],''),
		 td([class(src)],''),
		 td([class(src_mapped)],''),
		 td([class(target)],''),
		 td([class(target_mapped)],''),
		 td([class(format)],''),
		 td([class(count),style('text-align: right')],Total),
		 td([class(graph)], a(href(Link),'Total (double counting)'))
		])).

show_alignments([Graph|Tail], Number) -->
	{
	 findall(Label,
		 (   rdfs_individual_of(Graph, Class),
		     rdf_global_id(_NS:Label, Class)
		 ),
		 AlignmentTypes),
	 sort(AlignmentTypes, VocTypesUnique),
	 atomic_list_concat(VocTypesUnique, ' ', AlignmentTypesAtom),
	 http_link_to_id(http_compute_stats, [graph(Graph), stat(all)], MissingLink),
	 MissingValue = a([href(MissingLink)],'?'),
	 (   is_alignment_graph(Graph, Format) -> true; Format=empty),
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
	 ->  Source = \show_voc(SourceGraph)
	 ;   Source = MissingValue
	 ),
	 (   memberchk(target(TargetGraph), Props)
	 ->  Target = \show_voc(TargetGraph)
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
	html(tr([class(AlignmentTypesAtom)],
		[
		 td([class(nick)],\show_alignment(Graph)),
		 td([class(src)], Source),
		 td([class(src_mapped),style('text-align: right')],SourcesMapped),
		 td([class(target)], Target),
		 td([class(target_mapped), style('text-align: right')],TargetsMapped),
		 td([class(format)],FormatLink),
		 td([class(count),style('text-align: right')],Count),
		 td([class(graph)],div(\show_graph(Graph)))
		])),
	show_alignments(Tail, NewNumber).

show_overlap -->
	{
	 precomputed_overlaps(CountList),!,
	 http_link_to_id(http_clear_alignstats, [what(overlaps)], ClearCacheLink)
	},
	html([
	      table([id(aligntable)],
		    [
		     tr([th('Overlap'),th('# maps'), th([colspan(2)],'Example')]),
		     \show_countlist(CountList,0)
		    ]
		  ),
	      ul([class(ag_overlap_actions)],
		 [
		  % li([a([href(ComputeLink)], 'Compute'), ' all missing statistics.']),
		  li([a([href(ClearCacheLink)], 'Clear'), ' vocabulary statistics cache'])
		  %\li_del_derived
		 ])
	     ]).
show_overlap -->
	{
	 http_link_to_id(http_compute_overlaps, [], ComputeLink)
	},
	html([
	      p([class(no_overlaps_yet)],['No overlaps have been computed yet.']),
	      h4(class(ag_overlap_actions), 'Overlap actions'),
	      ul([class(ag_overlap_actions)],
		 [
		  li([a([href(ComputeLink)], 'Compute'), ' all alignment overlaps (might take a while).'])
		  %\li_del_derived
		 ])
	     ]).


li_export_graph(Graph) -->
	{
	 \+ is_alignment_graph(Graph, edoal)
	},!.
li_export_graph(Graph) -->
	{
	 http_link_to_id(http_skos_export, [], ExportLink),
	 % rdf_equal(skos:closeMatch, DefaultRelation),
	 Override=no_override,
	 supported_map_relations(MapRelations),
	 Base=export,
	 reset_gensym(Base),
	 repeat,
	 gensym(Base, Target),
	 \+ rdf_graph(Target),!
	},
	html(li(form([action(ExportLink)],
		      [input([type(submit), class(submit),
			      value('Export')
			     ],[]),
		       ' to graph ',
		       input([type(text), class(target),
			      name(name), value(Target),
			      size(10)],[]),
		       input([type(hidden),
			      name(graph),
			      value(Graph)],[]),
		       ' to export to a single triple format. Override provided map relation by: ',
		       select([name(relation)],
			      [\show_mapping_relations([Override|MapRelations], Override)])
		      ]
		     ))).

li_eval_graph(Graph) -->
	{
	 http_link_to_id(http_evaluator, [], EvalLink),
	 Base=evaluation,
	 reset_gensym(Base),
	 repeat,
	 gensym(Base, Target),
	 \+ rdf_graph(Target),!
	},
	html(li(form([action(EvalLink)],
		     [input([type(hidden),  name(graph), value(Graph)],[]),
		      input([class(submit), type(submit), value('Evaluate')]),
		      ' to graph ',
		      input([type(text), class(target), name(target), size(10),
			     value(Target)], [])
		     ]
		    ))).


li_sample_graph(Graph) -->
	{
	 http_link_to_id(http_sample_alignment, [], SampleLink),
	 Base=sample,
	 reset_gensym(Base),
	 repeat,
	 gensym(Base, Target),
	 \+ rdf_graph(Target),!
	},
	html(li(form([action(SampleLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([class(submit), type(submit), value('Sample')],[]),
		      ' to graph ',
		      input([type(text), name(name), value(Target), size(10), class(target)], []),
		      ' sample size N=',
		      input([type(text), name(size), value(25), size(4)],[]),
		      ' with method ',
		      select([name(method)],
			     [option([selected(selected), value(randommaps)],['N random mappings']),
			      option([value(random_alt_in_graph)],['idem, with alternatives in graph']),
			      option([value(random_alt_all)], ['idem, with alternatives from all graphs'])
			     ])
		     ])
	       )).


li_partition_graph(Graph) -->
	{
	 http_link_to_id(http_split_alignment, [], SplitLink)

	},
	html(li(form([action(SplitLink)],
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
		  ).

/*
	Selection on confidence level now only works on edoal graphs that have conf. level data:
*/

li_select_from_graph(Graph) -->
	{
	 \+ is_alignment_graph(Graph, edoal)
	},!.

li_select_from_graph(Graph) -->
	{
	 is_alignment_graph(Graph, edoal),
	 http_link_to_id(http_select_from_graph, [], SelectLink),
	 Base=select,
	 reset_gensym(Base),
	 repeat,
	 gensym(Base, Target),
	 \+ rdf_graph(Target),!
	},
	html(li(form([action(SelectLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([type(hidden), name(condition), value(confidence)],[]),
		      input([type(submit), class(submit), value('Select')],[]),
		      	       ' to graph ',
		       input([type(text), class(target),
			      name(target), value(Target),
			      size(10)],[]),
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
			     ],[])
		     ]
		    )
	       )
	    ).

li_one_to_one(Graph) -->
	{
	 http_link_to_id(http_select_from_graph, [], SelectLink),
	 atom_concat(Graph,'_1-1-only', Target)
	},

	html(li(form([action(SelectLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([type(hidden), name(condition), value(one_to_one)],[]),
		      input([type(submit), class(submit), value('Select')],[]),
		      	       ' to graph ',
		       input([type(text), class(target),
			      name(target), value(Target),
			      size(10)],[]),
		      ' only the one-to-one-mappings '
		     ]
		    )
	       )
	    ).
li_select_unique_labels(Graph) -->
	{
	 http_link_to_id(http_select_from_graph, [], SelectLink),
	 atom_concat(Graph,'_uniq_labels', Target)
	},

	html(li(form([action(SelectLink)],
		     [input([type(hidden), name(graph), value(Graph)],[]),
		      input([type(hidden), name(condition), value(unique_label)],[]),
		      input([type(submit), class(submit), value('Select')],[]),
		      	       ' to graph ',
		       input([type(text), class(target),
			      name(target), value(Target),
			      size(10)],[]),
		      ' only the unique label mappings '
		     ]
		    )
	       )
	    ).
