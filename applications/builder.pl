:- module(eq_builder, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(yui3_beta)).

:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/ag_controls)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/map)).

:- use_module(api(hints)).
:- use_module(api(node_info)).
:- use_module(api(mappinglist)).
:- use_module(api(skos_concepts)).
:- use_module(api(ag_process)).

:- multifile
	eq:menu_item/2.

% http handlers for this applications
:- http_handler(amalgame(build), http_eq_build, []).

eq:menu_item(200=http_eq_build, 'build').


backward_compatibilty_fixes(Strategy) :-
	fix_opmv_ns(Strategy),
	fix_sec_inputs(Strategy),
	fix_arity_params(Strategy),
	fix_publish_ns(Strategy),
	precalc_voc_stats(Strategy).

precalc_voc_stats(Strategy) :-
	% handy to know how many concepts etc are in each vocab,
	% both for the user as for the hints system etc.
	forall(rdf(Strategy, amalgame:includes, Vocab),
	       (   concept_count(Vocab, Strategy, _)
	       ->  true
	       ;   print_message(informational,
			     map(found, 'No SKOS Concepts for ', Vocab, 0))
	       )
	      ).


%%	http_eq_build(+Request)
%
%	HTTP handler for web page with interactive vocabulary alignment
%	builder.

http_eq_build(Request) :-
	% authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri,
				     description('URI of an alignment')]),
			  focus(Focus,
				[uri,
				 description('URI of current focus node'),
				 default(Alignment)
				])
			]),
	backward_compatibilty_fixes(Alignment),
	html_page(Alignment, Focus).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Strategy)
%
%	Emit html page with layout for the strategy builder
%	application.

html_page(Strategy, Focus) :-
	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('builder.css')),
			  \html_requires(css('skosbrowser.css')),
			  \html_requires(css('columnbrowser.css')),
			  \yui3_combo(yui3,
				      ['cssreset/reset-min.css',
				       'cssgrids/grids-min.css',
				       'cssfonts/fonts-min.css'
				      ]),
			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ \html_eq_header([
				     active(http_eq_build),
				     strategy(Strategy),
				     focus(Focus)]),
				div([class('yui3-g'), id(layout)],
				    [ div([class('yui3-u'), id(controls)],
					  div(class(content),
					      \html_controls)),
				      div([class('yui3-u'), id(main)],
					  [ div(id(graph),
						div(class(content),
						    div([id(strategy_graph)], []))),
					    div([id(bottom)],
						div(class(content),
						    [ div(id(mappingtable), []),
						      div(id(vocabularybrowser), [])
						]))
					  ])
				    ]),
				div([id(detail),class('hidden')],
				   \html_overlay)
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Strategy, Focus)
				 ])
			]).


html_overlay -->
	html(form([div(class('yui3-widget-hd'),
		       'Correspondence details'
		      ),
		   div(class('yui3-widget-bd'),
		       [ div(class('buttons up'), \html_buttons),
			 div([class(concepts), id(concepts)], [])
		       ]),
		   div(class('yui3-widget-ft'),
		       [ div(class(controls),
			     [ div(class('buttons bottom'), \html_buttons),
			       div(class(options), \html_options)
			     ])
		       ])
		  ])).

html_options -->
	html([ 'include all correspondences with the same: ',
	       input([type(checkbox), id(allsources), autocomplete(off)]),
	       label(' source'),
	       input([type(checkbox), id(alltargets), autocomplete(off)]),
	       label(' target')
	     ]).

html_buttons -->
	html([ input([type(button), class(cancel), value(cancel)]),
	       input([type(button), class(submit), value(submit)]),
	       input([type(button), class(prev), value(prev)]),
	       input([type(button), class(next), value(next)])
	     ]).

%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Alignment, Focus) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_focus_node(Alignment, Focus, FocusNode),
	  js_alignment_nodes(Alignment, Nodes),
	  (   has_write_permission
	  ->  Read_only = false
	  ;   Read_only = true
	  )
	},
	yui3([json([
		modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Builder',
			 json([alignment(Alignment),
			       paths(json(Paths)),
			       nodes(json(Nodes)),
			       selected(json(FocusNode)),
			       readonly(Read_only)
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(strategyGraph, Path) :-
	http_link_to_id(http_strategy_viz, [format(svg)], Path).
js_path(addprocess, Path) :-
	http_location_by_id(http_add_process, Path).
js_path(updatenode, Path) :-
	http_location_by_id(http_update_node, Path).
js_path(deletenode, Path) :-
	http_location_by_id(http_delete_node, Path).
js_path(info, Path) :-
	http_location_by_id(http_node_info, Path).
js_path(hint, Path) :-
	http_location_by_id(http_json_hint, Path).
js_path(eq_evaluate, Path) :-
	http_location_by_id(http_eq_evaluate , Path).
js_path(mapping, Path) :-
	http_location_by_id(http_data_mapping, Path).
js_path(evaluate, Path) :-
	http_location_by_id(http_data_evaluate, Path).
js_path(cinfo, Path) :-
	http_location_by_id(http_correspondence, Path).
js_path(concepts, Path) :-
	http_location_by_id(http_concepts, Path).
js_path(mappinglist, Path) :-
	http_location_by_id(http_mapping_list, Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(builder, json([fullpath(Path),
			   requires([node,event,
				     'json-parse', 'overlay','resize',
				     'datasource-io','datasource-cache',
				     strategy_viz,controls,infobox,mapping
				    ])
			  ])) :-
	http_absolute_location(js('builder.js'), Path, []).
js_module(strategy_viz, json([fullpath(Path),
			requires([node,event,widget,
				  io,'querystring-stringify-simple'
				 ])
		       ])) :-
	http_absolute_location(js('strategy_viz.js'), Path, []).
js_module(infobox, json([fullpath(Path),
			 requires([node,event,
				   io, 'querystring-stringify-simple'
				  ])
			])) :-
	http_absolute_location(js('infobox.js'), Path, []).
js_module(controls, json([fullpath(Path),
			  requires([node,event,anim,
				    'gallery-node-accordion'])
			])) :-
	http_absolute_location(js('controls.js'), Path, []).
js_module(mapping, json([fullpath(Path),
			requires([node,event,
				  mappingtable
				 ])
		       ])) :-
	http_absolute_location(js('mapping.js'), Path, []).
js_module(mappingtable, json([fullpath(Path),
			      requires([node,event,
					'datasource-jsonschema',
					'model-list', model,
					'gallery-paginator',
					datatable,'datatable-sort'])
			     ])) :-
	http_absolute_location(js('mappingtable.js'), Path, []).
js_module(vocabulary, json([fullpath(Path),
			    requires([node,event,'json-parse',
				      columnbrowser
				     ])
			   ])) :-
	http_absolute_location(js('vocabulary.js'), Path, []).
js_module(resourcelist, json([fullpath(Path),
			      requires([node,event,widget])
			     ])) :-
    http_absolute_location(js('resourcelist.js'), Path, []).
js_module(columnbrowser, json([fullpath(Path),
			     requires([node,event,widget,resourcelist])
			    ])) :-
    http_absolute_location(js('columnbrowser.js'), Path, []).


fix_publish_ns(S) :-
% backward compatibility
	(   rdf(S, amalgame:publish_ns, _,S)
	->  true
	;   setting(amalgame:default_publish_namespace, NS),
	    rdf_assert(S, amalgame:publish_ns, NS, S)
	).

fix_sec_inputs(Strategy) :-
% backward compatibility
	findall(rdf(S,RP,O),
		(   rdf_has(S,amalgame:secondary_input, O, RP),
		    rdf(S, RP, O, Strategy)
		), Triples),
	forall(member(rdf(S,P,O), Triples),
	       (   rdf_retractall(S,P,O,Strategy),
		   rdf_assert(S,amalgame:secondary_input, O, Strategy)
	       )
	      ).
fix_opmv_ns(Strategy) :- % backward compatibility
	OldProp = 'http://purl.org/net/opmv/ns#wasGeneratedBy',
	findall(rdf(S,OldProp,O),
		rdf(S, OldProp, O, Strategy),
		Triples),
	forall(member(rdf(S,P,O), Triples),
	       (   rdf_retractall(S,P,O,Strategy),
		   rdf_assert(S,amalgame:wasGeneratedBy, O, Strategy)
	       )
	      ).

fix_arity_params(Strategy) :-
% backward compatibility
	rdf_equal(amalgame:parameters, ParamProp),
	findall(rdf(S,ParamProp,O),
		(   rdf(S,ParamProp, literal(O), Strategy),
		    rdfs_individual_of(S, amalgame:'AritySelect')
		), ToBeFixed),
	forall(member(rdf(S,P,O), ToBeFixed),
	       (   rdf_retractall(S,P,literal(O),Strategy),
		   arity_param_convert(O,NewO),
		   rdf_assert(S,P,literal(NewO), Strategy)
	       )
	      ).
arity_param_convert('type=11', 'type=both'):- !.
arity_param_convert('type=1N', 'type=target'):- !.
arity_param_convert('type=N1', 'type=source'):- !.
arity_param_convert(X,X):- !.


		 /*******************************
		 *    skos browser hooks	*
		 *******************************/

cliopatria:concept_property(class, Concept, Graphs0, Class) :-
	graph_mappings(Graphs0, Graphs),
	(   is_mapped(Concept, Graphs)
	->  Class = mapped
	;   Class = unmapped
	).
cliopatria:concept_property(count, Concept, Graphs0, Count) :-
	graph_mappings(Graphs0, Graphs),
	mapped_descendant_count(Concept, Graphs, Count).


graph_mappings([Alignment], Graphs) :-
	rdf(Alignment, rdf:type, amalgame:'AlignmentStrategy'),
	!,
	findall(Mapping, rdf(Mapping, rdf:type, amalgame:'Mapping', Alignment), Graphs).
graph_mappings(Graphs, Graphs).


mapped_descendant_count(Concept, Graphs, Count) :-
	findall(C, descendant_of(Concept, C), Descendants0),
	sort(Descendants0, Descendants),
	(   Descendants	= []
	->  Count = @null
	;   mapped_chk(Descendants, Graphs, Mapped),
	    length(Descendants, Descendant_Count),
	    length(Mapped, Mapped_Count),
	    atomic_list_concat([Mapped_Count, '/', Descendant_Count], Count)
	).

descendant_of(Concept, D) :-
	rdf_reachable(D, skos:broader, Concept),
	\+ D = Concept.
descendant_of(Concept, D) :-
	rdf_reachable(Concept, skos:narrower, D),
	\+ D = Concept.


mapped_chk([], _, []).
mapped_chk([C|T], Graphs, [C|Rest]) :-
	is_mapped(C, Graphs),
	!,
	mapped_chk(T, Graphs, Rest).
mapped_chk([_|T], Graphs, Rest) :-
	mapped_chk(T, Graphs, Rest).

is_mapped(C, Graphs) :-
	has_correspondence(align(C,_,_), Graph),
	memberchk(Graph, Graphs),
	!.
is_mapped(C, Graphs) :-
	has_correspondence(align(_,C,_), Graph),
	memberchk(Graph, Graphs),
	!.
