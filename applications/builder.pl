:- module(ag_builder, []).

:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(yui3_beta)).
:- use_module(user(user_db)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/json_util)).
:- use_module(library(amalgame/expand_graph)).

:- use_module(components(amalgame/controls)).
:- use_module(components(amalgame/correspondence)).
:- use_module(components(amalgame/util)).

% we need http handlers of these APIs
:- use_module(api(strategy_viz)).
:- use_module(api(mapping)).
:- use_module(api(correspondence)).
:- use_module(api(hints)).
:- use_module(api(node_info)).
:- use_module(api(mappinglist)).
:- use_module(api(virtual_concepts)).
:- use_module(api(ag_process)).

% http handler for this application
:- http_handler(amalgame(app/build), http_ag_build, []).

ag:menu_item(200=http_ag_build, 'build').
ag:menu_item(900=Handler, Label) :-
	(   (logged_on(User, X), X \== User)
	->  fail
	;   Handler = cliopatria_openid:login_page,
	    Label = 'login'
	).
:- html_resource(css_ag_build,
		 [ virtual(true),
		   requires([ ag_build_core,
			      ag_build_extra
		   ])
		 ]).

:- html_resource(ag_build_core,
		 [ virtual(true),
		   requires([ css('builder.css'),
			      css('strategy_viz.css'),
			      css('infobox.css'),
			      css('controls.css'),
			      css('mappingtable.css'),
			      css('detail-overlay.css')
			    ])
		 ]).

:- html_resource(ag_build_extra,
		 [ virtual(true),
		   requires([css('skosbrowser.css'),
			     css('columnbrowser.css')
			    ])
		 ]).
:- multifile
	amalgame:prebuilder/1.

prebuilder_hook(Strategy) :-
	amalgame:prebuilder(Strategy), fail.
prebuilder_hook(_Strategy).

amalgame:prebuilder(Strategy) :-
	precalc_voc_stats(Strategy).

precalc_voc_stats(Strategy) :-
	% handy to know how many concepts etc are in each vocab,
	% both for the user as for the hints system etc.
	forall(rdf(Strategy, amalgame:includes, Vocab),
	       (   voc_property(Vocab, numberOfConcepts(_))
	       ->  ( setting(amalgame:precompute, true)
		   ->  precompute_node(Strategy, Vocab)
		   ;   true
		   )
	       ;   print_message(informational,
			     map(found, 'No SKOS Concepts for ', Vocab, 0))
	       )
	      ).


%%	http_ag_build(+Request)
%
%	HTTP handler for web page with interactive vocabulary alignment
%	strategy builder.

http_ag_build(Request) :-
	% authorized(write(default, _)),
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri,
				     description('URI of an alignment strategy')]),
			  focus(Focus,
				[uri,
				 description('URI of current focus node'),
				 default(Strategy)
				])
			]),
	prebuilder_hook(Strategy),
	html_page(Strategy, Focus).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Strategy)
%
%	Emit html page with layout for the strategy builder
%	application.

html_page(Strategy, Focus) :-
	reply_html_page(amalgame(app),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css_ag_build),
			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ \html_ag_header([
				     active(http_ag_build),
				     strategy(Strategy),
				     focus(Focus)]),
				div([class('yui3-g'), id(layout)],
				    [ div([class('yui3-u yui3-u-1-5'), id(controls)],
					  div(class(content),
					      \html_controls)),
				      div([class('yui3-u yui3-u-4-5'), id(main)],
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
				    [\html_correspondence_overlay([editmode(none)])
				    ])
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Strategy, Focus)
				 ])
			]).

%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Strategy, Focus) -->
	{ findall(K-V, js_path(K, V), Paths), dict_pairs(PathD, path, Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_focus_node(Strategy, Focus, FocusNode),
	  js_strategy_nodes(Strategy, Nodes),
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
			 json{strategy:Strategy,
			       paths:PathD,
			       nodes:Nodes,
			       selected:FocusNode,
			       readonly:Read_only
			     })
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
js_path(ag_evaluate, Path) :-
	http_location_by_id(http_ag_evaluate , Path).
js_path(mapping, Path) :-
	http_location_by_id(http_data_mapping, Path).
js_path(evaluate, Path) :-
	http_location_by_id(http_data_evaluate, Path).
js_path(cinfo, Path) :-
	http_location_by_id(http_correspondence, Path).
js_path(concepts, Path) :-
	http_location_by_id(http_virtual_concepts, Path).
js_path(mappinglist, Path) :-
	http_location_by_id(http_mapping_list, Path).
js_path(deep_voc_stats, Path) :-
	http_location_by_id(http_deep_voc_stats, Path).

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






		 /*******************************
		 *    skos browser hooks	*
		 *******************************/

cliopatria:concept_property(class, Concept, Graphs0, Class, Options) :-
	graph_mappings(Graphs0, Graphs),
	(   is_mapped(Concept, Graphs, Options)
	->  Class = mapped
	;   Class = unmapped
	).
cliopatria:concept_property(count, Concept, Graphs0, Count, Options) :-
	graph_mappings(Graphs0, Graphs),
	mapped_descendant_count(Concept, Graphs, Count, Options).


graph_mappings([Strategy], Graphs) :-
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy'),
	!,
	findall(Mapping, rdf(Mapping, rdf:type, amalgame:'Mapping', Strategy), Graphs).
graph_mappings(Graphs, Graphs).


mapped_descendant_count(Concept, Graphs, Count, Options) :-
	findall(C, skos_descendant_of(Concept, C), Descendants0),
	sort(Descendants0, Descendants),
	(   Descendants	= []
	->  Count = @null
	;   mapped_chk(Descendants, Graphs, Mapped, Options),
	    length(Descendants, Descendant_Count),
	    length(Mapped, Mapped_Count),
	    atomic_list_concat([Mapped_Count, '/', Descendant_Count], Count)
	).

mapped_chk([], _, [], _ ).
mapped_chk([C|T], Graphs, [C|Rest], Options) :-
	is_mapped(C, Graphs, Options),
	!,
	mapped_chk(T, Graphs, Rest, Options).
mapped_chk([_|T], Graphs, Rest, Options) :-
	mapped_chk(T, Graphs, Rest, Options).

is_mapped(Concept, Mappings, Options) :-
	option(strategy(Strategy), Options),
	member(Mapping, Mappings),
	(   is_mapped(Strategy, source, Concept, Mapping)
	->  true
	;   is_mapped(Strategy, target, Concept, Mapping)
	).

