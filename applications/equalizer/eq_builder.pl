:- module(eq_builder, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(yui3_beta)).

:- use_module(controls).
:- use_module(process).
:- use_module(voc).
:- use_module(eq_util).
:- use_module(hints).

% We have to load the opmviz module somewhere:
:- use_module(opmviz).

:- multifile
	eq:menu_item/2.

% http handlers for this applications
:- http_handler(amalgame(build), http_eq_build, []).

eq:menu_item(200=http_eq_build, 'build').


backward_compatibilty_fixes(Strategy) :-
	fix_sec_inputs(Strategy),
	fix_arity_params(Strategy),
	fix_publish_ns(Strategy),
	precalc_voc_stats(Strategy).

precalc_voc_stats(Strategy) :-
	% handy to know how many concepts etc are in each vocab,
	% both for the user as for the hints system etc.
	forall(rdf(Strategy, amalgame:includes, Vocab),
	       concept_count(Vocab, Strategy, _)
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
	html_set_options([dialect(html)]),
	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('builder.css')),
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
						    div([id(opm)], []))),
					    div([id(bottom)],
						div(class(content),
						    [ div(id(mappingtable), [])
						]))
					  ])
				    ]),
				div(id(detail),
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
	yui3([json([modules(json(Modules))])
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

js_path(opmgraph, Path) :-
	http_link_to_id(http_opmviz, [format(svg)], Path).
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

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(builder, json([fullpath(Path),
			   requires([node,event,
				     'json-parse', 'overlay','resize',
				     'datasource-io','datasource-cache',
				     opmviz,controls,infobox,mapping
				    ])
			  ])) :-
	http_absolute_location(js('builder.js'), Path, []).
js_module(opmviz, json([fullpath(Path),
			requires([node,event,widget,
				  io,'querystring-stringify-simple'
				 ])
		       ])) :-
	http_absolute_location(js('opmviz.js'), Path, []).
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
					'gallery-paginator',
					datatable,'datatable-sort'])
			     ])) :-
	http_absolute_location(js('mappingtable.js'), Path, []).

fix_publish_ns(S) :-
% backward compatibility
	(   rdf(S, amalgame:publish_ns, _,S)
	->  true
	;   setting(eq_publisher:default_namespace, NS),
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

