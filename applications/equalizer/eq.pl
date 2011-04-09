:- module(eq, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(ag_util)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/alignment_graph)).

:- use_module(components(label)).

:- use_module(start_page).
:- use_module(opmviz).
:- use_module(mapping).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(eq, Dir)).
:- asserta(user:file_search_path(css, eq(web/css))).
:- asserta(user:file_search_path(js, eq(web/js))).
:- asserta(user:file_search_path(icon, eq(web/icon))).

:- setting(rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers for this applications

:- http_handler(amalgame(eq), http_equalizer, []).
:- http_handler(amalgame(eq/new), http_eq_new, []).
:- http_handler(amalgame(eq/mapping), http_eq_mapping, []).
:- http_handler(amalgame(eq/workflow), http_eq_workflow, []).

%%	http_equalizer(+Request)
%
%	HTTP handler for web page for interactive vocabulary alignment.

http_equalizer(_Request) :-
	html_start_page.

http_eq_mapping(Request) :-
	http_parameters(Request,
		     [ uri(MappingURI,
			   [description('URI of a mapping graph')])
		     ]),
	Workflow = [MappingURI],
	html_page(Workflow, MappingURI).

http_eq_workflow(Request) :-
	http_parameters(Request,
		     [ uri(WorkflowURI,
			   [description('URI of an alignment workflow')])
		     ]),
 	html_page(WorkflowURI, @null).

http_eq_new(Request) :-
	http_parameters(Request,
		     [ source(Source,
			      [description('Source vocabulary')]),
		       target(Target,
			      [description('Target vocabulary')])
		     ]),
	new_workflow(Source, Target, Workflow),
	html_page(Workflow, @null).

new_workflow(_, _, test).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Graph, +Mapping)
%
%	Emit html page with layout for the appliation.

html_page(Graph, Mapping) :-
	html_set_options([dialect(html)]),
  	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
			  \html_requires(css('gallery-paginator.css')),
			  \html_requires(css('resize-core.css')),
			  \html_requires(css('resize-skin.css')),
  			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ div(id(top),
				    div([id(alignment), class('yui3-g')],
					[ div([class('yui3-u'), id(opm)],
					      []),
					  div([class('yui3-u'), id(right)],
					      [\html_controls])
					])),
				div(id(bottom),
				    div([id(mapping), class('yui3-g')],
					[ div([class('yui3-u'), id(mappingtable)],
					      []),
					  div([class('yui3-u'), id(correspondance)],
					      [])
					])),
 				script(type('text/javascript'),
				       [ \yui_script(Graph, Mapping)
				       ])
			      ])
			]).

html_controls -->
	html(div(id(controls),
		 [div([id(info), class('yui3-accordion')],
		      [ \html_acc_item(infobox, 'Info', [])
		      ]),
		  div([id(mappingcontrols), class('controlset hidden yui3-accordion')],
		      [\html_acc_item(filter, 'Filter', [filter]),
		       \html_acc_item(select, 'Select', [select]),
		       \html_acc_item(merge, 'Merge', [merge])
		      ]),
		  div([id(processcontrols), class('controlset hidden yui3-accordion')],
		      []),
		  div([id(vocabcontrols), class('controlset hidden yui3-accordion')],
		      [\html_acc_item(align, 'Align', \html_align_controls)
		      ])
		 ])).

html_acc_item(Id, Label, Body) -->
	html(div([id(Id), class('yui3-accordion-item')],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:void(0)'),
			  class('yui3-accordion-item-trigger')],
			 Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).

html_align_controls -->
	html([]).


%%	yui_script(+Graph, +Mapping)
%
%	Emit YUI object.

yui_script(Graph, Mapping) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes)
 	},
 	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Equalizer',
			 json([graph(Graph),
			       mapping(Mapping),
 			       paths(json(Paths))
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(opmgraph, Path) :-
	http_link_to_id(http_opmviz, [format(svg)], Path).
js_path(mapping, Path) :-
	http_link_to_id(http_data_mapping, [], Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(equalizer, json([fullpath(Path),
			   requires([node, event,anim,
				     'datasource-io','datasource-jsonschema','datasource-cache',
				     'querystring-stringify-simple',
				     'gallery-resize','gallery-node-accordion',
				     opmviz,infobox,mappingtable])
			  ])) :-
	http_absolute_location(js('equalizer.js'), Path, []).
js_module(opmviz, json([fullpath(Path),
			requires([node,event,widget,io])
		       ])) :-
	http_absolute_location(js('opmviz.js'), Path, []).
js_module(infobox, json([fullpath(Path),
			 requires([node,event])
			])) :-
	http_absolute_location(js('infobox.js'), Path, []).
js_module(mappingtable, json([fullpath(Path),
			      requires([node,event,
					'gallery-paginator',
					datatable,'datatable-sort'])
			     ])) :-
	http_absolute_location(js('mappingtable.js'), Path, []).




