:- module(eq_builder, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(user(user_db)).

:- use_module(controls).
:- use_module(process).
:- use_module(opmviz).
:- use_module(stats).
:- use_module(eq_util).

:- multifile
	eq:menu_item/2.

% http handlers for this applications
:- http_handler(amalgame(build), http_eq_build, []).

eq:menu_item(http_eq_build, 'build').

%%	http_eq_build(+Request)
%
%	HTTP handler for web page with interactive vocabulary alignment
%	builder.

http_eq_build(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri,
				     description('URI of an alignment')])
			]),
	html_page(Alignment).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Alignment)
%
%	Emit html page with layout for the alignment builder
%	application.

html_page(Alignment) :-
	html_set_options([dialect(html)]),
  	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('builder.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
  			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ \html_eq_header(http_eq_build, Alignment),
				div([id(main), class('yui3-g')],
				    [ div([class('yui3-u'), id(opm)],
					  []),
				      div([class('yui3-u'), id(right)],
					  [div(id(controls),
					       \html_controls)
					  ])
				    ])
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Alignment)
				 ])
			]).


%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Alignment) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_alignment_nodes(Alignment, Nodes)
 	},
 	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Builder',
			 json([alignment(Alignment),
  			       paths(json(Paths)),
			       nodes(json(Nodes))
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(opmgraph, Path) :-
	http_link_to_id(http_opmviz, [format(svg)], Path).
js_path(statistics, Path) :-
	http_location_by_id(http_eq_stats, Path).
js_path(addprocess, Path) :-
	http_location_by_id(http_add_process, Path).
js_path(updatelabel, Path) :-
	http_location_by_id(http_update_label, Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(builder, json([fullpath(Path),
			   requires([node,event,anim,
				     'json-parse',
				     'datasource-io','datasource-cache',
				     'querystring-stringify-simple',
 				     opmviz,controls,infobox])
			  ])) :-
	http_absolute_location(js('builder.js'), Path, []).
js_module(opmviz, json([fullpath(Path),
			requires([node,event,widget,io])
		       ])) :-
	http_absolute_location(js('opmviz.js'), Path, []).
js_module(infobox, json([fullpath(Path),
			 requires([node,event])
			])) :-
	http_absolute_location(js('infobox.js'), Path, []).
js_module(controls, json([fullpath(Path),
			  requires([node,event,
				    'gallery-node-accordion'])
			])) :-
	http_absolute_location(js('controls.js'), Path, []).
