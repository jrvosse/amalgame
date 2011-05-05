:- module(eq_evaluater,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(user(user_db)).

:- use_module(mapping).
:- use_module(stats).
:- use_module(eq_util).

:- multifile
	eq:menu_item/2.

eq:menu_item(http_eq_evaluate, 'evaluate').

:- setting(rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers for this applications

:- http_handler(amalgame(evaluate), http_eq_evaluate, []).

%%	http_eq_evaluate(+Request)
%
%	Emit html page with for the alignment analyser.

http_eq_evaluate(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri, optional(true),
				     description('URI of an alignment')])
			]),
	html_page(Alignment).

html_page(Alignment) :-
	html_set_options([dialect(html)]),
  	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('evaluater.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
			  \html_requires(css('gallery-paginator.css')),
			   \html_eq_header(http_eq_evaluate, Alignment),
   			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ div([id(content), class('yui3-g')],
				    [ div([class('yui3-u'), id(left)],
					  div(id(mappinglist), [])),
				      div([class('yui3-u'), id(main)],
					  div([id(mappingtable)], []))
				    ]),
				div(id(detail),
				   \html_overlay)
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Alignment)
				 ])
			]).

html_overlay -->
 	html(form([div(class('yui3-widget-hd'),
		      [  'Correspondance'
 		      ]),
		   div(class('yui3-widget-bd'),
		       [div(class(controls),
			    [ 'include all correspondences with the same:',
			      input([type(checkbox), id(msources), autocomplete(off)]),
			      label(source),
			      input([type(checkbox), id(msources), autocomplete(off)]),
			      label(target)
			    ]),
			div([class(concepts), id(concepts)], [])
		       ]),
		       div(class('yui3-widget-ft'),
			   [ div(class(controls), \html_controls)
			   ])
	     ])).

html_controls -->
	html([ input([type(button), value(prev)]),
	       input([type(submit), value(next)])
	     ]).


%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Alignment) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_mappings(Alignment, Mappings),
	  findall(json([uri=R,label=L]),
		  mapping_relation(L,R),
		  Relations)
 	},
 	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Evaluater',
			 json([alignment(Alignment),
  			       paths(json(Paths)),
			       mappings(Mappings),
			       relations(Relations)
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(statistics, Path) :-
	http_location_by_id(http_eq_info, Path).
js_path(mapping, Path) :-
	http_location_by_id(http_data_mapping, Path).
%js_path(info, Path) :-
%	http_location_by_id(http_correspondence, Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(evaluater, json([fullpath(Path),
			   requires([node, event,anim,
				     'overlay','json-parse','io-base',
				     'datasource-io','datasource-jsonschema','datasource-cache',
				     'querystring-stringify-simple',
				     mappinglist,mappingtable])
			  ])) :-
	http_absolute_location(js('evaluater.js'), Path, []).
js_module(mappinglist, json([fullpath(Path),
			requires([node,event,widget])
		       ])) :-
	http_absolute_location(js('mappinglist.js'), Path, []).
js_module(mappingtable, json([fullpath(Path),
			      requires([node,event,
					'gallery-paginator',
					datatable,'datatable-sort'])
			     ])) :-
	http_absolute_location(js('mappingtable.js'), Path, []).
