:- module(eq_analyser,
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
:- use_module(library(semweb/rdf_label)).

:- use_module(eq_util).

:- multifile
	eq:menu_item/2.

% temp commented out
% eq:menu_item(http_eq_analyse, 'analyse').

% http handlers for this applications

:- http_handler(amalgame(analyse), http_eq_analyse, []).

%%	http_equalizer(+Request)
%
%	Emit html page with for the alignment analyser.

http_eq_analyse(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri, optional(true),
				     description('URI of an alignment')]),
			   mapping(Mapping,
				  [uri, default(''),
				   description('URI of initially selected mapping')
				  ])
			]),
	html_page(Alignment, Mapping).

html_page(Alignment, Mapping) :-
	html_set_options([dialect(html)]),
	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('analyser.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css'),
			   \html_eq_header(http_eq_analyse, Alignment),
			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ div([id(main), class('yui3-g')],
				    [ div([class('yui3-u'), id(mappings)],
					  []),
				      div([class('yui3-u'), id(overlaps)],
					  [])
				    ])
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Alignment, Mapping)
				 ])
			]).


%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Alignment, Mapping) -->
	{ %findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_mappings(Alignment, Mappings)
	},
	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Analyser',
			 json([alignment(Alignment),
			      % paths(json(Paths)),
			       mappings(Mappings),
			       selected(Mapping)
			      ]))
	     ]).



%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(analyser, json([fullpath(Path),
			   requires([node,event,
				     mappinglist])
			  ])) :-
	http_absolute_location(js('analyser.js'), Path, []).
js_module(mappinglist, json([fullpath(Path),
			requires([node,event,widget,
				  history,querystring])
		       ])) :-
	http_absolute_location(js('mappinglist.js'), Path, []).

