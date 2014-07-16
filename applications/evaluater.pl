:- module(ag_evaluater,
	  []).

:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(yui3_beta)).
:- use_module(library(amalgame/util)).

:- use_module(user(user_db)).

:- use_module(api(node_info)).
:- use_module(api(mapping)). % need http_data_mapping handler

:- public amalgame_module/1.

amalgame_module(amalgame:'EvaluationProcess').

:- multifile
	ag:menu_item/2.

ag:menu_item(210=http_ag_evaluate, 'evaluate').

% http handlers for this applications

:- http_handler(amalgame(evaluate), http_ag_evaluate, []).

%%	http_ag_evaluate(+Request)
%
%	Emit html page with for the alignment analyser.

http_ag_evaluate(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri,
				     description('URI of an alignment')]),
			  focus(Mapping,
				  [uri, default(''),
				   description('URI of initially selected mapping')
				  ])
			]),
	(   rdfs_individual_of(Mapping, amalgame:'Mapping')
	->  SelectedMapping = Mapping
	;   rdfs_individual_of(SelectedMapping, amalgame:'Mapping')
	),
	html_page(Alignment, SelectedMapping).

html_page(Alignment, Mapping) :-
	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires(css('evaluater.css')),
			  \html_requires(css('gallery-paginator.css')),
			  \yui3_combo(yui3,
				      ['cssreset/reset-min.css',
				       'cssgrids/grids-min.css',
				       'cssfonts/fonts-min.css'
				      ]),
			  \html_ag_header([active(http_ag_evaluate),
					   strategy(Alignment),
					   focus(Mapping)
					  ]),
			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ div([id(content), class('yui3-g')],
				    [ div([class('yui3-u'), id(left)],
					  \html_sidebar),
				      div([class('yui3-u'), id(main)],
					  div([id(mappingtable)], []))
				    ]),
				div(id(detail),
				   \html_overlay)
			      ]),
			  script(type('text/javascript'),
				 [ \yui_script(Alignment, Mapping)
				 ])
			]).

html_sidebar -->
	html([ div(class(box),
		   [div(class(hd), 'Mappings'),
		    div([id(mappinglist), class(bd)], [])
		   ]),
	       div(class(box),
		   [div(class(hd), 'Info'),
		    div([id(mappinginfo), class(bd)], [])
		   ])
	     ]).

html_overlay -->
	html(form([div(class('yui3-widget-bd'),
		       [ div(class(buttons), \html_buttons),
			 div([class(concepts), id(concepts)], [])
		       ]),
		   div(class('yui3-widget-ft'),
		       [ div(class(controls),
			     [ div(class(buttons), \html_buttons),
			       div(class(options), \html_options)
			     ])
		       ])
		  ])).

html_options -->
	html([ 'include all correspondences with the same: ',
	       input([type(checkbox), id(allsources), checked, autocomplete(off)]),
	       label(' source'),
	       input([type(checkbox), id(alltargets), checked, autocomplete(off)]),
	       label(' target')
	     ]).

html_buttons -->
	html([
	       button([type(button), class(setall)], 'apply to all'),
	       button([type(button), class(cancel)], cancel),
	       button([type(button), class(submit)], submit),
	       button([type(button), class(prev)], prev),
	       button([type(button), class(next)], next)
	     ]).


%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Alignment, Mapping) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_mappings(Alignment, Mappings)
	},
	yui3([json([
		gallery('gallery-2011.02.23-19-01'),
		modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Evaluater',
			 json([alignment(Alignment),
			       paths(json(Paths)),
			       mappings(Mappings),
			       selected(Mapping)
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(mapping, Path) :-
	http_location_by_id(http_data_mapping, Path).
js_path(evaluate, Path) :-
	http_location_by_id(http_data_evaluate, Path).
js_path(mappinginfo, Path) :-
	http_location_by_id(http_node_info, Path).
js_path(info, Path) :-
	http_location_by_id(http_correspondence, Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(evaluater, json([fullpath(Path),
			   requires([node,event,anim,
				     'overlay','json-parse','io-base',
				     'datasource-io','datasource-jsonschema',
				     'querystring-stringify-simple',
				     mappinglist,mappingtable])
			  ])) :-
	http_absolute_location(js('evaluater.js'), Path, []).
js_module(mappinglist, json([fullpath(Path),
			requires([node,event,widget,
				  history,querystring])
		       ])) :-
	http_absolute_location(js('mappinglist.js'), Path, []).
js_module(mappingtable, json([fullpath(Path),
			      requires([node,event,
					'gallery-paginator',
					datatable,'datatable-sort'])
			     ])) :-
	http_absolute_location(js('mappingtable.js'), Path, []).
