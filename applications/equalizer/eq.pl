:- module(eq, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(library(http/http_session)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(start_page).
:- use_module(controls).
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

%%	http_equalizer(+Request)
%
%	HTTP handler for web page for interactive vocabulary alignment.

http_equalizer(Request) :-
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri, optional(true),
				     description('URI of an alignment')]),
			  mapping(Mapping,
				  [uri, optional(true),
				   description('URI of a mapping')]),
			  source(Source,
				 [uri, optional(true),
				  description('URI of a source vocabulary')]),
			  target(Target,
				 [uri, optional(true),
				  description('URI of a target vocabulary')])
		     ]),
	(   nonvar(Alignment)
	->  html_page(Alignment, @null)
	;   nonvar(Mapping)
	->  new_alignment(Mapping, Alignment),
	    html_page(Alignment, Mapping)
	;   nonvar(Source),
	    nonvar(Target)
	->  new_alignment(Source, Target, Alignment),
	    html_page(Alignment, @null)
	;   html_start_page
	).

%%	new_alignment(+Mapping, -AlignmentURI)
%
%	Assert a new alignment graph.

new_alignment(Mapping, Alignment) :-
	http_session_data(new_alignment(Alignment, Mapping)),
	!.
new_alignment(Mapping, Alignment) :-
	rdf_bnode(Alignment),
	rdf_transaction((rdf_assert(Alignment, rdf:type, amalgame:'Alignment', Alignment),
			 rdf_assert(Mapping, rdf:type, amalgame:'Mapping', Alignment))).

new_alignment(Source, Target, Alignment) :-
	http_session_data(new_alignment(Alignment, Source, Target)),
	!.
new_alignment(Source, Target, Alignment) :-
	rdf_bnode(Alignment),
	rdf_transaction((rdf_assert(Alignment, rdf:type, amalgame:'Alignment', Alignment),
			 rdf_assert(Source, rdf:type, amalgame:'Source', Alignment),
			 rdf_assert(Target, rdf:type, amalgame:'Target', Alignment))),
	http_session_assert(new_alignment(Alignment, Source, Target)).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Alignment, +Mapping)
%
%	Emit html page with layout for the appliation.

html_page(Alignment, Mapping) :-
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
					      [div(id(controls),
						   \html_controls)])
					])),
				div(id(bottom),
				    div([id(mapping), class('yui3-g')],
					[ div([class('yui3-u'), id(mappingtable)],
					      []),
					  div([class('yui3-u'), id(correspondance)],
					      [])
					])),
 				script(type('text/javascript'),
				       [ \yui_script(Alignment, Mapping)
				       ])
			      ])
			]).


%%	yui_script(+Graph, +Mapping)
%
%	Emit YUI object.

yui_script(Alignment, Mapping) -->
	{ findall(K-V, js_path(K, V), Paths),
	  findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  js_alignment_nodes(Alignment, Nodes)
 	},
 	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Equalizer',
			 json([alignment(Alignment),
			       mapping(Mapping),
 			       paths(json(Paths)),
			       nodes(json(Nodes))
			      ]))
	     ]).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(opmgraph, Path) :-
	http_link_to_id(http_opmviz, [format(svg)], Path).
js_path(mapping, Path) :-
	http_location_by_id(http_data_mapping, Path).
js_path(addprocess, Path) :-
	http_location_by_id(http_add_process, Path).

%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(equalizer, json([fullpath(Path),
			   requires([node, event,anim,tabview,
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

%%	js_alignment_nodes(+Alignment, -Nodes)
%
%	Nodes contains all nodes in alignment with their OPM type.

js_alignment_nodes(Alignment, Nodes) :-
	findall(S, rdf(S,_,_,Alignment), Nodes0),
	sort(Nodes0, Nodes1),
 	maplist(node_type, Nodes1, Nodes).

node_type(R, R=Type) :-
	rdf(R, rdf:type, Class),
	(   rdf_equal(Class, amalgame:'Alignment')
	->  Type = alignment
	;   rdfs_subclass_of(Class, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_subclass_of(Class, opmv:'Process')
	->  Type = process
	),
	!.
node_type(R, R=vocab).


%%	http:convert_parameter(+Type, +In, -URI) is semidet.
%
%	HTTP parameter conversion for the following types:
%
%	    * uri
%	    This  conversion  accepts NS:Local and absolute URIs.

http:convert_parameter(uri, In, URI) :-
	(   sub_atom(In, B, _, A, :),
	    sub_atom(In, _, A, 0, Local),
	    xml_name(Local)
	->  sub_atom(In, 0, B, _, NS),
	    rdf_global_id(NS:Local, URI)
	;   is_absolute_url(In)
	->  URI = In
	).
