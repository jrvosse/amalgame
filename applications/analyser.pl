:- module(ag_analyser,
	  []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(yui3_beta)).

:- use_module(library(amalgame/irr)).
:- use_module(library(amalgame/util)).
:- use_module(components(amalgame/util)).

% temp commented out
% ag:menu_item(240=http_ag_analyse, 'analyse').

% http handlers for this applications

:- http_handler(amalgame(app/analyse), http_ag_analyse, []).
:- http_handler(amalgame(data/agreement), http_agreement, []).

%%	http_ag_analyse(+Request) is det
%
%	Emit html page with for the alignment analyser.

http_ag_analyse(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri, optional(true),
				     description('URI of an strategy')]),
			   mapping(Mapping,
				  [uri, default(''),
				   description('URI of initially selected mapping')
				  ])
			]),
	html_page(Strategy, Mapping).

html_page(Strategy, Mapping) :-
	reply_html_page(amalgame(app),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('analyser.css')),
			  \html_ag_header([active(http_ag_analyse),
					   strategy(Strategy),
					   focus(Mapping)
					  ]),
			  div(class('yui3-skin-sam yui-skin-sam'),
			      [ div([id(main), class('yui3-g')],
				    [
				     div([class('yui3-u'), id(mappings)],
					 [div([class(hd)],['Mappings'])]),
				     div([class('yui3-u'), id(agreement)],
					 [\agreement_table])
				    ]
				   )
			      ]
			     ),
			 script(type('text/javascript'),
				[ \yui_script(Strategy, Mapping)])
			]).

agreement_table -->
	html([div([class(hd)],['Agreement statistics']),
	      table([
		    tr([td('alpha'),	 td([id(alpha),   class(agreement_stat)],[?])]),
		    tr([td('# subjects'),td([id(subjects),class(agreement_stat)],[?])]),
		    tr([td('# raters'),  td([id(raters),  class(agreement_stat)],[?])])
		   ])]).

http_agreement(Request) :-
	Ein = encoding([skos:closeMatch-1, skos:exactMatch-1,
			skos:broadMatch-2, skos:narrowMatch-3,
			skos:relatedMatch-0,
			'http://purl.org/vocabularies/amalgame/evaluator#unrelated'-0,
			'http://purl.org/vocabularies/amalgame/evaluator#unsure'-'NA',
			default-4]),
	http_parameters(Request,
			[
			 mapping(Mapping,
				 [description('Reference graph')]),
			 strategy(Strategy,
				   [description('Alignment strategy graph')])
			]),
	findall(M, rdf(M, rdf:type, amalgame:'Mapping'), Mappings),
	(   Mapping == ''
	->  Agreement = none
	;   selectchk(Mapping, Mappings, OtherMappings),
	    alpha(Strategy, [Mapping|OtherMappings], Results0, [Ein]),
	    selectchk(encoding(Eout), Results0, Results),
	    Agreement=json([encoding(json(Eout))|Results])
	),
	reply_json(Agreement).

%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Strategy, Mapping) -->
	{
	 findall(M-C, js_module(M,C), Modules),
	 pairs_keys(Modules, Includes),
	 findall(K-V, js_path(K, V), Paths),
	 js_mappings_metadata(Strategy, JSMappings, [])
	},
	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.Analyser',
			 json([strategy(Strategy),
			       paths(json(Paths)),
			       mappings(JSMappings),
			       selected(Mapping)
			      ]))
	     ]).



%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(analyser, json([fullpath(Path),
			   requires([node,event,'json-parse', 'io-base',
				     'datasource-io','datasource-jsonschema',
				     mappinglist])
			  ])) :-
	http_absolute_location(js('analyser.js'), Path, []).
js_module(mappinglist, json([fullpath(Path),
			requires([node,event,widget,
				  history,querystring])
		       ])) :-
	http_absolute_location(js('mappinglist.js'), Path, []).

%%	js_path(+Key, +Server_Path)
%
%	Path to the server used in javascript.

js_path(agreement, Path) :-
	http_location_by_id(http_agreement, Path).
