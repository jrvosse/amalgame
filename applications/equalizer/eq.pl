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
:- use_module(library(yui3)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(start_page).
:- use_module(applications(mappingview/mappingview)).
:- use_module(applications(opmviz/opmviz)).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(eq, Dir)).
:- asserta(user:file_search_path(css, eq(web/css))).
:- asserta(user:file_search_path(js, eq(web/js))).
:- asserta(user:file_search_path(icon, eq(web/icon))).

% http handlers for this applications

:- http_handler(amalgame(eq), http_equalizer, []).
:- http_handler(amalgame(eq/new), http_eq_new, []).
:- http_handler(amalgame(eq/mapping), http_eq_mapping, []).
:- http_handler(amalgame(eq/workflow), http_eq_workflow, []).


mapping_relations(['http://www.w3.org/2004/02/skos/core#exactMatch'-exact,
		   'http://www.w3.org/2004/02/skos/core#closeMatch'-close,
		   'http://www.w3.org/2004/02/skos/core#narrowMatch'-narrower,
		   'http://www.w3.org/2004/02/skos/core#broadMatch'-broader,
		   'http://www.w3.org/2004/02/skos/core#related'-related,
		   'http://purl.org/vocabularies/amalgame#unrelated'-unrelated,
		   'http://purl.org/vocabularies/amalgame#unsure'-unsure
		  ]).




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

%%	html_page(+Worklow, +Mapping)
%
%	Emit html page with layout for the appliation.

html_page(Workflow, Mapping) :-
  	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('eq.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
  			  div(class('yui3-skin-sam'),
			      [ div([id(main), class('yui3-g')],
				    [ div([class('yui3-u'), id(workflow)],
					  [ div(class(hd), []),
					    div(class(bd),
						[ \html_opmviz(Workflow),
						  \html_mapping_info
						])
					  ]),
				      div([class('yui3-u'), id(controls)],
					  div([class('yui3-accordion'), id('controls-acc')],
					      [\html_acc_item(actions, actions,
							      div(id(actions), [tbd])),
					       \html_acc_item(statistics, statistics,
							      div(id(statistics), [tbd])),
					       \html_acc_item(mappings, mappings,
							      \html_mapping_view(Mapping))
					      ]))
				    ]),
				script(type('text/javascript'),
				       [ \yui_script
				       ])
			      ])
			]).


html_mapping_info -->
	html(div(id(info),
		 [ div(class(relations),
		       [ div(class(hd), relations),
		         div([class(bd), id(relations)],
			    \html_relations)
		       ]),
		   div(class('yui3-g'),
		       [ div(class('yui3-u-1-2'),
			     [ div(class(hd), source),
			       div(class(bd),
				   div([id(sourceinfo), class('resource-info')], []))
			     ]),
			 div(class('yui3-u-1-2'),
			     [ div(class(hd), target),
			       div(class(bd),
				   div([id(targetinfo), class('resource-info')], []))
			     ])
		       ])
		 ])).

html_relations -->
	{ mapping_relations(Rs) },
	html([
	     \html_relation_radio(Rs),
	      div(class(comment),
		  [label('because: '),
		   input([name(comment), class(comment), autocomplete(off)])])
	     ]).

html_relation_radio([]) --> !.
html_relation_radio([URI-Label|Rs]) -->
	html([input([name(relation),
		     class(rcheck),
		     autocomplete(off),
		     value(URI),
		     type(radio)]),
	      span(Label)
	     ]),
	html_relation_radio(Rs).

%%	html_acc_item(+Id, +Label, +HTMLBody)
%
%	Emit html markup for a YUI3 accordion item.

html_acc_item(Id, Label, Body) -->
	html(div([class('yui3-accordion-item yui3-accordion-item-active'), id(Id)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:{}'), class('yui3-accordion-item-trigger')],
			   Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).

%%	yui_script(+Workflow, +Mapping)
%
%	Emit YUI object.

%%	yui_script
%
%	Emit YUI object.

yui_script -->
 	js_yui3([{modules:{gallery: 'gallery-2011.02.23-19-01'
  			  }}
		],
		[node,event,anim,
		 'gallery-node-accordion'
  		],
		[ 'Y.one("#controls-acc").plug(Y.Plugin.NodeAccordion,
					   {anim:false,
					    effect:Y.Easing.backIn,
					    minHeight:250
					   })'
  		]).

		 /*******************************
		 *	  data handlers		*
		 *******************************/
