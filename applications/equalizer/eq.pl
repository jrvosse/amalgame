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

:- use_module(start_page).
:- use_module(applications(mappingview/mappingview)).

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
	workflow_history(WorkflowURI, Workflow),
	html_page(Workflow, _).

http_eq_new(Request) :-
	http_parameters(Request,
		     [ source(Source,
			      [description('Source vocabulary')]),
		       target(Target,
			      [description('Target vocabulary')])
		     ]),
	new_workflow(Source, Target, Workflow),
	html_page(Workflow, _).

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
			[ \html_requires(css('equalizer.css')),
 			  div(id(header),
			      []),
			  div(id(main),
			      [ div(id(workflow), []),
				div(id(mappings), [])
			      ]),
 			  script(type('text/javascript'),
				 [ \yui_script(Workflow, Mapping)
				 ])
			]).

%%	yui_script(+Workflow, +Mapping)
%
%	Emit YUI object.

yui_script(_Workflow, _Mapping) -->
	js_yui3([
		],
		[node,event,widget,datasource],
		[
 		]).

		 /*******************************
		 *	  data handlers		*
		 *******************************/
