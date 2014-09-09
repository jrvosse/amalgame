:- module(ag_start_page,
	  [html_schemes_only//0  % for backward compat with europeana demo
	  ]).


:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- use_module(library(yui3_beta)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(applications(skos_browser)).

:- use_module(components(amalgame/startpage)).
:- use_module(api(form/amalgame/startpage)).

% main http handler for amalgame:
:- http_handler(amalgame(.), http_amalgame_main_page, []).

% Backward compatibility
:- http_handler(amalgame(eq),	    http_redirect(moved, amalgame(.)), []).
:- http_handler(amalgame(app/main), http_redirect(moved, amalgame(.)), []).

%%      http_amalgame_main_page(+Request) is det.
%
%	Emit html page to start a new or select/upload an existing
%	alignment strategy.

http_amalgame_main_page(Request) :-
	html_main_page(Request).

html_schemes_only -->
	{
	 amalgame_alignable_schemes(ConceptSchemes)
	},
	html_new(ConceptSchemes).

html_main_page(_Request) :-
	findall(A-S, strategy_vocabularies(A, S), StrategySchemePairs),
	amalgame_alignable_schemes(ConceptSchemes),
	reply_html_page(cliopatria(main),
			[ title(['Amalgame - strategies'])
			],
			[ \html_requires(css('startpage.css')),
			  \yui3_combo(yui3,
				      ['cssreset/cssreset-min.css',
				       'cssgrids/cssgrids-min.css',
				       'cssfonts/cssfonts-min.css'
				      ]),
			  div(class('yui-skin-sam yui3-skin-sam'),
			      [ div(id(header), []),
				div(id(main),
				    [
					div([id(content), class('yui3-accordion')],
					    [
						\html_open(StrategySchemePairs),
						\html_reference,
						\html_import,
						\html_publish(StrategySchemePairs),
						\html_new(ConceptSchemes)
					    ])
				    ]),
				script(type('text/javascript'),
				       [ \yui_script
				       ])
			      ])
			]).


%%	yui_script
%
%	Emit YUI object.

yui_script -->
	{ findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes)
	},
	yui3([json([
		gallery('gallery-2012.09.12-20-02'),
		modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.AmalgameStartPage', [])
	     ]).


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(startpage, json([fullpath(Path),
				    requires([node,base,event,anim,
					      'gallery-node-accordion'])
			  ])) :-
	http_absolute_location(js('startpage.js'), Path, []).








