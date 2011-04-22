:- module(eq_start_page,
	  [ html_start_page/0
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
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
:- use_module(components(label)).
:- use_module(applications(concept_finder/concept_finder)).
:- use_module(library(amalgame/alignment)).

%%	html_start_page
%
%	Emit html page to start a new or load an existing alignment
%	project.

html_start_page :-
	findall(C, rdf(C, rdf:type, skos:'ConceptScheme'), Cs),
 	findall(A-S, amalgame_alignment(A, S), Alignments),
	sort(Cs, ConceptSchemes),
   	reply_html_page(equalizer(start),
			[ title(['Amalgame - projects'])
 			],
			[ \html_requires(css('eq_start_page.css')),
 			  \html_requires(css('resize-core.css')),
			  \html_requires(css('resize-skin.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssgrids/grids-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
 			  div(class('yui-skin-sam yui3-skin-sam'),
			      [ div(id(header), []),
				div(id(main),
				    [ h1('AMALGAME'),
				      div([id(content), class('yui3-accordion')],
					  [ \html_new(ConceptSchemes),
 					    \html_open(Alignments),
					    \html_import
					  ])
				    ]),
				script(type('text/javascript'),
				       [ \yui_script
				       ])
			      ])
			]).



%%	html_new
%
%

html_new(Schemes) -->
	html_acc_item(new, 'new alignment project',
		      [ form(action(location_by_id(http_equalizer)),
			     [ \html_vocab_table(Schemes),
			       \html_submit('Start')
			     ])
		      ]).

html_vocab_table(Vs) -->
	html(table([thead(tr(\html_vocab_head)),
		    tbody(\html_vocab_rows(Vs))
		   ])).

html_vocab_head -->
	html([th([]),
 	      th(name),
	      th('# concepts (estimate)')
 	     ]).

html_vocab_rows([]) --> !.
html_vocab_rows([Scheme|Vs]) -->
	{ rdf_estimate_complexity(_, skos:inScheme, Scheme, Count)
	},
 	html(tr([td(input([type(checkbox), autocomplete(off), class(option),
			   name(scheme), value(Scheme)])),
 		 td(\html_graph_name(Scheme)),
		 td(class(count), Count)
  		])),
	html_vocab_rows(Vs).


%%	html_open(+Alignments)
%
%

html_open(Alignments) -->
	html_acc_item(open, 'open alignment',
		      [ form(action(location_by_id(http_equalizer)),
			     [ \html_alignment_table(Alignments),
 			       \html_submit('Start')
			     ])
		      ]).
html_alignment_table(Alignments) -->
	html(table([thead(tr(\html_alignment_head)),
		    tbody(\html_alignment_rows(Alignments))
		   ])).

html_alignment_head -->
	html([th([]),
	      th(name),
	      th(includes)
  	     ]).

html_alignment_rows([]) --> !.
html_alignment_rows([URI-Schemes|Gs]) -->
 	html(tr([td(input([type(radio), autocomplete(off), class(option), name(alignment), value(URI)])),
		 td(\html_graph_name(URI)),
		 td(\html_scheme_labels(Schemes))
 		])),
	html_alignment_rows(Gs).

html_scheme_labels([]) --> !.
html_scheme_labels([S|Ss]) -->
	html(div(\turtle_label(S))),
	html_scheme_labels(Ss).

html_graph_name(Graph) -->
	{ graph_label(Graph, Label)
	},
	html(Label).

graph_label(Graph, Label) :-
	rdf_label(Graph, Lit),
	literal_text(Lit, Label).
graph_label(Graph, Graph).


html_import -->
	html_acc_item(import, 'import alignment',
		      [ form(action(location_by_id(eq_upload_url)),
			     [ 'URL: ',
			       input([type(text), name(url), value('http://'),
				      autocomplete(off), size(50)
				     ]),
			       input([type(submit), value('Upload')])
			   ]),
			form([action(location_by_id(eq_upload_data)),
			      method('POST'),
			      enctype('multipart/form-data')
			     ],
			     [ 'File: ',
			       input([type(file), name(data),
				      size(50)%, autocomplete(off)
				     ]),
			       input([type(submit), value('Upload')])
			     ])
		      ]).


%%	html_submit(+Label)
%
%

html_submit(Label) -->
	html(div(class(controls),
		 [ input([type(submit), autocomplete(off), class(start),
			  disabled(true), value(Label)])
		 ])).


%%	html_acc_item(+Id, +Label, +HTMLBody)
%
%	Emit html markup for a YUI3 accordion item.

html_acc_item(Id, Label, Body) -->
	html(div([class('yui3-accordion-item'), id(Id)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:{}'), class('yui3-accordion-item-trigger')],
			   Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).

%%	html_alignment_table(+Graphs)
%
%	Emit HTML table with alignment graph properties.





%%	yui_script
%
%	Emit YUI object.

yui_script -->
	{ findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes)
 	},
 	yui3([json([modules(json(Modules))])
	     ],
	     Includes,
	     [ \yui3_new(eq, 'Y.EqualizerSelect', [])
 	     ]).


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module('equalizer-select', json([fullpath(Path),
				    requires([node,base,event,anim,
 					      'gallery-node-accordion'])
			  ])) :-
	http_absolute_location(js('equalizerselect.js'), Path, []).




:- http_handler(amalgame(load/url), eq_upload_url, []).
:- http_handler(amalgame(load/data), eq_upload_data, []).

%%	http_eq_load(Request)
%
%	Handler for alignment import

eq_upload_data(Request) :-
	http_parameters(Request,
			[ data(Data,
			       [ description('RDF data to be loaded')
			       ])
 			]),
	rdf_bnode(Graph),
 	atom_to_memory_file(Data, MemFile),
	setup_call_cleanup(open_memory_file(MemFile, read, Stream),
			   api_sesame:guess_format_and_load(Stream, [graph(Graph)]),
			   ( close(Stream),
			     free_memory_file(MemFile)
			   )),
	http_link_to_id(http_equalizer, [alignment(Graph)], Redirect),
	http_redirect(moved, Redirect, Request).

eq_upload_url(Request) :-
	http_parameters(Request,
			[ url(URL, [])
 			]),
	rdf_bnode(Graph),
	rdf_load(URL, [graph(Graph)]),
	http_link_to_id(http_equalizer, [alignment(Graph)], Redirect),
	http_redirect(moved, Redirect, Request).
