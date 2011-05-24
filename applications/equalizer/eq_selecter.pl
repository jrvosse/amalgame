:- module(eq_selecter,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_file_type)).
:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(eq_util).

% http handlers for this applications
:- http_handler(amalgame(eq), http_eq, []).
:- http_handler(amalgame(new), http_eq_new, []).
:- http_handler(amalgame(load/url), http_eq_upload_url, []).
:- http_handler(amalgame(load/data), http_eq_upload_data, []).

%%	http_eq(+Request)
%
%	Emit html page to start a new or load an existing alignment
%	project.

http_eq(_Request) :-
	authorized(write(default, _)),
	html_page.

find_schemes(Schemes) :-
	findall(C, rdf(C, rdf:type, skos:'ConceptScheme'), Cs),
	findall(G, is_edm_collection(G), Gs),
	append(Cs, Gs, All),
	sort(All, Schemes).

html_page :-
	findall(A-S, amalgame_alignment(A, S), Alignments),
	find_schemes(ConceptSchemes),
	reply_html_page(equalizer(start),
			[ title(['Amalgame - projects'])
			],
			[ \html_requires(css('selecter.css')),
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
		      [ form(action(location_by_id(http_eq_new)),
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
		      [ form(action(location_by_id(http_eq_build)),
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
		      [ form(action(location_by_id(http_eq_upload_url)),
			     [ 'URL: ',
			       input([type(text), name(url), value('http://'),
				      autocomplete(off), size(50)
				     ]),
			       input([type(submit), value('Upload')])
			   ]),
			form([action(location_by_id(http_eq_upload_data)),
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
	     [ \yui3_new(eq, 'Y.Selecter', [])
	     ]).


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

js_module(gallery, 'gallery-2011.02.23-19-01').
js_module(selecter, json([fullpath(Path),
				    requires([node,base,event,anim,
					      'gallery-node-accordion'])
			  ])) :-
	http_absolute_location(js('selecter.js'), Path, []).


		 /*******************************
		 *	       upload	        *
		 *******************************/

%%	http_eq_new(+Request)
%
%	Handler to create a new alignment

http_eq_new(Request) :-
	http_parameters(Request,
			[ scheme(Schemes,
				 [zero_or_more,
				  description('Zero or more concept schemes')])
			]),
	new_alignment(Schemes, Graph),
	build_redirect(Request, Graph).


%%	new_alignment(+Schemes, -AlignmentURI)
%
%	Assert a new alignment graph.

new_alignment(Schemes, Alignment) :-
	authorized(write(default, _)),
	rdf_bnode(Alignment),
	rdf_transaction((rdf_assert(Alignment, rdf:type, amalgame:'Alignment', Alignment),
			 assert_user_provenance(Alignment, Alignment),
			 add_schemes(Schemes, Alignment))).

add_schemes([], _).
add_schemes([Scheme|Ss], A) :-
	rdf_assert(A, amalgame:includes, Scheme, A),
	add_schemes(Ss, A).



%%	http_eq_upload_data(+Request)
%
%	Handler for alignment import

http_eq_upload_data(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ data(Data,
			       [ description('RDF data to be loaded')
			       ])
			]),
	rdf_bnode(Graph),
	atom_to_memory_file(Data, MemFile),
	setup_call_cleanup(open_memory_file(MemFile, read, Stream),
			   rdf_guess_format_and_load(Stream, [graph(Graph)]),
			   ( close(Stream),
			     free_memory_file(MemFile)
			   )),
	build_redirect(Request, Graph).

http_eq_upload_url(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ url(URL, [])
			]),
	rdf_bnode(Graph),
	rdf_load(URL, [graph(Graph)]),
	build_redirect(Request, Graph).

build_redirect(Request, Graph) :-
	http_link_to_id(http_eq_build, [alignment(Graph)], Redirect),
	http_redirect(moved, Redirect, Request).
