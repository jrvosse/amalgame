:- module(eq_selecter,
	  [html_schemes_only//0  % for backward compat with europeana demo
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(yui3_beta)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_file_type)).
:- use_module(library(skos/vocabularies)).
:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(library(amalgame/util)).

:- use_module(applications(skos_browser)).

% http handlers for this applications
:- http_handler(amalgame(eq), http_eq, []).
:- http_handler(amalgame(new), http_eq_new, []).
:- http_handler(amalgame(select), http_eq_select, []).
:- http_handler(amalgame(load/url), http_eq_upload_url, []).
:- http_handler(amalgame(load/data), http_eq_upload_data, []).

%%	http_eq(+Request)
%
%	Emit html page to start a new or load an existing alignment
%	strategy.

http_eq(_Request) :-
	% authorized(write(default, _)),
	html_page.

http_eq_select(Request) :-
	http_parameters(Request,
			[
			 alignment(Strategy,
				    [uri,
				     description('URI of the selected strategy')]),
			 submit(Action,
				[oneof(['View selected','Delete selected']),
				 description('Action to be performed on this strategy'),
				 default('View selected')
				])
		       ]),
	(   Action == 'View selected'
	->  build_redirect(Request, Strategy)
	;   Action == 'Delete selected'
	->  delete_redirect(Request, Strategy)
	).

find_schemes(Schemes) :-
	findall(C, rdfs_individual_of(C, skos:'ConceptScheme'), Cs),
	findall(G, is_edm_collection(G), Gs),
	append(Cs, Gs, All),
	sort(All, Schemes).

html_page :-
	findall(A-S, amalgame_alignment(A, S), Alignments),
	find_schemes(ConceptSchemes),
	reply_html_page(cliopatria(main),
			[ title(['Amalgame - strategies'])
			],
			[ \html_requires(css('selecter.css')),
			  \yui3_combo(yui3,
				      ['cssreset/reset-min.css',
				       'cssgrids/grids-min.css',
				       'cssfonts/fonts-min.css'
				      ]),
			  div(class('yui-skin-sam yui3-skin-sam'),
			      [ div(id(header), []),
				div(id(main),
				    [
				      div([id(content), class('yui3-accordion')],
					  [
					    \html_open(Alignments),
					    \html_new(ConceptSchemes),
					    \html_import,
					    \html_publish(Alignments)
					  ])
				    ]),
				script(type('text/javascript'),
				       [ \yui_script
				       ])
			      ])
			]).


html_schemes_only -->
	{
	 find_schemes(ConceptSchemes)
	},
	html_new(ConceptSchemes).

%%	html_new
%
%

html_new(Schemes) -->
	{
	 has_write_permission, !
	},
	html_acc_item(new, 'new alignment strategy',
		      [ form(action(location_by_id(http_eq_new)),
			     [ \html_vocab_table(Schemes),
			       \html_submit('Start')
			     ])
		      ]).

html_new(_) -->
	{
	 http_location_by_id(http_eq, This),
	 http_link_to_id(cliopatria_openid:login_page,
			 ['openid.return_to'(This)], Login)
	},
	html_acc_item(new,
		      'please login to access other functions',
		      [
		       div(a([class(login), href(Login)], ['login']))
		      ]).

html_vocab_table(Vs) -->
	html(table([thead(tr(\html_vocab_head)),
		    tbody(\html_vocab_rows(Vs))
		   ])).

html_vocab_head -->
	html([th([]),
	      th(name),
	      th('# concepts'),
	      th('# prefLabels'),
	      th('# altLabels'),
	      th('# mapped'),
	      th('(%)')
	     ]).

html_vocab_rows([]) --> !.
html_vocab_rows([Scheme|Vs]) -->
% rdf_estimate_complexity(_, skos:inScheme, Scheme, Count)
	{
	 (   voc_ensure_stats(all(Scheme))
	 ->  rdf(Scheme, amalgame:numberOfConcepts,   literal(type(_,ConceptCount))),
	     rdf(Scheme, amalgame:numberOfPrefLabels, literal(type(_,PrefCount))),
	     rdf(Scheme, amalgame:numberOfAltLabels, literal(type(_, AltCount))),
	     rdf(Scheme, amalgame:numberOfMappedConcepts, literal(type(_, MappedCount))),
	     voc_languages(Scheme, skos:prefLabel, PrefLangs),
	     voc_languages(Scheme, skos:altLabel, AltLangs)
	 ;   ConceptCount = 0,
	     PrefCount = 0, AltCount = 0,
	     MappedCount = 0,
	     PrefLangs=[], AltLangs=[]
	 ),
	 (   ConceptCount > 0
	 ->  Perc is (100*MappedCount)/ConceptCount,
	     format(atom(MPercent), '(~2f%)', [Perc])
	 ;   MPercent = -
	 )
	},
	html(tr([td(input([type(checkbox), autocomplete(off), class(option),
			   name(scheme), value(Scheme)])),
		 td(\html_scheme_name(Scheme)),
		 td(class(count), ConceptCount),
		 td([span(class(prefLabel), PrefCount),
		     span(class(preflangs), [' (', \showlist(PrefLangs), ')'])]),
		 td([span(class(altLabel), AltCount),
		      span(class(altlangs), [' (', \showlist(AltLangs),  ')'])]),
		 td(class(mapped), MappedCount),
		 td(class(pmapped), MPercent)
		])),
	html_vocab_rows(Vs).


%%	html_open(+Alignments)
%
%
html_open([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no strategies have been created yet'),
		      []),
	!.
html_open(Alignments) -->
	html_acc_item(open, 'edit/delete pre-loaded alignment strategy',
		      [ form(action(location_by_id(http_eq_select)),
			     [
			       \html_alignment_table(Alignments,
						    [linkto(http_eq_build)]),
			       \html_submit('View selected'),
			       \html_submit('Delete selected')
			     ])
		      ]).
html_publish([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no mappings have been created yet'),
		      []),
	!.
html_publish(Alignments) -->
	{
	 has_write_permission,
	 L=http_eq_publish_form,
	 !
	},
	html_acc_item(publish, 'publish	alignment results',
		      [ form(action(location_by_id(L)),
			     [ \html_alignment_table(Alignments, [linkto(L)]),
			       \html_submit('Publish')
			     ])
		      ]).
html_publish(_) -->  !.


%%	html_alignment_table(+Graphs, +Options)
%
%	Emit HTML table with alignment graph properties.

html_alignment_table(Alignments, Options) -->
	html(table([thead(tr(\html_alignment_head)),
		    tbody(\html_alignment_rows(Alignments, Options))
		   ])).

html_alignment_head -->
	html([th([]),
	      th(name),
	      th(includes),
	      th('created by')
	     ]).

html_alignment_rows([],_) --> !.
html_alignment_rows([URI-Schemes|Gs], Options) -->
	{
	 (   rdf(URI, dcterms:creator, Author, URI)
	 ->  true
	 ;   Author = anonymous
	 )
	},
	html(tr([td(input([type(radio), autocomplete(off), class(option), name(alignment), value(URI)])),
		 td(\html_strategy_name(URI, Options)),
		 td(\html_scheme_labels(Schemes)),
		 td(\turtle_label(Author))
		])),
	html_alignment_rows(Gs, Options).

html_scheme_labels([]) --> !.
html_scheme_labels([S|Ss]) -->
	html(div(\turtle_label(S))),
	html_scheme_labels(Ss).

html_strategy_name(Graph, Options) -->
	{ graph_label(Graph, Label),
	  option(linkto(LinkTo), Options, http_eq_build),
	  http_link_to_id(LinkTo, [alignment(Graph)], Link)
	},
	html(a([href(Link)],Label)).

html_scheme_name(Graph) -->
	{ graph_label(Graph, Label),
	  http_link_to_id(http_skos_browser, [scheme(Graph)], Link)
	},
	html(a([href(Link)],Label)).

graph_label(Graph, Label) :-
	rdf_display_label(Graph, Lit),
	literal_text(Lit, Label).
graph_label(Graph, Graph).


html_import -->
	{
	 has_write_permission,
	 !
	},
	html_acc_item(import, 'upload strategy or clone execution trace',
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

html_import --> !.

%%	html_submit(+Label)
%
%

html_submit(Label) -->
	html(span(class(controls),
		 [ input([type(submit), autocomplete(off), class(start),
			  name(submit), disabled(true), value(Label)])
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
	     [ \yui3_new(eq, 'Y.Selecter', [])
	     ]).


%%	js_module(+Key, +Module_Conf)
%
%	YUI3 and application specific modules used in javascript.

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
	setting(eq_publisher:default_namespace, NS),
	repeat, gensym(strategy, Local),
	atomic_list_concat([NS,Local], Alignment),
	\+ rdf_graph(Alignment),
	!,
	rdf_transaction((
			 rdf_assert(Alignment, rdf:type, amalgame:'AlignmentStrategy', Alignment),
			 rdf_assert(Alignment, rdf:type, prov:'Plan', Alignment),
			 rdf_assert(Alignment, amalgame:publish_ns, NS, Alignment),
			 assert_user_provenance(Alignment, Alignment),
			 add_schemes(Schemes, Alignment))).

add_schemes([], _).
add_schemes([Scheme|Ss], Strategy) :-
	rdf_assert(Strategy, amalgame:includes, Scheme, Strategy),
	add_schemes(Ss, Strategy).



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
	rdf_bnode(TmpGraph),
	atom_to_memory_file(Data, MemFile),
	setup_call_cleanup(open_memory_file(MemFile, read, Stream),
			   rdf_guess_format_and_load(Stream, [graph(TmpGraph)]),
			   ( close(Stream),
			     free_memory_file(MemFile)
			   )),
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy', TmpGraph),!,
	rdf_unload_graph(Strategy), % Delete old strategies under the same name

	% Copy entire strategy graph to keep original named graph:
	findall(rdf(S,P,O), rdf(S,P,O,TmpGraph), Triples),
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O,Strategy)),
	rdf_unload_graph(TmpGraph),
	build_redirect(Request, Strategy).

http_eq_upload_url(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ url(URL, [])
			]),
	rdf_bnode(TmpGraph),
	rdf_load(URL, [graph(TmpGraph)]),
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy', TmpGraph),!,
	rdf_unload_graph(Strategy), % Delete old strategies under the same name

	% Copy entire strategy graph to keep original named graph:
	findall(rdf(S,P,O), rdf(S,P,O,TmpGraph), Triples),
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O,Strategy)),
	rdf_unload_graph(TmpGraph),
	build_redirect(Request, Strategy).

build_redirect(Request, Strategy) :-
	http_link_to_id(http_eq_build, [alignment(Strategy)], Redirect),
	http_redirect(moved, Redirect, Request).

delete_redirect(Request, Strategy) :-
	rdf_unload_graph(Strategy),
	http_link_to_id(http_eq, [], Redirect),
	http_redirect(moved, Redirect, Request).

showlist([]) --> !.
showlist([H]) -->  html(H),!.
showlist([H1,H2|Tail]) -->  html([H1,', ']), showlist([H2|Tail]).
