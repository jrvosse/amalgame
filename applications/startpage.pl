:- module(ag_start_page,
	  [html_schemes_only//0  % for backward compat with europeana demo
	  ]).

:- use_module(library(option)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_file_type)).

:- use_module(user(user_db)).

:- use_module(library(yui3_beta)).
:- use_module(components(label)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(applications(skos_browser)).

:- use_module(components(amalgame/util)).

% main http handler for amalgame:
:- http_handler(amalgame(.), http_amalgame_main_page, []).

% Backward compatibility
:- http_handler(amalgame(eq),	    http_redirect(moved, amalgame(.)), []).
:- http_handler(amalgame(app/main), http_redirect(moved, amalgame(.)), []).

% handlers for the different forms on the main page.
% most handle the form request and then redirect to some other page,
% such as the strategy builder or the main page:
:- http_handler(amalgame(form/new),       http_ag_form_new_strategy, []).
:- http_handler(amalgame(form/select),    http_ag_form_select_strategy, []).
:- http_handler(amalgame(form/url),	  http_ag_form_upload_strategy_resource, []).
:- http_handler(amalgame(form/data),      http_ag_form_upload_strategy_data, []).
:- http_handler(amalgame(form/reference), http_ag_form_upload_reference, []).

%%      http_amalgame_main_page(+Request) is det.
%
%	Emit html page to start a new or select/upload an existing
%	alignment strategy.

http_amalgame_main_page(Request) :-
	html_main_page(Request).


%%     http_ag_form_select_strategy(+Request)
%
%      Execute action on selected strategy and redirect to
%      appropriate page.

http_ag_form_select_strategy(Request) :-
	http_parameters(Request,
			[
			 strategy(Strategies,
				    [list(uri),
				     description('URI of the selected strategy')]),
			 submit(Action,
				[oneof(['View selected',
					'Merge selected',
					'Delete selected']),
				 description('Action to be performed on this strategy'),
				 default('View selected')
				])
		       ]),
	(   Action == 'View selected'
	->  build_redirect(Request, Strategies)
	;   Action == 'Merge selected'
	->  merge_redirect(Request, Strategies)
	;   Action == 'Delete selected'
	->  delete_redirect(Request, Strategies)
	).

%%	http_ag_form_new_strategy(+Request)
%
%	Handle form data to create a new strategy

http_ag_form_new_strategy(Request) :-
	http_parameters(Request,
			[ scheme(Schemes,
				 [zero_or_more,
				  description('Zero or more concept schemes')])
			]),
	new_strategy(Graph, [schemes(Schemes), comment('New strategy')]),
	build_redirect(Request, [Graph]).



%%      http_ag_form_upload_strategy_data(+Request) is det.
%
%	Handler for strategy form data import.

http_ag_form_upload_strategy_data(Request) :-
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
	cp_strategy_from_tmp(Request, TmpGraph).

%%      http_ag_form_upload_strategy_resource(+Request) is det.
%
%	Handler for strategy form resource import.

http_ag_form_upload_strategy_resource(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ url(URL, [])
			]),
	rdf_bnode(TmpGraph),
	rdf_load(URL, [graph(TmpGraph)]),
	cp_strategy_from_tmp(Request, TmpGraph).

%%	http_ag_form_upload_reference(+Request) is det.
%
%	Handle form to upload an existing strategy
http_ag_form_upload_reference(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ data(Data,
			       [ description('RDF data to be loaded')
			       ])
			]),
	new_reference_name(NamedGraph),
	atom_to_memory_file(Data, MemFile),
	setup_call_cleanup(open_memory_file(MemFile, read, Stream),
			   rdf_guess_format_and_load(Stream, [graph(NamedGraph)]),
			   ( close(Stream),
			     free_memory_file(MemFile)
			   )),
	rdf_equal(amalgame:'LoadedMapping', LMGraph),
	rdf_assert(NamedGraph, rdf:type, amalgame:'LoadedMapping', LMGraph),

	http_link_to_id(list_graph, [graph(NamedGraph)], ListGraph),
	http_redirect(moved, ListGraph, Request).


find_schemes(Schemes) :-
	findall(C,
		(   is_vocabulary(C),
		    voc_property(C, virtual(false))
		),
		All),
	maplist(scheme_label, All, Labeled),
	keysort(Labeled, Sorted),
	pairs_values(Sorted, Schemes).

scheme_label(URI, Key-URI) :-
	graph_label(URI, CasedKey),
	downcase_atom(CasedKey, Key).

html_main_page(_Request) :-
	findall(A-S, amalgame_strategy_schemes(A, S), Alignments),
	find_schemes(ConceptSchemes),
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
						\html_open(Alignments),
						\html_reference,
						\html_import,
						\html_publish(Alignments),
						\html_new(ConceptSchemes)
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

html_new([]) -->
	html_acc_item(new,
		      'new alignment strategy: no (SKOS) vocabularies found',
		      div([style('padding: 1%')],[
			  'Please use the Repository drop-down menu to load ',
			  'the vocabularies you would like to align ',
			  'into the repository/triple store.'
		      ]),
		      [active]
		     ).

html_new(Schemes) -->
	{ has_write_permission, !,
	  ButtonsBottom = div(\html_submit('Start')),
	  length(Schemes, N),
	  (   N > 7
	  ->  ButtonsTop = ButtonsBottom
	  ;   ButtonsTop = div([],[])
	  )
	},
	html_acc_item(new,
		      'new alignment strategy',
		      [ form(action(location_by_id(http_ag_form_new_strategy)),
			     [  ButtonsTop,
				\html_vocab_table(Schemes),
				ButtonsBottom
			     ])
		      ],
		      [active]
		     ).

html_new(_) -->
	{
	 http_location_by_id(http_amalgame_main_page, This),
	 http_link_to_id(cliopatria_openid:login_page,
			 ['openid.return_to'(This)], Login)
	},
	html_acc_item(new,
		      'please login to access other functions',
		      [
			  div(a([class(login), href(Login)], ['login']))
		      ],
		      [inactive]
		     ).

html_vocab_table(Vs) -->
	html([
	    \html_requires(sortable),
	    table([class(sortable)],
		   [thead(tr(\html_vocab_head)),
		    tbody(\html_vocab_rows(Vs))
		   ])
	]).

html_vocab_head -->
	html([th([]),
	      th(class(name),       name),
	      th(class(version),    version),
	      th(class(count),     'estimated #concepts'),
	      th(class(preflangs), 'prefLabels'),
	      th(class(altlangs),  'altLabels')
	     ]).

html_vocab_rows([]) --> !.
html_vocab_rows([Scheme|Vs]) --> {
    (   voc_property(Scheme, numberOfConcepts(ConceptCount), [compute(false)])
    ->  true
    ;   rdf_estimate_complexity(_, skos:inScheme, Scheme, ConceptCount)
    ),
    voc_property(Scheme, languages(skos:prefLabel, PrefLangs)),
    voc_property(Scheme, languages(skos:altLabel, AltLangs)),
    voc_property(Scheme, version(Version0)),
    (	Version0 == ''
    ->	voc_property(Scheme, revision(Version))
    ;	Version = Version0
    )
},
	html(tr([td(input([type(checkbox), autocomplete(off), class(option),
			   name(scheme), value(Scheme)])),
		 td(class(name),    \html_scheme_name(Scheme)),
		 td(class(version), Version),
		 td(class(count), ConceptCount),
		 td([span(class(preflangs), \html_showlist(PrefLangs))]),
		 td([span(class(altlangs),  \html_showlist(AltLangs) )])

		])),
	html_vocab_rows(Vs).


%%	html_open(+Alignments)
%
%
html_open([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no strategies have been created yet'),
		      [],
		      [inactive]),
	!.
html_open(Alignments) -->
	{ ButtonsBottom = div([ \html_submit('View selected'),
			  \html_submit('Merge selected'),
			  \html_submit('Delete selected')
			]),
	  length(Alignments, N),
	  (   N > 7
	  ->  ButtonsTop = ButtonsBottom
	  ;   ButtonsTop = div([],[])
	  )
	},
	html_acc_item(open,
		      'edit/delete pre-loaded alignment strategy',
		      [ form(action(location_by_id(http_ag_form_select_strategy)),
			     [
				 ButtonsTop,
				 \html_strategy_table(Alignments,
						       [linkto(http_ag_build)]),
				 ButtonsBottom

			     ])
		      ],
		      [active]
		     ).
html_publish([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no mappings have been created yet'),
		      [],
		      [inactive]),
	!.
html_publish(Strategies) -->
	{
	 has_write_permission,
	 L=http_ag_publish_form,
	 !
	},
	html_acc_item(publish,
		      'publish alignment strategy results',
		      [ form(action(location_by_id(L)),
			     [ \html_strategy_table(Strategies, [linkto(L)]),
			       \html_submit('Publish')
			     ])
		      ],
		      [inactive]).
html_publish(_) -->  !.


%%	html_strategy_table(+Graphs, +Options)
%
%	Emit HTML table with strategy graph properties.

html_strategy_table(Strategies, Options) -->
	html([
	    \html_requires(sortable),
	    table(
		[class(sortable)],
		[ thead(tr(\html_alignment_head)),
		  tbody(\html_alignment_rows(Strategies, Options))
		])
	]).

html_alignment_head -->
	html([th([]),
	      th(name),
	      th(includes),
	      th('Created by:'),
	      th('Comment:')
	     ]).

html_alignment_rows([],_) --> !.
html_alignment_rows([URI-Schemes|Gs], Options) -->
	{
	 (   rdf(URI, dcterms:creator, Author, URI)
	 ->  true
	 ;   Author = anonymous
	 ),
	 (   rdf(URI, rdfs:comment, CommentR, URI)
	 ->  literal_text(CommentR, Comment)
	 ;   Comment = ''
	 )
	},
	html(tr([td(input([type(checkbox), autocomplete(off), class(option), name(strategy), value(URI)])),
		 td(\html_strategy_name(URI, Options)),
		 td(\html_scheme_labels(Schemes)),
		 td(\turtle_label(Author)),
		 td([class(comment)],Comment)
		])),
	html_alignment_rows(Gs, Options).

html_scheme_labels([]) --> !.
html_scheme_labels([S|Ss]) -->
	html(div(\turtle_label(S))),
	html_scheme_labels(Ss).

html_strategy_name(Graph, Options) -->
	{ graph_label(Graph, Label),
	  option(linkto(LinkTo), Options, http_ag_build),
	  http_link_to_id(LinkTo, [strategy(Graph)], Link)
	},
	html(a([href(Link)],Label)).

html_scheme_name(Graph) -->
	{ graph_label(Graph, Label),
	  http_link_to_id(http_skos_browser, [scheme(Graph)], Link)
	},
	html(a([href(Link)],Label)).

graph_label(Graph, Label) :-
	rdf_display_label(Graph, Lit),
	literal_text(Lit, Label),!.
graph_label(Graph, Graph).

html_reference -->
	{ has_write_permission,
	  !
	},
	html_acc_item(reference,
		      'upload existing/reference alignment',
		      form([action(location_by_id(http_ag_form_upload_reference)),
			    method('POST'),
			    enctype('multipart/form-data') ],
			   [ p(['Upload an exisiting alignment to build upon, ',
				'or to use as reference (ground truth)' ]),
			     input([type(file), name(data),
				    size(50), autocomplete(off)
				   ]),
			     input([type(submit), value('Upload')])
			   ]),
		      [inactive]
		     ).

html_reference --> !.

html_import -->
	{
	 has_write_permission,
	 !
	},
	html_acc_item(import,
		      'upload strategy or clone execution trace',
		      [ form([action(location_by_id(http_ag_form_upload_strategy_resource)),
			      method('POST')
			     ],
			     [ 'URL: ',
			       input([type(text), name(url), value('http://'),
				      autocomplete(off), size(50)
				     ]),
			       input([type(submit), value('Upload')])
			   ]),
			form([action(location_by_id(http_ag_form_upload_strategy_data)),
			      method('POST'),
			      enctype('multipart/form-data')
			     ],
			     [ 'File: ',
			       input([type(file), name(data),
				      size(50)%, autocomplete(off)
				     ]),
			       input([type(submit), value('Upload')])
			     ])
		      ],
		      [inactive]).

html_import --> !.

%%	html_submit(+Label)
%
%

html_submit(Label) -->
	html(span(class(controls),
		 [ input([type(submit), autocomplete(off), class(start),
			  name(submit), disabled(true), value(Label)])
		 ])).


%%	html_acc_item(+Id, +Label, +HTMLBody, +Options)
%
%	Emit html markup for a YUI3 accordion item.
%	Options:
%
%	* active/inactive

html_acc_item(Id, Label, Body, Options) -->
	{ (   option(active, Options)
	  ->  Class = 'yui3-accordion-item yui3-accordion-item-active'
	  ;   Class = 'yui3-accordion-item'
	  )
	},
	html(div([class(Class), id(Id)],
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


%%	new_strategy(-StrategyURI, Options)
%
%	Assert a new strategy graph.

new_strategy(S, Options) :-
	authorized(write(default, _)),
	new_strategy_name(S, NS),
	rdf_assert(S, rdf:type, amalgame:'AlignmentStrategy', S),
	rdf_assert(S, rdf:type, prov:'Plan', S),
	rdf_assert(S, amalgame:publish_ns, NS, S),
	assert_user_provenance(S, S),

	(   option(schemes(Schemes), Options)
	->  add_schemes(Schemes, S)
	;   true),

	(   option(comment(C), Options)
	->  rdf_assert(S, rdfs:comment, literal(C), S)
	;   true
	).

add_schemes([], _).
add_schemes([Scheme|Ss], Strategy) :-
	rdf_assert(Strategy, amalgame:includes, Scheme, Strategy),
	add_schemes(Ss, Strategy).

new_strategy_name(Strategy, NS) :-
	setting(amalgame:default_publish_namespace, NS),
	reset_gensym(strategy),
	repeat,
	gensym(strategy, Local),
	atomic_list_concat([NS,Local], Strategy),
	\+ rdf_graph(Strategy),
	!.
new_reference_name(Reference) :-
	setting(amalgame:default_publish_namespace, NS),
	reset_gensym(reference_alignment),
	repeat,
	gensym(reference_alignment, Local),
	atomic_list_concat([NS,Local], Reference),
	\+ rdf_graph(Reference),
	!.

cp_strategy_from_tmp(Request, TmpGraph) :-
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy', TmpGraph),!,
	cp_graph(TmpGraph, Strategy, true),
	rdf_unload_graph(TmpGraph),
	build_redirect(Request, [Strategy]).

build_redirect(Request, [Strategy|_]) :-
	http_link_to_id(http_ag_build, [strategy(Strategy)], Redirect),
	http_redirect(moved, Redirect, Request).

delete_redirect(Request, Strategies) :-
	authorized(write(default, _)),
	forall(member(Strategy, Strategies),
	       (   (   provenance_graph(Strategy, Prov)
		   ->  rdf_unload_graph(Prov)
		   ;   true
		   ),
		   rdf_unload_graph(Strategy)
	       )
	      ),
	http_link_to_id(http_amalgame_main_page, [], Redirect),
	http_redirect(moved, Redirect, Request).

merge_redirect(Request, Strategies) :-
	% Create comment
	maplist(scheme_label, Strategies, Labeled),
	keysort(Labeled, Sorted),
	pairs_keys(Sorted, Labels),
	atomic_list_concat(Labels, ', ', LabelsAtom),
	atomic_concat('Strategy merged from ', LabelsAtom, Comment),

	% Create merged strategy
	new_strategy(New, [comment(Comment)]),
	cp_graphs(Strategies, New),
	merge_strategy_nodes(Strategies, New),

	% Redirect to builder
	http_link_to_id(http_ag_build, [strategy(New)], Redirect),
	http_redirect(moved, Redirect, Request).

merge_strategy_nodes([], _New) :- !.
merge_strategy_nodes([H|T], New) :-
	findall(rdf(H,P,O),
		rdf(H,P,O,New),
		Triples),
	forall(member(rdf(_,P,O), Triples),
	       rdf_assert(New,P,O,New)),
	merge_strategy_nodes(T, New).

cp_graphs([], _Target) :- !.
cp_graphs([Head|Tail], Target) :-
	cp_graph(Head, Target, false),
	cp_graphs(Tail, Target).

cp_graph(Source, Target, true) :-
	rdf_unload_graph(Target), % Delete old graphs under the same name
	cp_graph(Source, Target, false).

cp_graph(Source, Target, false) :-
	findall(rdf(S,P,O), rdf(S,P,O,Source), Triples),
	forall(member(rdf(S,P,O), Triples),
	       rdf_assert(S,P,O,Target)).
