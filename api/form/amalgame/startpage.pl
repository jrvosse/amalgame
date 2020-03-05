:- module(ag_form_startpage,
	  [
	  ]).

:- use_module(library(apply)).
:- use_module(library(gensym)).
:- use_module(library(memfile)).
:- use_module(library(pairs)).
:- use_module(library(settings)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_file_type)).

:- use_module(user(user_db)).
:- use_module(user(preferences)).

:- use_module(library(amalgame/rdf_util)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/util)).

% handlers for the different forms on the start page.
% most handle the form request and then redirect to some other page,
% such as the strategy builder or back to the start page:

:- http_handler(amalgame(form/new),       http_ag_form_new_strategy, []).
:- http_handler(amalgame(form/select),    http_ag_form_select_strategy, []).
:- http_handler(amalgame(form/url),	  http_ag_form_upload_strategy_resource, []).
:- http_handler(amalgame(form/data),      http_ag_form_upload_strategy_data, []).
:- http_handler(amalgame(form/reference), http_ag_form_upload_reference, []).

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
	rdf_create_bnode(TmpGraph),
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
	rdf_create_bnode(TmpGraph),
	rdf_load(URL, [graph(TmpGraph)]),
	cp_strategy_from_tmp(Request, TmpGraph).

%%	http_ag_form_upload_reference(+Request) is det.
%
%	Handle form to upload an existing reference mapping
http_ag_form_upload_reference(Request) :-
	authorized(write(default, _)),
	is_multipart_post_request(Request), !,
	http_read_data(Request, Parts,
                       [ on_filename(load_file_into_named_graph)
                       ]),
	memberchk(data=file(_FileName, NamedGraph), Parts),
	http_link_to_id(list_graph, [graph(NamedGraph)], ListGraph),
	http_redirect(moved, ListGraph, Request).

http_ag_form_upload_reference(_Request) :-
        throw(http_reply(bad_request(bad_file_upload))).

:- public load_file_into_named_graph/3.

load_file_into_named_graph(InStream, file(FileName, NamedGraph), Options) :-
	option(filename(FileName), Options),
	format(atom(FileURL), 'file://~w', [FileName]),
	new_reference_name(NamedGraph),
	% atom_to_memory_file(Data, MemFile),
	rdf_guess_format_and_load(InStream, [graph(NamedGraph), base_uri(FileURL)]),
	now_xsd(TS),
	rdf_equal(amalgame:'LoadedMapping', LMGraph),
	rdf_assert(NamedGraph, amalgame:uploadedFrom, FileURL,  LMGraph),
	rdf_assert(NamedGraph, prov:generatedAtTime, TS^^xsd:dateTime, LMGraph),
	rdf_assert(NamedGraph, rdf:type, amalgame:'LoadedMapping', LMGraph).

is_multipart_post_request(Request) :-
        memberchk(method(post), Request),
        memberchk(content_type(ContentType), Request),
        http_parse_header_value(
            content_type, ContentType,
            media(multipart/'form-data', _)).


cp_strategy_from_tmp(Request, TmpGraph) :-
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy', TmpGraph),!,
	rdf_cp_graph(TmpGraph, Strategy, true),
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
	rdf_cp_graphs(Strategies, New),
	merge_strategy_nodes(Strategies, New),

	% Redirect to builder
	http_link_to_id(http_ag_build, [strategy(New)], Redirect),
	http_redirect(moved, Redirect, Request).

new_reference_name(Reference) :-
	setting(amalgame:default_publish_namespace, NS),
	reset_gensym(reference_alignment),
	repeat,
	gensym(reference_alignment, Local),
	atomic_list_concat([NS,Local], Reference),
	\+ rdf_graph(Reference),
	!.


merge_strategy_nodes([], _New) :- !.
merge_strategy_nodes([H|T], New) :-
	findall(rdf(H,P,O),
		rdf(H,P,O,New),
		Triples),
	forall(member(rdf(_,P,O), Triples),
	       rdf_assert(New,P,O,New)),
	merge_strategy_nodes(T, New).

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
	->  user_preference(user:lang, LangLiteral),
	    literal_text(LangLiteral, Lang),
	    rdf_assert(S, rdfs:comment, C@Lang, S)
	;   true
	).

scheme_label(URI, Key-URI) :-
	rdf_graph_label(URI, CasedKey),
	downcase_atom(CasedKey, Key).

add_schemes([], _).
add_schemes([Scheme|Ss], Strategy) :-
	strategy_add_vocabulary(Strategy, Scheme),
	add_schemes(Ss, Strategy).

new_strategy_name(Strategy, NS) :-
	setting(amalgame:default_publish_namespace, NS),
	reset_gensym(strategy),
	repeat,
	gensym(strategy, Local),
	atomic_list_concat([NS,Local], Strategy),
	\+ rdf_graph(Strategy),
	!.



:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
	[ 'A file upload must be submitted as multipart/form-data using', nl,
	  'name=file and providing a file-name'
	].
