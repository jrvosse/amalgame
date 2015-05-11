:- module(ag_process,
	  [
	  ]).

:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(user(user_db)).

:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/json_util)).
:- use_module(library(amalgame/amalgame_modules)).


:- setting(amalgame:precompute, boolean, true,
	   'When true (default) new mappings and virtual vocabularies are pre-computed in the background').

:- http_handler(amalgame(data/addprocess), http_add_process, []).
:- http_handler(amalgame(data/updatenode), http_update_node, []).
:- http_handler(amalgame(data/deletenode), http_delete_node, []).

%%	http_add_process(+Request)
%
%

http_add_process(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ input(Input,
				[uri,
				 optional(true),
				 description('URI of (primary) input mapping')]),
			  secondary_input(SecInputs0,
				  [uri,
				   zero_or_more,
				   description('List of mappings used as aditional input (e.g by exclude or ancestor matching)')]),
			  source(Source,
				 [uri,
				  optional(true),
				  description('URI of the source')]),
			  target(Target,
				 [uri,
				  optional(true),
				  description('URI of the target')]),
			  process(Process,
				  [uri,
				   description('URI of an existing process or type of new process')]),
			  strategy(Strategy,
				    [uri,
				     description('URI of the strategy graph to which the process is added')]),
			  update(Update,
				 [boolean, default(false),
				  descrption('When set to true process is updated with new parameters')])
			],
			[form_data(Params0)]),
	sort(SecInputs0, SecInputs),
	subtract(Params0, [input=_,source=_,target=_,process=_,strategy=_,update=_,graphic=_], Params1),
	findall(secondary_input=S,member(secondary_input=S, Params1), SecParams),
	subtract(Params1, SecParams, Params),
	expand_options(Params, ExpandedParams),
	flush_refs_cache_if_needed(Process),
	(   Update == true
	->  rdf_retractall(Process, amalgame:secondary_input, _, Strategy),
	    flush_dependent_caches(Process, Strategy),
	    strategy_update_process_parameters(Strategy, Process, SecInputs, ExpandedParams),
	    Focus = Process, URI = Process
	;   ((nonvar(Source), nonvar(Target)) ; nonvar(Input))
	->  strategy_new_process(Strategy, Process, Source, Target, Input, SecInputs, ExpandedParams, Focus, URI)
	;   true
	),
	% precompute results to speed things up
	(   setting(amalgame:precompute, true)
	->  precompute_process(Strategy, URI)
	;   true),
	js_focus_node(Strategy, Focus, FocusNode),
	js_strategy_nodes(Strategy, Nodes),
	reply_json(json{focus:FocusNode,
			nodes:Nodes}).


%%	http_update_node(+Request)
%
%	Change properties of a URI in Strategy and return the new
%	nodes in Strategy.

http_update_node(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri,
				     description('URI of strategy')
				    ]),
			  uri(URI,
				[uri,
				 description('URI of input resource')]),
			  publish_ns(PublishNS,
				     [uri,
				      description('Optional uri of new publication namespace'),
				      default('same')
				     ])
			],
			[form_data(Params)
			]),
	strategy_update_node(Strategy, Params, URI),
	update_amalgame_prov(Strategy, URI),
	change_ns_if_needed(PublishNS, URI, Strategy, NewStrategy),
	flush_deps_if_needed(Strategy, URI, Params),
	js_strategy_nodes(NewStrategy, Nodes),
	js_focus_node(NewStrategy, URI, FocusNode),

	reply_json(json{strategy:NewStrategy,
			 nodes:Nodes,
			 focus:FocusNode
		       }),

	% precompute results to speed things up
	(   setting(amalgame:precompute, true)
	->  precompute_node(Strategy, URI)
	;   true).

%%	http_delete_node(+Request)
%
%	delete URI in Strategy and all that are connected to it and
%	return the new nodes in Strategy.

http_delete_node(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri,
				     description('URI of strategy')
				    ]),
			  uri(URI,
				[uri,
				 description('URI of input resource')])
			]),
	strategy_delete_node(Strategy, URI),
	js_strategy_nodes(Strategy, Nodes),
	js_focus_node(Strategy, Strategy, FocusNode),
	reply_json(json{nodes:Nodes,
			focus:FocusNode
		       }).


change_ns_if_needed(NS, URI, Strategy, NewStrategy) :-
	rdf(Strategy, amalgame:publish_ns, OldNS, Strategy),
	(   (OldNS == NS; NS == 'same')
	->  NewStrategy = Strategy
	;   provenance_graph(Strategy, Prov),
	    rdf_unload_graph(Prov),
	    rdf_retractall(URI, amalgame:publish_ns, OldNS, Strategy),
	    rdf_assert(URI, amalgame:publish_ns, NS, Strategy),
	    flush_expand_cache(Strategy),
	    change_namespace(OldNS, NS, Strategy, NewStrategy)
	).

change_namespace(Old, New, Strategy, NewStrategy) :-
	(   sub_atom(Strategy, 0, Len, After, Old)
	->  sub_atom(Strategy, Len, After, 0, Local),
	    atom_concat(New, Local, NewStrategy)
	;   NewStrategy = Strategy
	),
	% fix subjects:
	findall(rdf(S,P,O), tainted_s_ns(S,P,O, Old, Strategy), SResults),
	forall(member(T, SResults), fix_s_ns(T, Old, New)),

	% fix objects:
	findall(rdf(S,P,O), tainted_o_ns(S,P,O, Old, Strategy), OResults),
	forall(member(T, OResults), fix_o_ns(T, Old, New)),

	% fix graphs
	rdf_transaction(forall(rdf(S,P,O,Strategy),
			       rdf_update(S,P,O,graph(NewStrategy))
			      )
		       ).

tainted_s_ns(S,P,O,Old,Strategy) :-
	rdf(S,P,O,Strategy:_),
	sub_atom(S, 0,_,_,Old).

tainted_o_ns(S,P,O,Old,Strategy) :-
	rdf(S,P,O,Strategy:_),
	rdf_is_resource(O),
	sub_atom(O, 0,_,_,Old).


fix_s_ns(rdf(S,P,O), Old, New) :-
	sub_atom(S,0,Len,After,Old),
	sub_atom(S,Len,After,0, Local),
	atom_concat(New,Local,NewS),
	rdf_update(S,P,O, subject(NewS)).

fix_o_ns(rdf(S,P,O), Old, New) :-
	sub_atom(O,0,Len,After,Old),
	sub_atom(O,Len,After,0, Local),
	atom_concat(New,Local,NewO),
	rdf_update(S,P,O, object(NewO)).

flush_refs_cache_if_needed(Process) :-
	(   rdfs_individual_of(Process, amalgame:'SelectPreLoaded')
	->  flush_refs_cache(Process)
	;   true
	).
flush_deps_if_needed(Strategy, URI, Params) :-
	(   option(default_relation(R), Params), R \= ''
	->  flush_dependent_caches(URI, Strategy)
	;   true
	).
