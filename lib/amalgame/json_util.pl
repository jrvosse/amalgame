:- module(ag_json_util,
	  [   js_mappings_metadata/3,
	      js_focus_node/3,
	      js_strategy_nodes/2
	  ]).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(cliopatria(components/label)).

:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/mapping_graph)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/ag_reference)).
:- use_module(library(amalgame/ag_evaluation)).

js_mappings_metadata(Strategy, Results, Options) :-
	amalgame_strategy_mappings(Strategy, Mappings, Options),
	maplist(mapping_metadata(Strategy), Mappings, Pairs),
	dict_pairs(Results, mappings, Pairs).

mapping_metadata(Strategy, M, M-Dict) :-
	Dict = mapping{uri:M, label:L, stats:Stats, agStatus:Status},
	(   node_stats(Strategy, M, Stats, [compute(false)]),
	    Stats.totalCount > 0
	->  true
	;   Stats = _{}
	),
	is_dict(Stats, mapping_stats_dict),
	(   rdf_has(M, amalgame:status, StatusQ)
	->  rdf_global_id(_:Status, StatusQ)
	;     Status = undefined
	),
	rdf_display_label(M, L),!.

mapping_metadata(Strategy, M, _) :-
	debug(js_mappings_metadata, 'mapping_metadata failed for ~p/~p', [Strategy, M]),
	fail.

%%	js_focus_node(+Strategy, +URI, -NodeProps)
%
%	NodeProps contains the currently accessible properties for URI

js_focus_node(Strategy, URI, NodeProps) :-
	findall(Type-Value, node_prop(Strategy, URI, Type, Value), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key_if_needed(Sorted, Grouped),
	dict_pairs(NodeProps, node, Grouped).

group_pairs_by_key_if_needed([], []).
group_pairs_by_key_if_needed([M-N|T0], [M-Result|T]) :-
	same_key(M, T0, TN, T1),
	(   TN == []
	->  Result = N
	;   Result = [N|TN]
	),
	group_pairs_by_key_if_needed(T1, T).

same_key(M, [M-N|T0], [N|TN], T) :- !,
	same_key(M, T0, TN, T).
same_key(_, L, [], L).

%%	js_strategy_nodes(+Strategy, -Nodes)
%
%	Nodes contains all nodes in alignment Strategy with their type
%	(process, vocab, strategy or mapping).

js_strategy_nodes(Strategy, Nodes) :-
	findall(S, graph_resource(Strategy, S), NodeURIs),
	sort(NodeURIs, URIsUnique),
	maplist(node_data(Strategy), URIsUnique, Pairs),
	dict_pairs(Nodes, nodes, Pairs).

amalgame_strategy_mappings(Strategy, Mappings, Options) :-
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy'),
	findall(URI, (rdf(URI, rdf:type, _ ,Strategy),
		      rdfs_individual_of(URI, amalgame:'Mapping'),
		      ag_map_filter(Strategy, URI, Options)
		     ), Mappings).

ag_map_filter(Strategy, M, Options) :-
	option(references(exclude), Options),
	is_reference_mapping(Strategy, M),
	!, fail.
ag_map_filter(Strategy, M, Options) :-
	option(references(only), Options),
	\+ is_reference_mapping(Strategy, M),
	!, fail.
ag_map_filter(_,_,_).




graph_resource(Graph, R) :-
	rdf(R,rdf:type,_,Graph),
	\+ is_empty_eval_graph(R).
graph_resource(Graph, R) :-
	rdf(_,amalgame:source,R,Graph).
graph_resource(Graph, R) :-
	rdf(_,amalgame:target,R,Graph).
graph_resource(Graph, R) :-
	strategy_vocabulary(Graph, R).

node_data(Strategy, R, R-Props) :-
	findall(Type-Value, node_prop(Strategy, R, Type, Value), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key_if_needed(Sorted, Grouped),
	dict_pairs(Props, node, Grouped).

node_prop(_, R, uri, R).
node_prop(S, R, label, Label) :-
	(   rdf(R, rdfs:label, Lit, S) % use label defined in strategy by user!
	->  true
	;   rdf_display_label(R, Lit)
	),
	literal_text(Lit, Label).
node_prop(S, R, stats, Censored) :-
	node_stats(S,R,Stats, [compute(false)]),
	select_dict(_{'@private':_, '@properties':_}, Stats, Censored).

node_prop(_S, R, type, Type) :-
	(   rdfs_individual_of(R, amalgame:'AlignmentStrategy')
	->  Type = strategy
	;   rdfs_individual_of(R, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_individual_of(R, amalgame:'Process')
	->  Type = process
	;   Type = vocab
	).
node_prop(S, R, secondary_inputs, Inputs) :-
	findall(I,
		(   rdf_has(R, amalgame:secondary_input, I, RP),
		    rdf(R, RP, I, S)

		), Inputs).
node_prop(S, R, local, Local) :-
	map_localname(S,R,Local).
node_prop(S, R, status, Status) :-
	rdf(R, amalgame:status, Status, S).
node_prop(S, R, default_relation, Relation) :-
	rdf(R, amalgame:default_relation, Relation, S).
node_prop(S, R, comment, Comment) :-
	rdf(R, rdfs:comment, Lit, S),
	literal_text(Lit, Comment).
node_prop(_, R, link, Link) :-
	resource_link(R, Link).
node_prop(S, R, abbrev, Nick) :-
	rdfs_individual_of(R, amalgame:'Mapping'),
	map_nickname(S,R,Nick).
node_prop(S, R, namespace, NS) :-
	rdf(R, amalgame:publish_ns, NS, S).
