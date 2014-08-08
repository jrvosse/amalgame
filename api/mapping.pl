:- module(ag_mapping,
	  [
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).

:- use_module(user(user_db)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_evaluation)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/util)).

:- setting(amalgame:rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers:

:- http_handler(amalgame(data/mapping), http_data_mapping, []).
:- http_handler(amalgame(data/evaluate), http_data_evaluate, []).

%%	http_data_mapping(+Request)
%
%	Emit JSON object with mappings for a URL.

http_data_mapping(Request) :-
	setting(amalgame:rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping or evaluation graph')]),
			  strategy(Strategy, [description('URL of strategy')]),
			  sort(SortBy,
			       [default(source),
				oneof([source,target]),
				description('Sort by')]),
			  limit(Limit,
				[default(RowsPerPage), number,
				 description('limit number of mappings returned')]),
			  offset(Offset,
				 [default(0), number,
				  description('first result that is returned')])
		       ]),

	expand_node(Strategy, URL, Mapping0),
	length(Mapping0, Count),
	augment_relations(Strategy, URL, Mapping0, Augmented, []),
	maplist(mapping_label, Augmented, Labeled),
	sort_key(SortBy, SortKey),
	sort_by_arg(Labeled, SortKey, MSorted),
	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_json(MLimit, Mapping),
	node_stats(Strategy, URL, Stats, []),
	reply_json(jsondict{url:URL,
			    limit:Limit,
			    offset:Offset,
			    stats:Stats,
			    total:Count,
			    mapping:Mapping
			   }).

sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SLabel, T,TLabel, Relation)) :-
	skos_notation_ish(S, SLabel),
	skos_notation_ish(T, TLabel),
	append(Prov, FlatProv),
	(   option(relation(Rel), FlatProv)
	->  relation_label(Rel, RLabel),
	    option(comment(Comment), FlatProv, ''),
	    Relation = json([uri=Rel, label=RLabel, comment=Comment])
	;   Relation = null
	).

concept_labels(In, Out) :-
	skos_notation_ish(In.source, SLabel),
	skos_notation_ish(In.target, TLabel),
	Out = c{source:s{uri:In.source, label:SLabel},
		target:t{uri:In.target, label:TLabel}
	       }.

mapping_json([], []).
mapping_json([Align|As], [json(Data)|Os]) :-
	Align = align(Source, SLabel, Target, TLabel, Relation),
	Data = [source=json([uri=Source, label=SLabel]),
		target=json([uri=Target, label=TLabel]),
		relation = Relation
	       ],
	mapping_json(As, Os).

relation_label(R, Label) :-
	mapping_relation(Label, R), !.
relation_label(R, R).


%%	http_data_evaluate(+Request)
%
%	Accept/reject a mapping.

http_data_evaluate(Request) :-
	logged_on(User0, anonymous),
	http_parameters(Request,
			[  new(Values,
				  [description('JSON object with the new source/target pair values')]),
			   originals(Originals,
				     [description('JSON object with original source/target pair')]),
			   relation(Relation,
				    [optional(true),
				     description('Relation between source and target')]),
			   mapping(Mapping,
				   [description('URI of mapping being evaluated')]),
			   strategy(Strategy,
				    [description('Alignment strategy graph')]),
			   comment(Comment,
				   [default(''),
				    description('Explanation of action')]),
			   editmode(EditMode,
				    [default(eval),
				     oneof([eval,edit]),
				     description('Save edits in the same or in the evaluation Graph')
				    ]),
			   remove(Remove,
				  [ default(false),
				    boolean,
				    description('Remove this mapping from the manual evaluation graph')
				  ]),
			   applyTo(ApplyMode,
				[default(one), oneof([all,one]),
				description('Apply to one or all correspondences of this mapping')])
			]),
	(   EditMode == eval
	->  evaluation_graph(Strategy, Mapping, Graph)
	;   Graph = Mapping
	),
	process_entity(EvalProcess,  Graph),
	flush_expand_cache(EvalProcess, Strategy),  % graph cache is now outdated
	flush_refs_cache(Strategy),                 % to recompute all reference stats
	flush_stats_cache(Graph, Strategy),         % to recompute G's basic stats

	user_property(User0, url(User)),

	my_atom_json_dict(Values,    Va, []),
	my_atom_json_dict(Originals, Oa, []),
	concept_labels(Va, V),
	concept_labels(Oa, O),

	BasicOptions = options{user:User,
			       graph:Graph,
			       strategy:Strategy,
			       mapping:Mapping},
	Options = BasicOptions.put(options{
				       comment:Comment,
				       relation:Relation,
				       editmode:EditMode,
				       remove:Remove,
				       applyTo:ApplyMode
				   }),
	(   V=O
	->  original_concepts_assessment(V, JSON, Options)
	;   WithdrawOptions = BasicOptions.put(options{comment:WithDrawComment,
						       relation:Unrelated}),
	    format(atom(WithDrawComment), 'Overruled by ~p ~p ~p',
		   [V.source.uri, Relation, V.target.uri]),
	    rdf_equal(evaluator:unrelated, Unrelated),
	    new_concepts_assessment(V, O, JSON, Options, WithdrawOptions)
	),
	reply_json(JSON).



original_concepts_assessment(V, JSON, Options) :-
	(   option(applyTo(one), Options)
	->  assert_relation(V, JSON, Options)
	;   option(mapping(Mapping), Options),
	    assert_relations(Mapping, JSON, Options)
	).

new_concepts_assessment(V, O, JSON, Options, WithdrawOptions) :-
	assert_relation(O, JSONwith, WithdrawOptions),
	assert_relation(V, JSONadd, Options),
	JSON = JSONwith.put(JSONadd).

assert_relations(Mapping, JSON, Options) :-
	option(strategy(Strategy), Options),
	expand_node(Strategy, Mapping, Mappings),
	forall(member(A, Mappings),
	       assert_relation(A, Options)
	      ),
	JSON = json{status:ok}.

assert_relation(align(S,T,E), Options) :-
	C = correspondence{source:s{uri:S, label:undefined},
			   target:t{uri:T, label:undefined}},
	assert_relation(C, _, Options.put(prov,E)).

assert_relation(C, JSON, Options) :-
	remove_existing_correspondence(C, JSONrm, Options),
	(   option(remove(false), Options, false)
	->  assert_new__correspondence(C, JSONadd, Options)
	;   JSONadd = json{}
	),
	JSON = JSONrm.put(JSONadd).


remove_existing_correspondence(C, JSON, Options) :-
	option(graph(Graph), Options, eval),

	(   has_correspondence(   align(C.source.uri, C.target.uri, Prov), Graph)
	->  remove_correspondence(align(C.source.uri, C.target.uri, Prov), Graph),
	    JSON = json{ remove:C }
	;   JSON = json{ remove:status{status:'nothing removed'}}
	).

assert_new__correspondence(C, JSON, Options) :-
	option(graph(Graph), Options, eval),
	option(relation(Relation), Options),
	option(comment(Comment), Options, ''),
	option(user(User), Options, ''),
	option(prov(Prov), Options, []),

	now_xsd(Now),
	NewProv = [ method(manual_evaluation),
		    user(User),
		    date(Now),
		    comment(Comment),
		    relation(Relation)
		  ],
	AssertOptions = [
			 evidence_graphs(enabled),
			 graph(Graph),
			 prov([NewProv|Prov])
			],
	merge_options(AssertOptions, Options, NewOptions),
	debug(ag_expand, 'assert cell options: ~w', NewOptions),
	assert_cell(C.source.uri, C.target.uri, NewOptions),
	mapping_relation(RLabel, Relation),
	JSON = json{add:correspondence{source:C.source,
				       target:C.target,
				       relation:r{uri:Relation, label:RLabel}
				      }
		   }.


