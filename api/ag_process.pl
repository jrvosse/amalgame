:- module(ag_process,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).

:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/map)).

:- setting(amalgame:precompute_mapping, boolean, true,
	   'When true mappings are computed in the background').

:- http_handler(amalgame(data/addprocess), http_add_process, []).
:- http_handler(amalgame(data/updatenode), http_update_node, []).
:- http_handler(amalgame(data/deletenode), http_delete_node, []).

:- rdf_meta
	new_output(r,r,r,r,r,r),
	output_type(r,r).

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
			  alignment(Strategy,
				    [uri,
				     description('URI of the alignment graph to which the process is added')]),
			  update(Update,
				 [boolean, default(false),
				  descrption('When set to true process is updated with new parameters')])
			],
			[form_data(Params0)]),
	sort(SecInputs0, SecInputs),
	subtract(Params0, [input=_,source=_,target=_,process=_,alignment=_,update=_,graphic=_], Params1),
	findall(secondary_input=S,member(secondary_input=S, Params1), SecParams),
	subtract(Params1, SecParams, Params),
	fix_not_expanded_options(Params, ExpandedParams),
	(   Update == true
	->  rdf_retractall(Process, amalgame:secondary_input, _, Strategy),
	    update_process(Process, Strategy, SecInputs, ExpandedParams),
	    Focus = Process
	;   ((nonvar(Source), nonvar(Target)) ; nonvar(Input))
	->  new_process(Process, Strategy, Source, Target, Input, SecInputs, ExpandedParams, Focus)
	;   true
	),
	js_focus_node(Strategy, Focus, FocusNode),
	js_alignment_nodes(Strategy, Nodes),
	reply_json(json([focus=json(FocusNode),
			 nodes=json(Nodes)])).


%%	http_update_node(+Request)
%
%	Change properties of a URI in Strategy and return the new
%	nodes in Strategy.

http_update_node(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Strategy,
				    [uri,
				     description('URI of alignment')
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
	rdf_transaction(update_node_props(Params, URI, Strategy)),
	update_amalgame_prov(Strategy, URI),
	change_ns_if_needed(PublishNS, URI, Strategy, NewStrategy),
	js_alignment_nodes(NewStrategy, Nodes),
	js_focus_node(NewStrategy, URI, FocusNode),

	reply_json(json([alignment=NewStrategy,
			 nodes=json(Nodes),
			 focus=json(FocusNode)
			])),

	% precompute results to speed things up
	(   setting(amalgame:precompute_mapping, true)
	->  precompute_mapping(Strategy, URI)
	;   true).

%%	update_process(+Process, +Strategy, +SecInputs, +Params)
%
%	Update the parameters of Process.
update_process(Process, Strategy, SecInputs, Params) :-
	flush_dependent_caches(Process, Strategy),
	uri_query_components(Search, Params),
	rdf(Process, rdf:type, Type, Strategy),
	assert_secondary_inputs(SecInputs, Process, Type, Strategy),
	rdf_transaction((rdf_retractall(Process, amalgame:parameters, _),
			 rdf_assert(Process, amalgame:parameters, literal(Search), Strategy)
			)).

is_dependent_chk(Mapping, Process, Strategy) :-
	rdf_has(Mapping, amalgame:wasGeneratedBy, Process, RP),
	rdf(Mapping, RP, Process, Strategy),
	!.
is_dependent_chk(Mapping, Process, Strategy) :-
	rdf_has(Mapping, amalgame:wasGeneratedBy, OtherProcess, RP1),
	rdf(Mapping, RP1, OtherProcess, Strategy),
	rdf_has(OtherProcess, amalgame:input, OtherMapping, RP2),
	rdf(OtherProcess, RP2, OtherMapping, Strategy),
	is_dependent_chk(OtherMapping, Process, Strategy),!.


%%	new_process(+Process, +Strategy, +Source, +Target, +Input,
%%	+SecInputs, +Params, -NewFocus)
%
%	Create new amalgame process.

new_process(Type, Strategy, Source, Target, Input, SecInputs, Params, Focus) :-
	% hack needed till we have nested rdf transactions:
	retractall(ag_alignment:nickname_cache(Strategy,_,_)),

	rdf_bnode(URI),
	rdf_transaction( % this rdf_transaction is to make it MT safe
	    (	assert_process(URI, Type, Strategy, Params),
		assert_user_provenance(URI, Strategy),
		assert_input(URI, Type, Strategy, Source, Target, Input),
		assert_secondary_inputs(SecInputs, URI, Type, Strategy),
		assert_output(URI, Type, Strategy, Input, SecInputs, Focus)
	    )),

	% precompute results to speed things up
	(   setting(amalgame:precompute_mapping, true)
	->  precompute_process(Strategy, URI)
	;   true).

precompute_process(Strategy, Process) :-
	rdf_has(Mapping, amalgame:wasGeneratedBy, Process, RP),
	rdf(Mapping, RP, Process, Strategy),
	precompute_mapping(Strategy, Mapping).

precompute_mapping(Strategy, Mapping) :-
	thread_create( % Write debug output to server console, cannot write to client:
	    (	set_stream(user_output, alias(current_output)),
		expand_node(Strategy, Mapping, _)
	    ),
	    _,[ detached(true) ]).

assert_input(_Process, Type, _Graph, _Source, _Target, _Input) :-
	rdfs_subclass_of(Type, amalgame:'MultiInputComponent'),
	!.

assert_input(Process, _Type, Graph, Source, Target, _Input) :-
	nonvar(Source),
	nonvar(Target),
	!,
	rdf_assert(Process, amalgame:source, Source, Graph),
	rdf_assert(Process, amalgame:target, Target, Graph).
assert_input(Process, _Type, Graph, _Source, _Target, Input) :-
	rdf_assert(Process, amalgame:input, Input, Graph).

assert_secondary_inputs([], _, _, _).
assert_secondary_inputs([URI|URIs], Process, Type, Strategy) :-
	(   rdfs_subclass_of(Type, amalgame:'SetOperator')
	->  rdf_equal(Pred, amalgame:input)
	;   rdf_equal(Pred, amalgame:secondary_input)
	),
	(   is_dependent_chk(URI, Process, Strategy)
	->  debug(eq, 'Not adding secondary input ~p, it will lead to cyclic dependency on process ~p', [URI, Process])
	;   rdf_assert(Process, Pred, URI, Strategy)
	),
	assert_secondary_inputs(URIs, Process, Type, Strategy).

assert_process(Process, Type, Graph, Params) :-
	process_label(Type, Label),
	uri_query_components(Search, Params),
	rdf_assert(Process, rdf:type, Type, Graph),
	rdf_assert(Process, rdfs:label, literal(Label), Graph),
	rdf_assert(Process, amalgame:parameters, literal(Search), Graph).

assert_output(Process, Type, Graph, Input, _, MainOutput) :-
	rdfs_subclass_of(Type, amalgame:'MappingSelecter'),
	!,
	rdf_equal(amalgame:'Mapping', OutputClass),
	new_output(OutputClass, Process, amalgame:selectedBy,  Input, Graph, MainOutput),
	new_output(OutputClass, Process, amalgame:discardedBy, Input, Graph, _),
	new_output(OutputClass, Process, amalgame:undecidedBy, Input, Graph, _).

assert_output(Process, Type, Strategy, Input, SecInputs, Strategy) :-
	rdfs_subclass_of(Type, amalgame:'OverlapComponent'),
	!,
	rdf_equal(amalgame:'Mapping', OutputClass),
	oset_power(SecInputs, [[]|PowSet]),
	forall(member(InSet0, PowSet),
	       (   sort(InSet0, InSet),
		   term_to_atom(InSet, InSetAtom),
		   new_output(OutputClass, Process, amalgame:wasGeneratedBy, Input, Strategy, OutputUri),
		   findall(Nick,
			   (	member(Id, InSet),
				nickname(Strategy,Id,Nick)
			   ),
			   Nicks),
		   atomic_list_concat(Nicks, AllNicks),
		   format(atom(Comment), 'Mappings found only in: ~p', [InSet]),
		   format(atom(Label), 'Intersect: ~w', [AllNicks]),
		   rdf_assert(OutputUri, amalgame:overlap_set, literal(InSetAtom), Strategy),
		   rdf_assert(OutputUri, rdfs:comment, literal(Comment), Strategy),
		   rdf_assert(OutputUri, rdfs:label, literal(Label), Strategy)
	       )
	      ).


assert_output(Process, Type, Graph, Input, _, MainOutput) :-
	output_type(Type, OutputClass),
	new_output(OutputClass, Process, amalgame:wasGeneratedBy, Input, Graph, MainOutput).

new_output(Type, Process, P, Input, Strategy, OutputURI) :-
	mint_node_uri(Strategy, dataset, OutputURI),
	rdf_assert(OutputURI, rdf:type, Type, Strategy),
	rdf_assert(OutputURI, amalgame:status, amalgame:intermediate, Strategy),
        rdf_assert(OutputURI, P, Process, Strategy),

	rdfs_individual_of(Process, PType),
	(   rdf_has(PType, amalgame:materialize, amalgame:always)
	->  rdf_assert(OutputURI, amalgame:recordEvidence, amalgame:enabled, Strategy)
	;   true
	),

	assert_relation(OutputURI, Input, Strategy),
	nickname(Strategy, OutputURI, _Nick).

assert_relation(Output, Input, Strategy) :-
	nonvar(Input),
	rdf(Input, amalgame:default_relation, Relation, Strategy),
	rdf_assert(Output, amalgame:default_relation, Relation, Strategy),
	!.

assert_relation(_,_,_).

output_type(ProcessType, skos:'ConceptScheme') :-
	rdfs_subclass_of(ProcessType, amalgame:'VocabSelecter'),
	!.
output_type(_ProcessType, amalgame:'Mapping').


process_label(P, Lit) :-
	(   rdf_display_label(P, L)
	->  Lit = L
	;   rdf_global_id(_:Local, P),
	    Lit = literal(Local)
	).




update_node_props([], _, _).
update_node_props([T|Ts], URI, Strategy) :-
	update_node_prop(T, URI, Strategy),
	!,
	update_node_props(Ts, URI, Strategy).
update_node_props([_|Ts], URI, Strategy) :-
	update_node_props(Ts, URI, Strategy).


update_node_prop(label=Label, URI, Strategy) :-
	rdf_retractall(URI, rdfs:label, _, Strategy),
	(   Label == ''
	->  true
	;   rdf_assert(URI, rdfs:label, literal(Label), Strategy)
	).

update_node_prop(abbrev=Abbrev, URI, Strategy) :-
	rdf_retractall(URI, amalgame:nickname, _, Strategy),
	(   Abbrev == ''
	->  true
	;   rdf_assert(URI, amalgame:nickname, literal(Abbrev), Strategy)
	).

update_node_prop(comment=Comment, URI, Strategy) :-
	rdf_retractall(URI, rdfs:comment, _, Strategy),
	(   Comment == ''
	->  true
	;   rdf_assert(URI, rdfs:comment, literal(Comment), Strategy)
	).
update_node_prop(status=Status, URI, Strategy) :-
	rdf_retractall(URI, amalgame:status, _, Strategy),
	(   Status == ''
	->  true
	;   rdf_assert(URI, amalgame:status, Status, Strategy)
	).

update_node_prop(default_relation=Relation, URI, Strategy) :-
	rdf_retractall(URI, amalgame:default_relation, _, Strategy),
	flush_dependent_caches(URI, Strategy),
	(   Relation == ''
	->  true
	;   rdf_assert(URI, amalgame:default_relation, Relation, Strategy)
	).


change_ns_if_needed(NS, URI, Strategy, NewStrategy) :-
	rdf(Strategy, amalgame:publish_ns, OldNS, Strategy),
	(   (OldNS == NS; NS == 'same')
	->  NewStrategy = Strategy
	;   provenance_graph(Strategy, Prov),
	    rdf_unload_graph(Prov),
	    rdf_retractall(URI, amalgame:publish_ns, OldNS, Strategy),
	    rdf_assert(URI, amalgame:publish_ns, NS, Strategy),
	    flush_stats_cache(Strategy),
	    flush_expand_cache(Strategy),
	    change_namespace(OldNS, NS, Strategy, NewStrategy)
	).

%%	http_delete_node(+Request)
%
%	delete URI in Strategy and all that are connected to it and
%	return the new nodes in Strategy.

http_delete_node(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Strategy,
				    [uri,
				     description('URI of alignment')
				    ]),
			  uri(URI,
				[uri,
				 description('URI of input resource')])
			]),
	rdf_transaction((process_retract(URI, Strategy),
			 node_retract(URI, Strategy)
			)),
	js_alignment_nodes(Strategy, Nodes),
	js_focus_node(Strategy, Strategy, FocusNode),
	reply_json(json([nodes=json(Nodes),
			 focus=json(FocusNode)
			])).

node_retract(URI, Strategy) :-
	provenance_graph(Strategy, ProvGraph),
	rdf_retractall(URI, _, _, Strategy),
	rdf_retractall(URI, _, _, ProvGraph),
	forall(rdf(Subj,_,URI,Strategy),
	       node_retract(Subj, Strategy)).

process_retract(URI, Strategy) :-
	rdf_has(URI, amalgame:wasGeneratedBy, P),
	findall(S, rdf_has(S, amalgame:wasGeneratedBy, P), [URI]),
	provenance_graph(Strategy, ProvGraph),
	!,
	rdf_retractall(P, _, _, Strategy),
	rdf_retractall(P, _, _, ProvGraph).
process_retract(_, _).




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

fix_not_expanded_options([''],[]).
fix_not_expanded_options([],[]).
fix_not_expanded_options([Key=Value|Tail], [Key=FixedValue|Results]):-
	(   \+ sub_atom(Value,0,_,_,'http:'),
	    atomic_list_concat([NS,L], :, Value),
	    rdf_global_id(NS:L,FixedValue)
	->  true
	;   FixedValue = Value
	),
	fix_not_expanded_options(Tail, Results).
