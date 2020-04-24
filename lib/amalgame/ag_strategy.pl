:- module(ag_strategy,
	  [ % get information about a strategy:
	      strategy_process_entity/3,
	      strategy_vocabulary/2,
	      strategy_vocabularies/2,
	      strategy_languages/2,
	      strategy_entity_status/3,

	    % change strategy
	      strategy_new_process/9,
	      strategy_update_process_parameters/4,
	      strategy_update_node/3,
	      strategy_delete_node/2,
	      strategy_add_vocabulary/2,

	    assert_output/6 % used in strategy_backward_compatability.pl
	  ]).

:- use_module(library(option)).
:- use_module(library(oset)).
:- use_module(library(uri)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/mapping_graph)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(user(preferences)).


:- rdf_meta
	strategy_process_entity(r,r,r),
	strategy_vocabulary(r,r),
	strategy_entity_status(r,r,r),
	strategy_delete_node(r,r),
	strategy_update_node(r,+,r),
	strategy_update_process_parameters(r,r,+,+),
	assert_output(r,r,r,r,r,r),
	new_output(r,r,r,r,r,r),
	output_type(r,r),
	process_entity(r,r,r).

%%	strategy_process_entity(?Strategy, ?Process, ?Entity) is nondet.
%
%	True if Entity is generated by Process in Strategy.

strategy_process_entity(Strategy, Process,Entity) :-
	rdf_has(Entity, amalgame:wasGeneratedBy, Process, RealProperty),
	rdf(Entity, RealProperty, Process, Strategy),
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy').


%%	strategy_entity_status(?Strategy, ?Entity, ?Status) is nondet.
%
%	True if Status is the current status of Entity in Strategy.

strategy_entity_status(Strategy, Entity, Status) :-
	rdf(Entity, amalgame:status, Status, Strategy).

%%	strategy_vocabulary(?Strategy, ?Vocabulary) is nondet.
%
%	True if Vocabulary is included in Strategy.
%

strategy_vocabulary(Strategy, Vocabulary) :-
	rdf(Strategy, amalgame:includes, Vocabulary, Strategy).

%%	strategy_vocabularies(?Strategy, ?Schemes) is nondet.
%
%	Strategy is an amalgame alignment strategy and Schemes are the
%       conceptSchemes that it includes.

strategy_vocabularies(Strategy, Schemes) :-
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy'),
	findall(S, strategy_vocabulary(Strategy, S), Schemes),
	Schemes \== [].

%%	strategy_languages(+Strategy, -Languages) is det.
%
%	Languages is a list of language codes used in the vocabularies
%	of Strategy.

strategy_languages(Strategy, Languages) :-
	strategy_vocabularies(Strategy, Vocs),
	maplist(lang_used(Strategy), Vocs, Langs),
	append(Langs, Languages0),
	sort(Languages0, Languages).

lang_used(Strategy, Voc, Langs) :-
	node_stats(Strategy, Voc, Stats, []),
	option(languages(Langs), Stats).

%%	strategy_add_vocabulary(+Strategy, +Vocabulary) is det.
%
%	Adds Vocabulary as an included scheme in Strategy.

strategy_add_vocabulary(Strategy, Vocabulary) :-
	rdf_assert(Strategy, amalgame:includes, Vocabulary, Strategy),
	rdf_assert(Vocabulary, rdf:type, skos:'ConceptScheme', Strategy).


%%	new_process(+Process, +Strategy, +Source, +Target, +Input,
%%	+SecInputs, +Params, -NewFocus)
%
%	Create new amalgame process.

strategy_new_process(Strategy, Type, Source, Target, Input, SecInputs, Params, Focus, URI) :-
	% hack needed till we have nested rdf transactions:
	retractall(ag_map:nickname_cache(Strategy,_,_)),

	rdf_create_bnode(URI),
	rdf_transaction( % this rdf_transaction is to make it MT safe
	    (	assert_process(URI, Type, Strategy, Params),
		assert_user_provenance(URI, Strategy),
		assert_input(URI, Type, Strategy, Source, Target, Input, Params),
		assert_secondary_inputs(SecInputs, URI, Type, Strategy),
		assert_output(URI, Type, Strategy, Input, SecInputs, Focus)
	    )).
%%	update_process(+Process, +Strategy, +SecInputs, +Params) is det.
%
%	Update the parameters of Process.
strategy_update_process_parameters(Strategy, Process, SecInputs, Params) :-
	uri_query_components(Search, Params),
	rdf(Process, rdf:type, Type, Strategy),
	assert_secondary_inputs(SecInputs, Process, Type, Strategy),
	rdf_transaction((rdf_retractall(Process, amalgame:parameters, _),
			 rdf_assert(Process, amalgame:parameters, Search^^xsd:string, Strategy)
			)).


%%	strategy_update_node(+Strategy, +Properties, +Node) is det.
%
%	Update Properties of Node in Strategy named graph.

strategy_update_node(Strategy, Properties, Node) :-
	rdf_transaction(strategy_update_props(Strategy, Properties, Node)).

strategy_delete_node(Strategy, Node) :-
	rdf_transaction((process_retract(Node, Strategy),
			 node_retract(Node, Strategy)
			)).

assert_output(Process, Type, Graph, Input, _, MainOutput) :-
	rdfs_subclass_of(Type, amalgame:'Partitioner'),
	!,
	output_type(Type, OutputClass),
	new_output(OutputClass, Process, amalgame:selectedBy,  Input, Graph, MainOutput),
	new_output(OutputClass, Process, amalgame:discardedBy, Input, Graph, _),
	new_output(OutputClass, Process, amalgame:undecidedBy, Input, Graph, _).

assert_output(Process, Type, Strategy, Input, SecInputs, Strategy) :-
	rdfs_subclass_of(Type, amalgame:'OverlapComponent'),
	!,
	user_preference(user:lang, literal(Lang)),
	output_type(Type, OutputClass),
	oset_power(SecInputs, [[]|PowSet]),
	forall(member(InSet0, PowSet),
	       (   sort(InSet0, InSet),
		   term_to_atom(InSet, InSetAtom),
		   new_output(OutputClass, Process, amalgame:wasGeneratedBy, Input, Strategy, OutputUri),
		   findall(Nick,
			   (	member(Id, InSet),
				map_nickname(Strategy,Id,Nick)
			   ),
			   Nicks),
		   atomic_list_concat(Nicks, AllNicks),
		   format(atom(Comment), 'Mappings found only in: ~p', [InSet]),
		   format(atom(Label), 'Intersect: ~w', [AllNicks]),
		   rdf_assert(OutputUri, amalgame:overlap_set, InSetAtom^^xsd:string, Strategy),
		   rdf_assert(OutputUri, rdfs:comment, Comment@Lang, Strategy),
		   rdf_assert(OutputUri, rdfs:label, Label@Lang, Strategy)
	       )
	      ).

assert_output(Process, Type, Graph, Input, _, MainOutput) :-
	output_type(Type, OutputClass),
	new_output(OutputClass, Process, amalgame:wasGeneratedBy, Input, Graph, MainOutput).

assert_input(_Process, Type, _Graph, _Source, _Target, _Input, _Params) :-
	rdfs_subclass_of(Type, amalgame:'MultiInputComponent'),
	!.
assert_input(Process, Type, Graph, Source, Target, _Input, Params) :-
	nonvar(Source),
	nonvar(Target),
	!,
	rdf_assert(Process, amalgame:source, Source, Graph),
	rdf_assert(Process, amalgame:target, Target, Graph),
	assert_preloaded_input(Process, Type, Graph, Params).
assert_input(Process, Type, Graph, _Source, _Target, Input, Params) :-
	nonvar(Input),
	!,
	rdf_assert(Process, amalgame:input, Input, Graph),
	assert_preloaded_input(Process, Type, Graph, Params).

assert_preloaded_input(Process, Type, Graph, Params) :-
	(   rdfs_subclass_of(Type, amalgame:'SelectPreLoaded'),
	    option(name(Name), Params)
	->  rdf_assert(Process, amalgame:input, Name, Graph),
	    rdf_assert(Name, amalgame:status, amalgame:reference, Graph),
	    rdf_assert(Name, rdf:type, amalgame:'LoadedMapping', Graph)
	;   true
	).

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
	process_label_literal(Type, Label),
	uri_query_components(Search, Params),
	rdf_assert(Process, rdf:type, Type, Graph),
	rdf_assert(Process, rdfs:label, Label, Graph),
	rdf_assert(Process, amalgame:parameters, Search^^xsd:string, Graph).

new_output(Type, Process, P, Input, Strategy, OutputURI) :-
	mint_node_uri(Strategy, dataset, OutputURI),
	rdf_assert(OutputURI, rdf:type, Type, Strategy),
	rdf_assert(OutputURI, amalgame:status, amalgame:intermediate, Strategy),
        rdf_assert(OutputURI, P, Process, Strategy),

	rdfs_individual_of(Process, PType),
	!,
	(   rdf_has(PType, amalgame:materialize, amalgame:always)
	->  rdf_assert(OutputURI, amalgame:recordEvidence, amalgame:enabled, Strategy)
	;   true
	),

	assert_relation(OutputURI, Input, Strategy),
	map_nickname(Strategy, OutputURI, _Nick).

assert_relation(Output, Input, Strategy) :-
	nonvar(Input),
	rdf(Input, amalgame:default_relation, Relation, Strategy),
	rdf_assert(Output, amalgame:default_relation, Relation, Strategy),
	!.

assert_relation(_,_,_).

output_type(ProcessType, amalgame:'VirtualConceptScheme') :-
	rdfs_subclass_of(ProcessType, amalgame:'VocabPartitioner'),
	!.
output_type(_ProcessType, amalgame:'Mapping').

process_label_literal(P, Lit) :-
	(   rdf_label(P, Lit)
	->  true
	;   rdf_global_id(_:Local, P),
	    Lit = literal(Local)
	).

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

strategy_update_props(_, [], _).
strategy_update_props(Strategy, [T|Ts], URI) :-
	update_node_prop(T, URI, Strategy),
	!,
	strategy_update_props(Strategy, Ts, URI).
strategy_update_props(Strategy, [_|Ts], URI) :-
	strategy_update_props(Strategy, Ts, URI).


update_node_prop(label=Label, URI, Strategy) :-
	user_preference(user:lang, literal(Lang)),
	rdf_retractall(URI, rdfs:label, _, Strategy),
	(   Label == ''
	->  true
	;   rdf_assert(URI, rdfs:label, Label@Lang, Strategy)
	).

update_node_prop(abbrev=Abbrev, URI, Strategy) :-
	rdf_retractall(URI, amalgame:nickname, _, Strategy),
	(   Abbrev == ''
	->  true
	;   rdf_assert(URI, amalgame:nickname, Abbrev^^xsd:string, Strategy)
	).

update_node_prop(comment=Comment, URI, Strategy) :-
	user_preference(user:lang, literal(Lang)),
	rdf_retractall(URI, rdfs:comment, _, Strategy),
	(   Comment == ''
	->  true
	;   rdf_assert(URI, rdfs:comment, Comment@Lang, Strategy)
	).
update_node_prop(status=Status, URI, Strategy) :-
	rdf_retractall(URI, amalgame:status, _, Strategy),
	(   Status == ''
	->  true
	;   rdf_assert(URI, amalgame:status, Status, Strategy)
	).

update_node_prop(default_relation=Relation, URI, Strategy) :-
	rdf_retractall(URI, amalgame:default_relation, _, Strategy),
	(   Relation == ''
	->  true
	;   rdf_assert(URI, amalgame:default_relation, Relation, Strategy)
	).

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
