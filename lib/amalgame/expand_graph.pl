:- module(expand_graph,
	  [ expand_node/3,
	    vocab_spec/3,
	    precompute_process/2,
	    precompute_node/2,
	    all_mapped/5
	  ]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/correspondence)).
:- use_module(library(amalgame/mapping_graph)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/amalgame_modules)).

:- use_module(library(ag_drivers/exec_amalgame_process)).

:- table expand_node/3 as shared.

%%	expand_node(+StrategyURL, +NodeURL, -Result) is det.
%
%	Compute result of expanding NodeURL as defined by StrategyURL.
%	Result is a term defined by the type of output of the
%	components.
%
%	@param Id
%          if Id is a mapping, Result is [align(c1,c2,evidence)]
%          if Id is a Vocabulary Result is a vocspec/1 term that can
%          be used as the second argument to vocab_member/2.

expand_node(Strategy, Id, Result) :-
	ground(Id),
	atomic_concat(expand_node, Id, Mutex),
	debug(mutex, 'Locking mutex: ~w', [Mutex]),
	with_mutex(Mutex, expand_node_(Strategy, Id, Result)),
	debug(mutex, 'Releasing mutex: ~w', [Mutex]).

%%	precompute_process(+Strategy, +Process) is det.
%
%	Process is precomputed in the background in a separate thread.
%	Subsequent expand_node/3 calls will use the cached results
%	computed here.
precompute_process(Strategy, Process) :-
	rdf_has(Mapping, amalgame:wasGeneratedBy, Process, RP),
	rdf(Mapping, RP, Process, Strategy),
	precompute_node(Strategy, Mapping).

%%	precompute_node(+Strategy, +Mapping) is det.
%
%	Mapping is precomputed in the background in a separate thread.
%	Subsequent expand_node/3 calls will use the cached results
%	computed here.

precompute_node(Strategy, Mapping) :-
	debug(ag_expand, 'Precomputing ~p', [Mapping]),
	thread_create( % Write debug output to server console, cannot write to client:
	    (	set_stream(user_output, alias(current_output)),
		expand_node(Strategy, Mapping, _)
	    ),
	    _,[ detached(true) ]).

%%	all_mapped(+Strategy, +Type, +Mapping, -Concepts, -Sorted) is
%	semidet.
%
%	True if Concepts are all sources/targets in the correspondences
%	of Mapping. Type is either source or target.
all_mapped(Strategy, Type, Mapping, Concepts, Sorted) :-
	atom(Mapping),
	(   cache_mapped_concepts(Strategy, Type, Mapping, Concepts)
	->  assoc_to_keys(Concepts, Sorted)
	;   expand_node(Strategy, Mapping, Result),
	    maplist(my_correspondence_element(Type), Result, ConceptsPairs),
	    sort(ConceptsPairs, SortedPairs),
	    ord_list_to_assoc(SortedPairs, Concepts),
	    assoc_to_keys(Concepts, Sorted),
	    cache_mapped_concepts(Strategy, Type, Mapping, Concepts)
	).

all_mapped(Strategy, Type, Mappings, Concepts, Sorted) :-
	is_list(Mappings),
	(   cache_mapped_concepts(Strategy, Type, Mappings, Concepts)
	->  assoc_to_keys(Concepts, Sorted)
	;   maplist(expand_node(Strategy), Mappings, Results),
	    append(Results, Result),
	    maplist(my_correspondence_element(Type), Result, ConceptPairs),
	    sort(ConceptPairs, SortedPairs),
	    ord_list_to_assoc(SortedPairs, Concepts),
	    assoc_to_keys(Concepts, Sorted),
	    cache_mapped_concepts(Strategy, Type, Mappings, Concepts)
	).

my_correspondence_element(Type, Align3, E-t) :-
	correspondence_element(Type, Align3, E).

expand_node_(Strategy, Id, Result) :-
	% Try if we get this result from the expand_cache (on node id) first:
	get_expand_cache(Strategy, Id, Result),!.

expand_node_(Strategy, Id, Result) :-
	% Try if we get this result from the expand_cache (on process id):
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	get_expand_cache(Strategy, Process, ProcessResult),
	!,
	debug(ag_expand, 'Output ~p of process ~p taken from cache',
	      [Id, Process]),

	(   rdfs_individual_of(Id, amalgame:'Mapping')
	->  select_result_mapping(Id, ProcessResult, OutputType, Result)
	;   amalgame_alignable_scheme(Id)
	->  select_result_scheme(Id, ProcessResult, OutputType, Result)
	;   Result = error(Id)
	).


expand_node_(Strategy, Id, Result) :-
	% Cache miss, we need to do the work ...
	(   rdfs_individual_of(Id, amalgame:'Mapping')
	->  expand_mapping(Strategy, Id, Result)
	;   amalgame_alignable_scheme(Id)
	->  expand_vocab(Strategy, Id, Result)
	;   Result=error(Id)
	).

%%	expand_mapping(+Strategy, +Id, -Result) is det.
%
%	Generate the Result corresponding to mapping Id.
%

expand_mapping(Strategy, Id, Mapping) :-
	exploit_materialized_graph(Strategy, Id),
	!,
	debug(ag_expand, 'Using & caching already materialized mapping ~p', [Id]),

	findall(C, has_correspondence(C,Id), Mapping0),
	sort(Mapping0, Mapping),

	(   rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	    rdf(Id, OutputType, Process, Strategy)
	->  select_result_mapping(Id, MapSpec, OutputType, Mapping),
	    cache_result_stats(Process, Strategy, MapSpec)
	    % cache_result(0.1, Process, Strategy, MapSpec)
	;   mapping_stats(Id, Mapping, Strategy, MapStats),
	    set_stats_cache(Strategy, Id,  MapStats)
	    % cache_result(0.1, Id, Strategy, Mapping)
	).

expand_mapping(Strategy, Id, Mapping) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	atom_concat(expand_process, Process, Mutex),
	debug(mutex, 'Locking mutex: ~w', [Mutex]),
	with_mutex(Mutex,
		   expand_process(Strategy, Process, Result)),
	debug(mutex, 'Releasing mutex: ~w', [Mutex]),

	materialize_results_if_needed(Strategy, Process, Result), % only for sampler ...
	select_result_mapping(Id, Result, OutputType, Mapping).

% we should not exploit materialized graphs
% until the evidence is properly serialized ... :-(
exploit_materialized_graph(Strategy, Id) :-
	(   rdf_graph(Id), rdf(Id, amalgame:recordEvidence, amalgame:enabled, Strategy))
	;   rdfs_individual_of(Id, amalgame:'EvaluatedMapping')
	;   rdfs_individual_of(Id, amalgame:'LoadedMapping')
	;   rdf_has(Id, amalgame:wasGeneratedBy, Process),
	    rdf(Process, rdf:type, amalgame:'SelectPreloaded').

%%	expand_vocab(+Strategy, +Id, -Vocspec) is det.
%
%	Generate the Vocsped according to Strategy.
%	@param Id is URI of a conceptscheme or an identifier for a set
%	of concepts derived by a vocabulary process,
%	@param VocSpec is a specification of the concept scheme.

expand_vocab(Strategy, Id, Concepts) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	atomic_concat(expand_process, Process, Mutex),
	debug(mutex, 'Locking mutex: ~w', [Mutex]),
	with_mutex(Mutex,
		   expand_process(Strategy, Process, Result)),
	select_result_scheme(Id, Result, OutputType, Concepts),
	debug(mutex, 'Releasing mutex: ~w', [Mutex]).

expand_vocab(Strategy, Vocab, Assoc) :-
	findall(C-t, skos_in_scheme(Vocab, C), Pairs),
	debug(ag_expand, 'Concepts of ~p computed and cached', [Vocab]),
	sort(Pairs, Sorted),
	ord_list_to_assoc(Sorted,Assoc),
	handle_scheme_stats(Strategy, _Process, Vocab, Assoc).

	% cache_result(_, Vocab, Strategy, Assoc).

vocab_spec(Strategy, Id, Spec) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
		rdf(Id, OutputType, Process, Strategy),
	!,
	expand_specification(Strategy, Process, Result),
	select_result_scheme(Id, Result, OutputType, Spec).

vocab_spec(_Strategy, Id, rscheme(Id)).

expand_specification(Strategy, Process, Specification) :-
	ground(Process),
	rdf(Process, rdf:type, Type, Strategy),

	% Do not try multiple types if something fails below...
	!,

	% Collect options and run:
	amalgame_module_id(Type, Module),
	process_options(Process, Module, Options),
	specification(Type, Process, Strategy, Module,
			      Specification, _Time, Options).


%%	expand_process(+Strategy, +Process, -Result, -Time)
%
%	Expand Process according to Strategy to generate Result.

expand_process(Strategy, Process, Result) :-
	ground(Process),
	rdf(Process, rdf:type, Type, Strategy),

	% Do not try multiple types if something fails below...
	!,

	% Collect options and run:
	amalgame_module_id(Type, Module),
	process_options(Process, Module, Options),
	exec_amalgame_process(Type, Process, Strategy,
			      Module, Result, Time, Options),
	(   ground(Result)
	->  debug(ag_expand, 'Output of process ~p (~p) computed in ~ws',
		  [Process,Type,Time])
	;   throw(error(instantiation_error, 'Mappings results not grounded'))
	),


	cache_result_stats(Process, Strategy, Result),
	% cache_result(Time, Process, Strategy, Result),

	% Provenance admin:
	findall(URI, ( rdf_has(URI, amalgame:wasGeneratedBy, Process, OutputType),
		       rdf(URI, OutputType, Process, Strategy)
		     ),
		Artifacts),
	add_amalgame_prov(Strategy, Process, Artifacts).

materialize_results_if_needed(Strategy, Process, Results) :-
	findall(Id-RP,
		(   rdf_has(Id, amalgame:wasGeneratedBy, Process, RP),
		    rdf(Id, RP, Process, Strategy)
		),
		Ids),
	forall(
	    (	member(Id-P, Ids),
		needs_materialization(Id, Process, Strategy)
	    ),
	    (   select_result_mapping(Id, Results, P, Mapping),
		materialize(Strategy, Id, Mapping)
	    )
	).

needs_materialization(_Id, Process, _Strategy) :-
	rdfs_individual_of(Process, ProcessType),
	rdf(ProcessType, amalgame:materialize, amalgame:always),
	!.

materialize(Strategy, Id, Mapping) :-
	(   rdf_has(Id, amalgame:recordEvidence, amalgame:enabled)
	->  Enabled = enabled
	;   Enabled = disabled
	),
	% voc_clear_stats(all),
	materialize_mapping_graph(Strategy, Mapping, [graph(Id), evidence_graphs(Enabled)]).

run_strategy :-
	run_strategy(_).

run_strategy(Strategy) :-
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy'),
	findall(M, rdf(M, amalgame:status, _, Strategy), Fs),
	forall(member(F, Fs), expand_node(Strategy, F, _)).
