:- module(expand_graph,
	  [ expand_node/3,
	    precompute_node/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/amalgame_modules)).

:- use_module(library(ag_drivers/exec_amalgame_process)).

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
	ground(Strategy),
	ground(Id),
	with_mutex(Id, expand_node_(Strategy, Id, Result)).

%%	precompute_node(+Strategy, +Mapping) is det.
%
%	Mapping is precomputed in the background in a separate thread.
%	Subsequent expand_node/3 calls will use the cached results
%	computed here.

precompute_node(Strategy, Mapping) :-
	thread_create( % Write debug output to server console, cannot write to client:
	    (	set_stream(user_output, alias(current_output)),
		expand_node(Strategy, Mapping, _)
	    ),
	    _,[ detached(true) ]).
expand_node_(Strategy, Id, Result) :-
	% Try if we get this result from the expand_cache first:
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	expand_cache(Process-Strategy, ProcessResult),
	!,
	debug(ag_expand, 'Output ~p of process ~p taken from cache',
	      [Id, Process]),

	(   rdfs_individual_of(Id, amalgame:'Mapping')
	->  select_result_mapping(Id, ProcessResult, OutputType, Result)
	;   skos_is_vocabulary(Id)
	->  Result = ProcessResult
	;   Result = error(Id)
	).


expand_node_(Strategy, Id, Result) :-
	% Cache miss, we need to do the work ...
	(   rdfs_individual_of(Id, amalgame:'Mapping')
	->  expand_mapping(Strategy, Id, Result)
	;   skos_is_vocabulary(Id)
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
	    cache_result(0.1, Process, Strategy, MapSpec)
	;   cache_result(0.1, Id, Strategy, Mapping)
	).

expand_mapping(Strategy, Id, Mapping) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process,
		   expand_process(Strategy, Process, Result)),
	materialize_results_if_needed(Strategy, Process, Result),
	select_result_mapping(Id, Result, OutputType, Mapping).

% we should not exploit materialized graphs
% until the evidence is properly serialized ... :-(
exploit_materialized_graph(Strategy, Id) :-
	(   rdf_graph(Id), rdf(Id, amalgame:recordEvidence, amalgame:enabled, Strategy))
	;   rdfs_individual_of(Id, amalgame:'EvaluatedMapping')
	;   rdfs_individual_of(Id, amalgame:'LoadedMapping')
	;   rdf_has(Id, amalgame:wasGeneratedBy, Process),
	    rdf(Process, rdf:type, amalgame:'SelectPreloaded').

%%	expand_vocab(+Strategy, +Id, -Concepts) is det.
%
%	Generate the Vocab according to Strategy.
%	@param Id is URI of a conceptscheme or an identifier for a set
%	of concepts derived by a vocabulary process,

expand_vocab(Strategy, Id, Vocab) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process,
		   expand_process(Strategy, Process, Vocab)).

expand_vocab(Strategy, Vocab, vocspec(alignable(Vocab))) :-
	rdfs_individual_of(Vocab, amalgame:'Alignable'),
	!,
	rdf_equal(amalgame:preloaded, Preloaded),
	cache_result(0, Preloaded, Strategy, vocspec(alignable(Vocab))).

expand_vocab(Strategy, Vocab, vocspec(scheme(Vocab))) :-
	rdf_equal(amalgame:preloaded, Preloaded),
	cache_result(0, Preloaded, Strategy, vocspec(scheme(Vocab))).


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

	cache_result(Time, Process, Strategy, Result),

	% Provenance admin:
	findall(URI, ( rdf_has(URI, amalgame:wasGeneratedBy, Process, OutputType),
		       rdf(URI, OutputType, Process, Strategy)
		     ),
		Artifacts),
	add_amalgame_prov(Strategy, Process, Artifacts),
	preload_hack(Strategy, Process, Type, Options).


preload_hack(Strategy, Process, Type, Options) :-
	% HACK: this is needed to get preloaded graphs in the strategy graph
        % because we cannot select a mapping that has not been generated within the
	% builder as an input mapping...
	(   rdfs_subclass_of(Type, amalgame:'SelectPreLoaded')
	->  option(name(PreloadedGraph), Options),
	    rdf_assert(Process, amalgame:input, PreloadedGraph, Strategy),
	    rdf_assert(PreloadedGraph, rdf:type, amalgame:'LoadedMapping', Strategy)
	    % rdf_assert(PreloadedGraph, amalgame:status, amalgame:imported, Strategy)
	;   true
	).


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
		materialize(Id, Mapping)
	    )
	      ).

needs_materialization(_Id, Process, _Strategy) :-
	rdfs_individual_of(Process, ProcessType),
	rdf(ProcessType, amalgame:materialize, amalgame:always),
	!.

materialize(Id, Mapping) :-
	(   rdf_has(Id, amalgame:recordEvidence, amalgame:enabled)
	->  Enabled = enabled
	;   Enabled = disabled
	),
	% voc_clear_stats(all),
	materialize_mapping_graph(Mapping, [graph(Id), evidence_graphs(Enabled)]).

run_strategy :-
	run_strategy(_).

run_strategy(Strategy) :-
	rdfs_individual_of(Strategy, amalgame:'AlignmentStrategy'),
	findall(M, rdf(M, amalgame:status, _, Strategy), Fs),
	forall(member(F, Fs), expand_node(Strategy, F, _)).
