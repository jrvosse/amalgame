:- module(expand_graph,
	  [
	    expand_node/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/amalgame_modules)).

:- use_module(library(ag_drivers/exec_amalgame_process)).

%%	expand_node(+StrategyURL, +NodeURL, -Result) is det.
%
%	Compute result of expanding NodeURL as defined by StrategyURL.
%	Result is a term defined by the type of output of the
%	components.
%
expand_node(Strategy, Id, Result) :-
	ground(Strategy),
	ground(Id),
	with_mutex(Id, expand_node_(Strategy, Id, Result)).

expand_node_(Strategy, Id, Result) :-
	(   rdfs_individual_of(Id, amalgame:'Mapping')
	->  expand_mapping(Strategy, Id, Result)
	;   rdfs_individual_of(Id, skos:'ConceptScheme')
	->  expand_vocab(Strategy, Id, Result)
	;   Result=error(Id)
	).

%%	expand_mapping(+Strategy, +Id, -Result) is det.
%
%	Generate the Result corresponding to Id.
%	We use a mutex so that the next thread will use the cached
%	version.
%
%	@param Id
%          if Id is a Mapping Result is [align(c1,c2,prov)]
%          if Id is a Vocabulary Result is an assoc or one of
%          scheme(Scheme) or type(Class)

expand_mapping(Strategy, Id, Mapping) :-
	debug(ag_expand, 'Expanding mapping ~p', [Id]),
	(   rdf_graph(Id)
	;   rdfs_individual_of(Id, amalgame:'EvaluatedMapping')
	;   rdfs_individual_of(Id, amalgame:'LoadedMapping')
	;   rdf_has(Id, amalgame:wasGeneratedBy, Process),
	    rdf(Process, rdf:type, amalgame:'SelectPreloaded')
	),
	!,
	findall(C, has_correspondence(C,Id), Mapping0),
	sort(Mapping0, Mapping),
	cache_result(0, Id, Strategy, Mapping).



expand_mapping(Strategy, Id, Mapping) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process,
		   (   expand_process(Strategy, Process, Result, Time),
		       cache_result(Time, Process, Strategy, Result))),
	materialize_results_if_needed(Strategy, Process, Result),
	select_result_mapping(Id, Result, OutputType, Mapping),
	length(Mapping, Count),
	debug(ag_expand, 'Found ~w mappings for ~p', [Count, Id]).



%%	expand_vocab(+Strategy, +Id, -Concepts) is det.
%
%	Generate the Vocab according to Strategy.
%	@param Id is URI of a conceptscheme or an identifier for a set
%	of concepts derived by a vocabulary process,

expand_vocab(Strategy, Id, Vocab) :-
	rdf_has(Id, amalgame:wasGeneratedBy, Process ,OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process,
		   (   expand_process(Strategy, Process, Vocab, Time),
		       cache_result(Time, Process, Strategy, Vocab)
		   )
		  ).

expand_vocab(Strategy, Vocab, Vocab) :-
	rdf_equal(amalgame:preloaded, Preloaded),
	cache_result(0, Preloaded, Strategy, scheme(Vocab)).


%%	expand_process(+Strategy, +Process, -Result, -Time)
%
%	Expand Process according to Strategy to generate Result.

expand_process(Strategy, Process, Result, Time) :-
	ground(Process),
	(   expand_cache(Process-Strategy, Result)
	->  Time = 0,
	    debug(ag_expand, 'Output of process ~p taken from cache', [Process])
	;   do_expand_process(Strategy, Process, Result, Time)
	).

do_expand_process(Strategy, Process, Result, Time) :-
	rdf(Process, rdf:type, Type, Strategy),

	% Do not try multiple types if something fails below...
	!,

	% Collect options and run:
	amalgame_module_id(Type, Module),
	process_options(Process, Module, Options),
	exec_amalgame_process(Type, Process, Strategy,
			      Module, Result, Time, Options),
	debug(ag_expand, 'Output of process ~p (~p) computed in ~ws',
	      [Process,Type,Time]),

	% Provenance admin:
	(   Result = scheme(_)   % Result is a single vocabulary
	->  add_amalgame_opm(Strategy, Process, Result)
	;   findall(URI-Mapping, % Result is one or more mappings
		    (   rdf_has(URI, amalgame:wasGeneratedBy, Process, OutputType),
			rdf(URI, OutputType, Process, Strategy),
			select_result_mapping(URI, Result, OutputType, Mapping)
		    ),
		    Artifacts),
	    add_amalgame_opm(Strategy, Process, Artifacts)
	),

	% HACK: this is needed to get preloaded graphs in the strategy graph
        % because we cannot select a mapping that has not been generated within the
	% builder as an input mapping...
	(   rdfs_subclass_of(Type, amalgame:'SelectPreLoaded')
	->  option(name(PreloadedGraph), Options),
	    rdf_assert(Process, opmv:used, PreloadedGraph, Strategy),
	    rdf_assert(PreloadedGraph, rdf:type, amalgame:'LoadedMapping', Strategy),
	    rdf_assert(PreloadedGraph, amalgame:status, amalgame:imported, Strategy)
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

%%	materialize_if_needed(+Id, Mapping) is det.
%
%	materialize result in Mapping in named graph Id if this graph
%	does not exist yet and if the resource with the same
%	Id has the amalgame:status amalgame:final.

needs_materialization(Id, _, _) :-
	rdf_graph(Id), !, % Already materialized in a prev. run
	fail.
needs_materialization(_Id, Process, _Strategy) :-
	rdfs_individual_of(Process, ProcessType),
	rdf(ProcessType, amalgame:materialize, amalgame:always),
	!.
needs_materialization(Id, _, Strategy) :-
	rdf(Id, amalgame:status, amalgame:final, Strategy).

/*
needs_materialization(Id, _, Strategy) :-
	rdf_has(Id, amalgame:status, Status, Strategy),
	\+ rdf_equal(Status, amalgame:final),
	!, % Not a final graph, no need to materalize.
	fail.
*/
materialize(Id, Mapping) :-
	(   rdf_has(Id, amalgame:recordEvidence, amalgame:enabled)
	->  Enabled = enabled
	;   Enabled = disabled
	),
	% voc_clear_stats(all),
	materialize_mapping_graph(Mapping, [graph(Id), evidence_graphs(Enabled)]).



