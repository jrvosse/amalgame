:- module(expand_graph,
	  [ expand_mapping/4,
	    expand_vocab/4,
	    expand_process/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
% :- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/opm)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/amalgame_modules)).

:- use_module(library(skos/vocabularies)).
:- use_module(library(ag_drivers/exec_amalgame_process)).

%%	expand_mapping(+Strategy, +Id, -Result, -Stats) is det.
%
%	Generate the Result corresponding to Id.
%	We use a mutex so that the next thread will use the cached
%	version.
%
%	@param Id
%          if Id is a Mapping Result is [align(c1,c2,prov)]
%          if Id is a Vocabulary Result is an assoc or one of
%          scheme(Scheme) or type(Class)

expand_mapping(Strategy, Id, Mapping, Stats) :-
	(   rdf_graph(Id)
	;   rdfs_individual_of(Id, amalgame:'EvaluatedMapping')
	%;   rdfs_individual_of(Id, amalgame:'LoadedMapping')
	%;   rdf(Id, opmv:wasGeneratedBy, Process),
	%    rdf(Process, rdf:type, amalgame:'SelectPreloaded')
	),
	!,
	findall(C, has_correspondence(C,Id), Mapping0),
	sort(Mapping0, Mapping),
	mapping_stats(Id, Mapping, Strategy, Stats).
expand_mapping(Strategy, Id, Mapping, Stats) :-
	rdf_has(Id, opmv:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process, expand_process(Strategy, Process, Result)),
	materialize_results_if_needed(Strategy, Process, Result),
	select_result_mapping(Id, Result, OutputType, Mapping),
	length(Mapping, Count),
	debug(ag_expand, 'Found ~w mappings for ~p', [Count, Id]),
	mapping_stats(Id, Mapping, Strategy, Stats).


%%	expand_vocab(+Strategy, +Id, -Concepts, -Stats) is det.
%
%	Generate the Vocab according to Strategy.
%	@param Id is URI of a conceptscheme or an identifier for a set
%	of concepts derived by a vocabulary process,

expand_vocab(Strategy, Id, Vocab, Stats) :-
	rdf_has(Id, opmv:wasGeneratedBy, Process ,OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process, expand_process(Strategy, Process, Vocab)),
	vocab_stats(Id, Vocab, Strategy, Stats).

expand_vocab(Strategy, Vocab, Vocab, Stats) :-
	vocab_stats(Vocab, Vocab, Strategy, Stats).


%%	expand_process(+Strategy, +Process, -Result)
%
%	Expand Process according to Strategy to generate Result.
%
%	Results are cached when execution time of process takes longer
%	then setting(cache_time).

expand_process(Strategy, Process, Result) :-
	ground(Process),
	(   expand_cache(Process-Strategy, Result)
	->  debug(ag_expand, 'Output of process ~p taken from cache', [Process])
	;   do_expand_process(Strategy, Process, Result)
	).

do_expand_process(Strategy, Process, Result) :-
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

	% Work is done, but still lots of admin to do...
	cache_expand_result(Time, Process, Strategy, Result),

	% Provenance admin:
	(   Result = scheme(_)   % Result is a single vocabulary
	->  add_amalgame_opm(Strategy, Process, Result)
	;   findall(URI-Mapping, % Result is one or more mappings
		    (   rdf_has(URI, opmv:wasGeneratedBy, Process, OutputType),
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



%%	select_result_mapping(+Id, +Result, +OutputType, -Mapping)
%
%	Mapping is part of (process) Result as defined by OutputType.
%
%	@param OutputType is an RDF property
%	@error existence_error(mapping_select)

select_result_mapping(_Id, select(Selected, Discarded, Undecided), OutputType, Mapping) :-
	!,
	(   rdf_equal(amalgame:selectedBy, OutputType)
	->  Mapping = Selected
	;   rdf_equal(amalgame:discardedBy, OutputType)
	->  Mapping = Discarded
	;   rdf_equal(amalgame:undecidedBy, OutputType)
	->  Mapping = Undecided
	;   throw(error(existence_error(mapping_selector, OutputType), _))
	).

select_result_mapping(Id, overlap(List), P, Mapping) :-
	rdf_equal(opmv:wasGeneratedBy, P),
	(   member(Id-Mapping, List)
	->  true
	;   Mapping=[]
	).

select_result_mapping(_Id, Mapping, P, Mapping) :-
	is_list(Mapping),
	rdf_equal(opmv:wasGeneratedBy, P).


materialize_results_if_needed(Strategy, Process, Results) :-
	findall(Id-RP,
		(   rdf_has(Id, opmv:wasGeneratedBy, Process, RP),
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
%	this graph does not exist yet and if the resource with the same
%	Id has the amalgame:status amalgame:final.

needs_materialization(Id, _, _) :-
	rdf_graph(Id), !, % Already materialized in a prev. run
	fail.
needs_materialization(_Id, Process, _Strategy) :-
	rdfs_individual_of(Process, ProcessType),
	rdf(ProcessType, amalgame:materialize, amalgame:always),
	!,
	true.
needs_materialization(Id, _, _) :-
	rdf_has(Id, amalgame:status, Status),
	\+ rdf_equal(Status, amalgame:final),
	!, % Not a final graph, no need to materalize.
	fail.

materialize(Id, Mapping) :-
	(   rdf_has(Id, amalgame:recordEvidence, amalgame:enabled)
	->  Enabled = enabled
	;   Enabled = disabled
	),
	% voc_clear_stats(all),
	materialize_mapping_graph(Mapping, [graph(Id), evidence_graphs(Enabled)]).



