:- module(expand_graph,
	  [ expand_mapping/3,
	    expand_vocab/3,
	    flush_expand_cache/0,
	    flush_expand_cache/2,     % +Id, +Strategy
	    process_options/3,
	    save_mappings/3,
	    evaluation_graph/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_parameters)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/opm)).
:- use_module(library(amalgame/ag_provenance)).

:- use_module(library(skos/vocabularies)).

:- dynamic
	expand_cache/2.

:- setting(cache_time, float, 0.5,
	   'Minimum execution time to cache results').

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

expand_mapping(_Strategy, Id, Mapping) :-
	rdfs_individual_of(Id, amalgame:'EvaluatedMapping'),
	!,
	findall(C, has_correspondence(C,Id), Mapping).

expand_mapping(Strategy, Id, Mapping) :-
	rdf_has(Id, opmv:wasGeneratedBy, Process, OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process, expand_process(Strategy, Process, Result)),
	select_result_mapping(Result, OutputType, Mapping),
	length(Mapping, Count),
	debug(ag_expand, 'Found ~w mappings for ~p', [Count, Id]),
	materialize_if_needed(Id, Mapping).

%%	expand_vocab(+Strategy, +Id, -Concepts) is det.
%
%	Generate the Vocab according to Strategy.
%	@param Id is URI of a conceptscheme or an identifier for a set
%	of concepts derived by a vocabulary process,

expand_vocab(Strategy, Id, Vocab) :-
	rdf_has(Id, opmv:wasGeneratedBy, Process ,OutputType),
	rdf(Id, OutputType, Process, Strategy),
	!,
	with_mutex(Process, expand_process(Strategy, Process, Vocab)).

expand_vocab(_Strategy, Vocab, Vocab).

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

	% Work is done, but still lots of admin to do... caching first
	cache_expand_result(Time, Process, Strategy, Result),

	% Provenance admin:
	(   Result = scheme(_)   % Result is a single vocabulary
	->  add_amalgame_opm(Strategy, Process, Result)
	;   findall(URI-Mapping, % Result is one or more mappings
		    (   rdf_has(URI, opmv:wasGeneratedBy, Process, OutputType),
			rdf(URI, OutputType, Process, Strategy),
			select_result_mapping(Result, OutputType, Mapping)
		    ),
		    Artifacts),
	    add_amalgame_opm(Strategy, Process, Artifacts)
	),

	% HACK: this is needed to get preloaded graphs in the strategy graph
        % because we cannot select a mapping that has not been generated within the
	% builder as an input mapping...
	(   rdfs_subclass_of(Type, amalgame:'SelectPreLoaded')
	->  option(name(PreloadedGraph), Options),
	    rdf_assert(Process, opmv:used, PreloadedGraph, Strategy)
	;   true
	).

cache_expand_result(ExecTime, Process, Strategy, Result) :-
	setting(cache_time, CacheTime),
	ExecTime > CacheTime,
	!,
	assert(expand_cache(Process-Strategy, Result)).
cache_expand_result(_, _, _, _).

%%	flush_expand_cache(+Id)
%
%	Retract all cached mappings.

flush_expand_cache :-
	del_prov_graphs,
	del_materialized_vocs,
	del_materialized_mappings,
	forall(expand_cache(Id-Strategy, _),
	       flush_expand_cache(Id, Strategy)
	      ).

flush_expand_cache(Id, Strategy) :-
	expand_cache(Id-Strategy, _), % make sure Id is bounded to something in the cache
	retractall(expand_cache(Id-Strategy, _)),
	catch(rdf_unload(Id), _, true),
	debug(ag_expand, 'flush cache and unloading graph for ~p', [Id]).

del_prov_graphs :-
	findall(P,provenance_graph(_,P), ProvGraphs),
	forall(member(P, ProvGraphs),
	       (   catch(rdf_unload(P), _, true),
		   debug(ag_expand, 'Deleting provenance graph ~w', [P])
	       )
	      ).

del_materialized_mappings :-
	% (   rdf_graph(void) -> rdf_unload(void); true),
	findall(Id, (
		     rdfs_individual_of(Id, amalgame:'Mapping'),
		     rdf_graph(Id)
		    ), Finals),
	forall(member(F, Finals),
	       (   catch(rdf_unload(F), _, true),
		   debug(ag_expand, 'Deleting final result graph ~w', [F])
	       )
	      ).

del_materialized_vocs :-
	findall(Voc,
		(   rdfs_individual_of(Voc, skos:'ConceptScheme'),
		    rdf_graph(Voc),
		    rdf_has(Voc, opmv:wasGeneratedBy, _)
		), Vocs),
	forall(member(V, Vocs),
	       (   catch(rdf_unload(V), _, true),
		   voc_clear_stats(V),
		   debug(ag_expand, 'Deleting materialized vocab graph ~w', [V])
	       )
	      ).

%%	exec_amalgame_process(+Type, +Process, +Module, -Result,
%%	+Options)
%
%	Result is generated by executing Process of type Type.
%
%	@error existence_error(mapping_process)

exec_amalgame_process(Type, Process, Strategy, Module, Mapping, Time, Options) :-
	rdfs_subclass_of(Type, amalgame:'Matcher'),
	!,
	(   rdf(Process, amalgame:source, SourceId, Strategy),
	    rdf(Process, amalgame:target, TargetId, Strategy)
	->  expand_vocab(Strategy, SourceId, Source),
	    expand_vocab(Strategy, TargetId, Target),
	    timed_call(Module:matcher(Source, Target, Mapping0, Options), Time)
	;   rdf(Process, amalgame:input, InputId)
	->  expand_mapping(Strategy, InputId, MappingIn),
	    timed_call(Module:filter(MappingIn, Mapping0, Options), Time)
	),
	merge_provenance(Mapping0, Mapping).

exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'VocExclude'),
	rdf(NewVocab, opmv:wasGeneratedBy, Process, Strategy),
	NewVocOption = new_scheme(NewVocab),
	!,
	once(rdf(Process, amalgame:input, Input, Strategy)),
	expand_vocab(Strategy, Input, Vocab),
	findall(S, rdf(Process, amalgame:exclude, S), Ss),
	maplist(expand_mapping(Strategy), Ss, Expanded),
	append(Expanded, Mapping),
	timed_call(Module:exclude(Vocab, Mapping, Result, [NewVocOption|Options]), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'MappingSelecter'),
	!,
	Result = select(Selected, Discarded, Undecided),
	once(rdf(Process, amalgame:input, InputId, Strategy)),
	expand_mapping(Strategy, InputId, MappingIn),
	timed_call(Module:selecter(MappingIn, Selected, Discarded, Undecided, Options), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'VocabSelecter'),
	!,
	once(rdf(Process, amalgame:input, Input, Strategy)),
	expand_vocab(Strategy, Input, Vocab),
	timed_call(Module:selecter(Vocab, Result, Options), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'Merger'),
	!,
	findall(Input, rdf(Process, amalgame:input, Input, Strategy), Inputs),
	maplist(expand_mapping(Strategy), Inputs, Expanded),
	timed_call(Module:merger(Expanded, Result, Options), Time).

exec_amalgame_process(Class, Process,_,_, _, _, _) :-
	throw(error(existence_error(mapping_process, [Class, Process]), _)).

timed_call(Goal, Time) :-
	thread_self(Me),
        thread_statistics(Me, cputime, T0),
	call(Goal),
	thread_statistics(Me, cputime, T1),
        Time is T1 - T0.


%%	select_result_mapping(+ProcessResult, +OutputType, -Mapping)
%
%	Mapping is part of ProcessResult as defined by OutputType.
%
%	@param OutputType is an RDF property
%	@error existence_error(mapping_select)

select_result_mapping(select(Selected, Discarded, Undecided), OutputType, Mapping) :-
	!,
	(   rdf_equal(amalgame:selectedBy, OutputType)
	->  Mapping = Selected
	;   rdf_equal(amalgame:discardedBy, OutputType)
	->  Mapping = Discarded
	;   rdf_equal(amalgame:undecidedBy, OutputType)
	->  Mapping = Undecided
	;   throw(error(existence_error(mapping_selector, OutputType), _))
	).
select_result_mapping(Mapping, P, Mapping) :-
	is_list(Mapping),
	rdf_equal(opmv:wasGeneratedBy, P).

%%	process_options(+Process, +Module, -Options)
%
%	Options are the instantiated parameters for Module based on the
%	parameters string in Process.

process_options(Process, Module, Options) :-
	rdf(Process, amalgame:parameters, literal(ParamString)),
	!,
	module_options(Module, Options, Parameters),
	parse_url_search(ParamString, Search),
	Request = [search(Search)] ,
	http_parameters(Request, Parameters).
process_options(_, _, []).


%%	module_options(+Module, -Options, -Parameters)
%
%	Options  are  all  option  clauses    defined   for  Module.
%	Parameters is a specification list for http_parameters/3.
%	Module:parameter is called as:
%
%	    parameter(Name, Properties, Description)
%
%	Name is the name of the	the option, The Properties are as
%	supported by http_parameters/3.	Description is used by the help
%	system.

module_options(Module, Options, Parameters) :-
	current_predicate(Module:parameter/4),
	!,
	findall(O-P,
		( call(Module:parameter, Name, Type, Default, _Description),
		  O =.. [Name, Value],
		  param_options(Type, Default, ParamOptions),
		  P =.. [Name, Value, ParamOptions]
		),
		Pairs),
	pairs_keys_values(Pairs, Options, Parameters).
module_options(_, _, []).


param_options(Type, Default, Options) :-
	(   is_list(Type)
	->  Options = [default(Default)|Type]
	;   Options = [default(Default), Type]
	).

%%	materialize_if_needed(+Id, Mapping) is det.
%
%	materialize result in Mapping in named graph Id if this graph
%	this graph does not exist yet and if the resource with the same
%	Id has the amalgame:status amalgame:final.

materialize_if_needed(Id, _) :-
	rdf_graph(Id), !. % Already materialized in a prev. run
materialize_if_needed(Id, _) :-
	rdf_has(Id, amalgame:status, Status),
	\+ rdf_equal(Status, amalgame:final),
	!. % Not a final graph, no need to materalize.
materialize_if_needed(Id, Mapping) :-
	(   rdf_has(Id, amalgame:recordEvidence, amalgame:enabled)
	->  Enabled = enabled
	;   Enabled = disabled
	),
	voc_clear_stats(all),
	materialize_mapping_graph(Mapping, [graph(Id), evidence_graphs(Enabled)]).

void_graph(Strategy, VoidGraph) :-
	ground(Strategy),
	atomic_concat(Strategy, '_void', VoidGraph).

make_new_directory(D) :-
	(   exists_directory(D)
	->  atomic_concat(D, '/*.ttl', WildCard),
	    expand_file_name(WildCard, L),
	    forall(member(F,L), delete_file(F)),
	    delete_directory(D)
	;   true
	),
	make_directory(D).

save_mappings(Strategy, Dir, Options) :-
	provenance_graph(Strategy, ProvGraph),
	void_graph(Strategy, VoidGraph),
	(   rdf_graph(VoidGraph) -> rdf_unload(VoidGraph); true),

	make_new_directory(Dir),
	file_base_name(Strategy, StrategyB),
	file_base_name(ProvGraph, ProvGraphB),
	absolute_file_name(StrategyB,  StratFile, [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(ProvGraphB, ProvFile,  [relative_to(Dir), extensions([ttl])]),
	absolute_file_name(void,      VoidFile,  [relative_to(Dir), extensions([ttl])]),

	select_mappings_to_be_saved(Strategy, Mappings, Options),
	forall(member(Mapping, Mappings),
	       save_mapping(Mapping, Strategy, ProvGraph, [dir(Dir),Options])),

	rdf_save_turtle(StratFile, [graph(Strategy)|Options]),
	rdf_save_turtle(ProvFile,  [graph(ProvGraph)|Options]),
	rdf_save_turtle(VoidFile,  [graph(VoidGraph)|Options]),
	rdf_unload(VoidGraph).


save_mapping(Id, Strategy, ProvGraph, Options) :-
	(   \+ rdf_graph(Id)
	->  expand_mapping(Strategy, Id, Mapping),
	    materialize_mapping_graph(Mapping, [graph(Id)])
	;   true
	),
	rdf_equal(xsd:int, Int),

	void_graph(Strategy, Void),
	rdf_statistics(triples_by_file(Id, NrOfTriples)),
	assert_metadata(Id, Strategy, Void),
	rdf_assert(Id, void:vocabulary,   amalgame:'', Void),
	rdf_assert(Id, void:vocabulary,   void:'', Void),
	rdf_assert(Id, rdf:type,          void:'Linkset', Void),
	rdf_assert(Id, void:triples, literal(type(Int,NrOfTriples)), Void),

	rdf_assert(Id, amalgame:strategy, Strategy, Void),
	rdf_assert(Id, amalgame:opm,      ProvGraph, Void),

	file_base_name(Id, Base),
	option(dir(Dir), Options, tmpdir),
	absolute_file_name(Base,  Name, [relative_to(Dir), extensions([ttl])]),
	rdf_save_turtle(Name, [graph(Id)|Options]).

assert_metadata(Id, Strategy, Graph) :-
	findall(rdf(Id,P,O),
		is_metadata_triple(Id, P, O, Strategy),
		Triples),
	expand_bnode_objects(Triples, Expanded),
	forall(member(rdf(S,P,O), Expanded), rdf_assert(S,P,O,Graph)).

expand_bnode_objects([],[]).
expand_bnode_objects([rdf(S,P,O)|Tail], [rdf(S,P,O)|Expanded]) :-
	expand_bnode_objects(Tail, ExpandedTail),
	(   rdf_is_bnode(O)
	->  Bnode = O,
	    findall(rdf(Bnode, P1, O1), rdf(Bnode, P1, O1), BnodeTriples),
	    expand_bnode_objects(BnodeTriples, ExpandedBnode),
	    append(ExpandedBnode, ExpandedTail, Expanded)
	;   Expanded = ExpandedTail
	).

is_metadata_triple(S,P,O,Graph) :-
	rdf_has(S,opmv:wasGeneratedBy, Process, RP),
	rdf(S,RP,Process,Graph),
	rdf(Process, opmv:wasPerformedBy, O),
	rdf_equal(dcterms:creator, P).
is_metadata_triple(S,P,literal(type(T,N)), _Graph) :-
	rdf_has(S, amalgame:mappedSourceConcepts, literal(type(T,N))),
	rdf_equal(P, void:distinctSubjects).
is_metadata_triple(S,P,literal(type(T,N)), _Graph) :-
	rdf_has(S, amalgame:mappedTargetConcepts, literal(type(T,N))),
	rdf_equal(P, void:distinctObjects).

select_mappings_to_be_saved(Graph, Mappings, Options) :-
	option(status(Status), Options, all),
	(   Status == all
	->  findall(Mapping,
		    rdf(Mapping, rdf:type, amalgame:'Mapping', Graph),
		    Mappings)
	;   findall(Mapping, (
		rdf(Mapping, rdf:type, amalgame:'Mapping', Graph),
		rdf(Mapping, amalgame:status, Status)
	      ), Mappings)
	).

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	!.

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	gensym(evaluation_process, EvalProcess),
	gensym(evaluation_result, EvalGraph),
	format(atom(Comment), 'Manual evaluation of ~w', [Mapping]),

	rdf_assert(EvalProcess, rdf:type, amalgame:'EvaluationProcess', Strategy),
	rdf_assert(EvalProcess, rdfs:label, literal('Manual evaluation')),
	rdf_assert(EvalProcess, amalgame:input,	Mapping, Strategy),

	rdf_assert(EvalGraph, rdf:type, amalgame:'EvaluatedMapping', Strategy),
	rdf_assert(EvalGraph, rdfs:label, literal('Evaluation results')),
	rdf_assert(EvalGraph, rdfs:comment, literal(Comment)),
	rdf_assert(EvalGraph, opmv:wasGeneratedBy, EvalProcess, Strategy),
	rdf_assert(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),

	Options = [was_derived_from([Mapping])],
	provenance_graph(Strategy, ProvGraph),
	opm_was_generated_by(EvalProcess, [EvalGraph], ProvGraph, Options).

delete_eval_graph_admin(Strategy, Mapping, EvalGraph) :-
	% Beware, this will delete all metadata about your manual evaluations!
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	rdf(EvalGraph, opmv:wasGeneratedBy, EvalProcess, Strategy),
	!,
	rdf_retractall(EvalGraph, _, _, Strategy),
	rdf_retractall(EvalProcess, _, _, Strategy).


