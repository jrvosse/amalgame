:- module(ag_caching,
	  [
	      get_stats_cache/3,
	      set_stats_cache/3,
	      get_expand_cache/3,
	      cache_result/4,
	      cache_mapped_concepts/4,
	      clean_repository/0,
	      flush_dependent_caches/2,
	      flush_expand_cache/1,     % ?Strategy
	      flush_expand_cache/2,     % +Id, +Strategy
	      flush_refs_cache/1,       % ?Strategy
	      flush_refs_cache/2,
	      flush_stats_cache/1, % ?Strategy
	      flush_stats_cache/2  % +Mapping, +Strategy
	  ]).

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(skos/util)).

:- use_module(ag_provenance).
:- use_module(map).
:- use_module(ag_stats).
:- use_module(scheme_stats).

:- dynamic
	expand_cache/2,
	mapped_concepts_cache/4,
	stats_cache/2.

:- setting(amalgame:cache_time, float, 0.0,
	   'Minimum execution time to cache intermediate \c
	   results, defaults to 0.0, caching everything').


user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing mapping statistics cache after running make/0', []),
	flush_stats_cache(_),
	nickname_clear_cache,
	fail.

user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing expand mapping cache after running make/0', []),
	flush_expand_cache(_),
	fail.
user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing prov cache after running make/0', []),
	flush_prov_cache,
	fail.

get_stats_cache(Strategy, Node, Value) :-
	nonvar(Strategy), nonvar(Node), !,
	atomic_list_concat([stats_cache_, Strategy, Node], Mutex),
	with_mutex(Mutex, stats_cache(Node-Strategy, Value)).
get_stats_cache(Strategy, Node, Value) :-
	stats_cache(Node-Strategy, Value).
set_stats_cache(Strategy, Node, Value) :-
	retractall(stats_cache(Node-Strategy,_)),
	assert(stats_cache(Node-Strategy, Value)).
get_expand_cache(Strategy, Node, Value) :-
	expand_cache(Node-Strategy, Value).

flush_stats_cache(Mapping, Strategy) :-
	retractall(mapped_concepts_cache(Strategy, _, Mapping, _)),
	retractall(stats_cache(Mapping-Strategy,_)).

flush_stats_cache(Strategy) :-
	flush_stats_cache(_Mapping, Strategy).

flush_refs_cache(Strategy) :-
	flush_refs_cache(_Mapping,Strategy).

flush_refs_cache(Mapping, Strategy) :-
	retractall(stats_cache(Mapping-Strategy, refs(_))).

cache_result(_ExecTime, Id, Strategy, Result) :-
	rdfs_individual_of(Id, amalgame:'Mapping'),
	!,
	flush_stats_cache(Id, Strategy),
	mapping_stats(Id, Result, Strategy, Stats),
	assert(stats_cache(Id-Strategy, Stats)).

cache_result(_ExecTime, Id, Strategy, Result) :-
	skos_is_vocabulary(Id),
	!,
	flush_expand_cache(Id, Strategy),
	assert(expand_cache(Id-Strategy, Result)),
	handle_scheme_stats(Strategy, _Process, Id, Result).

cache_result(ExecTime, Process, Strategy, Result) :-
	cache_expand_result(ExecTime, Process, Strategy, Result),
	cache_result_stats(Process, Strategy, Result).

handle_scheme_stats(Strategy, Process, Scheme, Result) :-
	atomic_list_concat([stats_cache_,Strategy,Scheme], Mutex),
	debug(mutex, 'starting ~w', [Mutex]),
	with_mutex(Mutex,
		   handle_scheme_stats_(Strategy, Process, Scheme, Result)),
	debug(mutex, 'finished ~w', [Mutex]).

handle_scheme_stats_(Strategy, Process, Scheme, Result) :-
	flush_stats_cache(Scheme, Strategy),
	scheme_stats(Strategy, Scheme, Result, Stats),
	assert(stats_cache(Scheme-Strategy, Stats)),

	thread_create(
            (   set_stream(user_output, alias(current_output)),
		handle_deep_scheme_stats(Strategy, Process, Scheme, Result)
            ),
	    _,[ detached(true) ]).


handle_deep_scheme_stats(Strategy, Process, Scheme, Result) :-
	atomic_list_concat([stats_cache,Strategy,Scheme], Mutex),
	debug(mutex, 'starting deep ~w', [Mutex]),
	with_mutex(Mutex,
		   handle_deep_scheme_stats_(Strategy, Process, Scheme, Result)),
	debug(mutex, 'finished deep ~w', [Mutex]).
handle_deep_scheme_stats_(Strategy, _Process, Scheme, Result) :-
	scheme_stats_deep(Strategy, Scheme, Result, DeepStats),
	(   stats_cache(Scheme-Strategy, Stats)
	->  retractall(stats_cache(Scheme-Strategy, Stats))
	;   scheme_stats(Strategy, Scheme, Result, Stats)
	),
	put_dict(Stats, DeepStats, NewStats),
	assert(stats_cache(Scheme-Strategy, NewStats)).

cache_mapped_concepts(Strategy, Type, Mapping, Concepts) :-
	var(Concepts),!,
	mapped_concepts_cache(Strategy, Type, Mapping, Concepts).
cache_mapped_concepts(Strategy, Type, Mapping,  Sorted) :-
	assert(mapped_concepts_cache(Strategy, Type, Mapping, Sorted)).

clean_repository :-
	debug(ag_expand, 'Deleting all graphs made by amalgame, including strategies!', []),
	nickname_clear_cache,
	findall(G, is_amalgame_graph(G), Gs),
	forall(member(G, Gs),
	       (   debug(ag_expand, 'Deleting named graph ~p', [G]),
		   rdf_unload_graph(G)
	       )
	      ).

%%	flush_expand_cache(+Strategy)
%
%	Retract all cached mappings.

flush_expand_cache(Strategy) :-
	del_prov_graphs(Strategy),
	del_materialized_vocs(Strategy),
	del_materialized_mappings(Strategy),
	forall(expand_cache(Id-Strategy, _),
	       flush_expand_cache(Id, Strategy)
	      ).

flush_expand_cache(Id, Strategy) :-
	(   expand_cache(Id-Strategy, _) % make sure Id is bounded to something in the cache
	->  retractall(expand_cache(Id-Strategy, _)),
	    debug(ag_expand, 'flush expand mapping cache for results of process ~p', [Id])
	;   true
	).

flush_dependent_caches(Id, Strategy) :-
	(   rdf_has(Id, amalgame:wasGeneratedBy, Process, RP0),
	    rdf(Id, RP0, Process, Strategy)
	->  true
	;   rdf_has(Data, amalgame:wasGeneratedBy, Id, RP1),
	    rdf(Data, RP1, Process, Strategy)
	->  Process = Id
	;   rdf_has(Process, prov:used, Id, RP2),
	    rdf(Process, RP2, Id, Strategy)
	->  true
	;   true
	),
	(   ground(Process)
	->  flush_process_dependent_caches(Process, Strategy,
					   [flush(expand), flush(stats)])
	;   true
	).


flush_process_dependent_caches(Process, Strategy, Options) :-
	(   option(flush(expand), Options)
	->  flush_expand_cache(Process, Strategy),
	    provenance_graph(Strategy, ProvGraph),
	    remove_old_prov(Process, ProvGraph)
	;   true
	),

	findall(Result,
		(   rdf_has(Result, amalgame:wasGeneratedBy, Process, RP3),
		    rdf(Result, RP3, Process, Strategy)
		), Results),
	forall(member(Result, Results),
	       (   (   option(flush(stats), Options)
		   ->  flush_stats_cache(Result, Strategy),
		       debug(ag_expand, 'flush stats cache for ~p', [Result])
		   ;   true
		   ),
		   (   option(flush(expand), Options)
		   ->  catch(rdf_unload_graph(Result), _, true),
		       debug(ag_expand, 'unloading any materialized graphs for ~p', [Result])
		   ;   true
		   )
	       )
	      ),
	findall(DepProcess,
		(   member(Result, Results),
		    rdf_has(DepProcess, amalgame:input, Result, RP4),
		    rdf(DepProcess, RP4, Result, Strategy)
		),
		Deps),
	forall(member(Dep, Deps),
	       flush_process_dependent_caches(Dep, Strategy, Options)).

cache_result_stats(_Process, Strategy, mapspec(overlap(List))) :-
	forall(member(Id-Mapping, List),
	       (   mapping_stats(Id, Mapping, Strategy, Stats),
		   flush_stats_cache(Id, Strategy),
		   assert(stats_cache(Id-Strategy, Stats))
	       )
	      ).

cache_result_stats(Process, Strategy, mapspec(select(Sel, Disc, Undec))) :-
	rdf(S, amalgame:selectedBy, Process, Strategy),
	!,
	mapping_stats(S, Sel, Strategy,  Sstats),
	flush_stats_cache(S, Strategy),
	assert(stats_cache(S-Strategy, Sstats)),

	% in older strategies discarded and undecided were optional...
	(   rdf(D, amalgame:discardedBy, Process, Strategy)
	->  mapping_stats(D, Disc, Strategy, Dstats),
	    flush_stats_cache(D, Strategy),
	    assert(stats_cache(D-Strategy, Dstats))
	;   true
	),
	(   rdf(U, amalgame:undecidedBy, Process, Strategy)
	->  mapping_stats(U, Undec, Strategy,Ustats),
	    flush_stats_cache(U, Strategy),
	    assert(stats_cache(U-Strategy, Ustats))
	;   true
	).
cache_result_stats(Process, Strategy, vocspec(select(Sel, Dis, Und))) :-
	!,
	rdf(S, amalgame:selectedBy,  Process, Strategy),
	rdf(D, amalgame:discardedBy, Process, Strategy),
	rdf(U, amalgame:undecidedBy, Process, Strategy),

	handle_scheme_stats(Strategy, Process, S, Sel),
	handle_scheme_stats(Strategy, Process, D, Dis),
	handle_scheme_stats(Strategy, Process, U, Und).

cache_result_stats(Process, Strategy, mapspec(mapping(Result))) :-
	rdf_has(D, amalgame:wasGeneratedBy, Process, RP),
	rdf(D, RP, Process, Strategy),
	!,
	flush_stats_cache(D, Strategy),
	mapping_stats(D, Result, Strategy, Dstats),
	assert(stats_cache(D-Strategy, Dstats)).


cache_result_stats(Process, Strategy, vocspec(Result)) :-
	rdf_has(Vocab, amalgame:wasGeneratedBy, Process, RP),
	rdf(Vocab, RP, Process, Strategy),
	!,
	handle_scheme_stats(Strategy, Process, Vocab, Result).

cache_result_stats(Process, _Strategy, _Result) :-
	debug(ag_caching, 'Error: do not know how to cache stats of ~p', [Process]),
	fail.

cache_expand_result(ExecTime, Process, Strategy, Result) :-
	setting(amalgame:cache_time, CacheTime),
	ExecTime > CacheTime,
	!,
	assert(expand_cache(Process-Strategy, Result)).

cache_expand_result(_, _, _, _).


del_prov_graphs(S) :-
	findall(P,provenance_graph(S,P), ProvGraphs),
	forall(member(P, ProvGraphs),
	       (   catch(rdf_unload_graph(P), _, true),
		   debug(ag_expand, 'Deleting provenance graph ~w', [P])
	       )
	      ).

del_materialized_mappings(Strategy) :-
	findall(Id, mapping_to_delete(Id, Strategy), ToDelete),
	sort(ToDelete, Sorted),
	forall(member(F, Sorted),
	       (   del_evidence_graphs(F),
		   catch(rdf_unload_graph(F), _, true),
		   debug(ag_expand, 'Deleting materialized mapping graph ~w', [F])
	       )
	      ).

% deletes all ev graphs, for development use only:
del_evidence_graphs :-
	forall(
	    (	rdf_graph(G),
		sub_atom(G, 0,_,_, '__bnode'),        % hack
		sub_atom(G, _,_,0, '_evidence_graph') % hack
	    ),
	    rdf_unload_graph(G)).

del_evidence_graphs(Mapping) :-
	forall(
	    (
	    rdf(Mapping, align:map, Cell, Mapping),
	    rdf(Cell, amalgame:evidence, Bnode, Mapping),
	    rdf(Bnode, amalgame:evidenceGraph, EvGraph, Mapping),
	    rdf_graph(EvGraph)
	    ),
	    rdf_unload_graph(EvGraph)).


mapping_to_delete(Id, Strategy) :-
	rdfs_individual_of(Id, amalgame:'Mapping'),
	rdf(Id,_,_, Strategy),
	\+ rdfs_individual_of(Id, amalgame:'EvaluatedMapping'),
	\+ rdfs_individual_of(Id, amalgame:'LoadedMapping'),
	rdf_graph(Id).

del_materialized_vocs(Strategy) :-
	findall(Voc,
		(   skos_is_vocabulary(Voc),
		    rdf_graph(Voc),
		    rdf_has(Voc, amalgame:wasGeneratedBy, Process, RP),
		    rdf(Voc, RP, Process, Strategy)
		), Vocs),
	forall(member(V, Vocs),
	       (   catch(rdf_unload_graph(V), _, true),
		   flush_stats_cache(V,Strategy),
		   debug(ag_expand, 'Deleting materialized vocab graph ~w', [V])
	       )
	      ).

del_bnode_graphs :-
	forall((rdf_graph(Bnode),
		rdf_is_bnode(Bnode)
	       ),
	       rdf_unload_graph(Bnode)).

del_empty_graphs :-
	forall(rdf_graph_property(Graph, triples(0)),
	       rdf_unload_graph(Graph)).

