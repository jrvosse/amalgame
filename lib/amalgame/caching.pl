:- module(ag_caching,
	  [
	   expand_cache/2,
	   stats_cache/2,
	   cache_result/4,
	   clean_repository/0,
	   flush_dependent_caches/2,
	   flush_expand_cache/1,     % ?Strategy
	   flush_expand_cache/2,     % +Id, +Strategy
	   flush_refs_cache/1,       % ?Strategy
	   flush_refs_cache/2,
	   flush_stats_cache/1, % ?Strategy
	   flush_stats_cache/2  % +Mapping, +Strategy
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(ag_stats).

:- dynamic
	expand_cache/2,
	stats_cache/2.

:- setting(amalgame:cache_time, float, 0.05,
	   'Minimum execution time to cache results').

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
	debug(ag_expand, 'Flushing prov program version cache after running make/0', []),
	flush_prov_cache,
	fail.
user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing vocabulary statistics cache after running make/0', []),
	voc_clear_stats(all),
	fail.

flush_stats_cache(Strategy) :-
	flush_stats_cache(_Mapping, Strategy).

flush_stats_cache(Mapping, Strategy) :-
	retractall(stats_cache(Mapping-Strategy,_)).

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

cache_result(ExecTime, Process, Strategy, Result) :-
	cache_expand_result(ExecTime, Process, Strategy, Result),
	cache_result_stats(Process, Strategy, Result).

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
	(   rdf_has(Id, amalgame:wasGeneratedBy, Process, RP),
	    rdf(Id, RP, Process, Strategy)
	->  true
	;   Process = Id
	),
	flush_expand_cache(Process, Strategy),
	findall(Result,
		(   rdf_has(Result, amalgame:wasGeneratedBy, Process, RP),
		    rdf(Result, RP, Process, Strategy)
		), Results),
	forall(member(Result, Results),
	       (   flush_stats_cache(Result, Strategy),
		   catch(rdf_unload_graph(Result), _, true),
		   debug(ag_expand, 'flush stats cache for ~p and unloading any materialized graphs', [Result])
	       )
	      ),
	findall(DepProcess,
		(   member(Result, Results),
		    rdf_has(DepProcess, amalgame:input, Result, RP),
		    rdf(DepProcess, RP, Result, Strategy)
		),
		Deps),
	forall(member(Dep, Deps),
	       flush_dependent_caches(Dep, Strategy)),

	provenance_graph(Strategy, ProvGraph),
	remove_old_prov(Process, ProvGraph).


cache_result_stats(_Process, Strategy, overlap(List)) :-
	forall(member(Id-Mapping, List),
	       (   mapping_stats(Id, Mapping, Strategy, Stats),
		   flush_stats_cache(Id, Strategy),
		   assert(stats_cache(Id-Strategy, Stats))
	       )
	      ).

cache_result_stats(Process, Strategy, select(Sel, Disc, Undec)) :-
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

cache_result_stats(Process, Strategy, SchemeSpec) :-
	(   SchemeSpec = scheme(Id)
	->  true
	;   SchemeSpec = and(_,_)
	->  rdf(Id, amalgame:wasGeneratedBy, Process, Strategy)
	;   fail
	),
	!,
	vocab_stats(SchemeSpec, Count),
	retractall(stats_cache(Id-Strategy,_)),
	assert(stats_cache(Id-Strategy, stats(Count))).

cache_result_stats(Process, Strategy, Result) :-
	rdf_has(D, amalgame:wasGeneratedBy, Process, RP),
	rdf(D, RP, Process, Strategy),
	!,
	flush_stats_cache(D, Strategy),
	mapping_stats(D, Result, Strategy, Dstats),
	assert(stats_cache(D-Strategy, Dstats)).

cache_result_stats(Process, _Strategy, _Result) :-
	debug(ag_expand, 'Error: do not know how to cache stats of ~p', [Process]),
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
	       (   catch(rdf_unload_graph(F), _, true),
		   del_evidence_graphs(F),
		   debug(ag_expand, 'Deleting materialized mapping graph ~w', [F])
	       )
	      ).

del_evidence_graphs(Mapping) :-
	forall(
	    (
	    rdf(Mapping, align:map, Cell, Mapping),
	    rdf(Cell, amalgame:evidence, Bnode, Mapping),
	    rdf(Bnode, amalgame:evidenceGraph, Bnode, Mapping),
	    rdf_graph(Bnode),
	    rdf_is_bnode(Bnode)
	    ),
	    rdf_unload_graph(Bnode)).


mapping_to_delete(Id, Strategy) :-
	rdfs_individual_of(Id, amalgame:'Mapping'),
	rdf(Id,_,_, Strategy),
	\+ rdfs_individual_of(Id, amalgame:'EvaluatedMapping'),
	\+ rdfs_individual_of(Id, amalgame:'LoadedMapping'),
	rdf_graph(Id).

del_materialized_vocs(Strategy) :-
	findall(Voc,
		(   rdfs_individual_of(Voc, skos:'ConceptScheme'),
		    rdf_graph(Voc),
		    rdf_has(Voc, amalgame:wasGeneratedBy, Process, RP),
		    rdf(Voc, RP, Process, Strategy)
		), Vocs),
	forall(member(V, Vocs),
	       (   catch(rdf_unload_graph(V), _, true),
		   voc_clear_stats(V),
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

