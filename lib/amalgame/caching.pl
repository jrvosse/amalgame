:- module(ag_caching,
	  [
	   expand_cache/2,
	   stats_cache/2,
	   cache_expand_result/4,
	   clean_repository/0,
	   flush_expand_cache/0,
	   flush_expand_cache/2,     % +Id, +Strategy
	   flush_stats_cache/0,
	   flush_stats_cache/2 % +Mapping, +Strategy
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
%:- use_module(library(http/http_parameters)).
%:- use_module(library(amalgame/alignment)).
%:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/opm)).
:- use_module(library(amalgame/ag_provenance)).
%:- use_module(library(amalgame/vocabulary)).
%:- use_module(library(amalgame/amalgame_modules)).

:- use_module(library(skos/vocabularies)).
%:- use_module(library(ag_drivers/exec_amalgame_process)).

:- dynamic
	expand_cache/2,
	stats_cache/2.

:- setting(cache_time, float, 0.5,
	   'Minimum execution time to cache results').

user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing stats cache after running make/0', []),
	flush_stats_cache,
	fail.

user:message_hook(make(done(_)), _, _) :-
	debug(ag_expand, 'Flushing expand cache after running make/0', []),
	retractall(current_program_uri(_)),
	flush_expand_cache,
	fail.

flush_stats_cache :-
	retractall(stats_cache(_,_)).

flush_stats_cache(Mapping, Strategy) :-
	retractall(stats_cache(Mapping-Strategy,_)).


cache_expand_result(ExecTime, Process, Strategy, Result) :-
	setting(cache_time, CacheTime),
	ExecTime > CacheTime,
	!,
	assert(expand_cache(Process-Strategy, Result)).

cache_expand_result(_, _, _, _).

clean_repository :-
	debug(ag_expand, 'Deleting all graphs made by amalgame', []),
	retractall(ag_alignment:nickname_cache(_,_,_)),
	findall(G, is_amalgame_graph(G), Gs),
	forall(member(G, Gs),
	       (   debug(ag_expand, 'Deleting named graph ~p', [G]),
		   rdf_unload(G)
	       )
	      ).

is_amalgame_graph(G) :-
	rdf_graph(G),
	(   rdf(G, amalgame:strategy, _) % G is provenance graph
	;   rdfs_individual_of(G, amalgame:'AlignmentStrategy')
	;   once(rdf(G, align:map, _, G))	 % G is mapping graph
	;   G == amalgame
	;   G == amalgame_vocs
	).

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
	(   expand_cache(Id-Strategy, _) % make sure Id is bounded to something in the cache
	->  retractall(expand_cache(Id-Strategy, _)),
	    catch(rdf_unload(Id), _, true),
	    debug(ag_expand, 'flush cache and unloading graph for ~p', [Id])
	;   true
	).

del_prov_graphs :-
	findall(P,provenance_graph(_,P), ProvGraphs),
	forall(member(P, ProvGraphs),
	       (   catch(rdf_unload(P), _, true),
		   debug(ag_expand, 'Deleting provenance graph ~w', [P])
	       )
	      ).

del_materialized_mappings :-
	findall(Id, (
		     rdfs_individual_of(Id, amalgame:'Mapping'),
		     \+ rdfs_individual_of(Id, amalgame:'EvaluatedMapping'),
		     \+ rdfs_individual_of(Id, amalgame:'LoadedMapping'),
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




