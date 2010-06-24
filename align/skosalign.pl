:- module(skosalign,
	  [
	   align_schemes/3
	  ]).

:- use_module(library(semweb/rdf_db)).

:- use_module('../matchers/skosmatcher').
:- use_module('../rankers/skosranker').

align_schemes(S1, S2, Options) :-
	rdf_has(C1, skos:inScheme, S1),
	skos_find_candidates(C1, S2, [labels_must_match(true)|Options], Candidates),
	rank_candidates(C1, Candidates, Options).
