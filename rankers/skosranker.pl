:- module(skosranker,
	  [
	   rank_candidates/3
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../mapping_io/edoal').

rank_candidates(Concept, Candidates, Options) :-
	findall(Justifications,
		(   member(Cand, Candidates),
		    justify(Concept, Cand, Options, Justifications)
		),
	       JustList),
	rank_just_list(JustList, _Results).

rank_just_list(JustList, JustList).

justify(C1, C2, Options, Justifications) :-
	findall(Justification,
		justification(C1, C2, Options, Justification),
		Justifications).

:- rdf_meta
   meta_property(+,r).

match_property(def, skos:definition).

justification(C1, C2, _Options, MatchType) :-
	match_property(MatchType, Property),
	rdf_has(C1, Property, Value),
	rdf_has(C2, Property, Value).




