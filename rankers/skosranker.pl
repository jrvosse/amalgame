:- module(skosranker,
	  [
	   rank_candidates/2
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../mapping_io/edoal').

rank_candidates(Candidates, Options) :-
	findall(Justifications,
		(   member(Cand, Candidates),
		    justify(Cand, Options, Justifications)
		),
	       JustList),
	rank_just_list(JustList, _Results).

rank_just_list(JustList, JustList):-
	debug(rank, 'Ranking still to be implemented ~w', [JustList]).

justify(Cand, Options, Justifications) :-
	findall(Justification,
		justification(Cand, Options, Justification),
		Justifications).

:- rdf_meta
   meta_property(+,r).

match_property(propmatch:def,  P):- rdf_equal(skos:definition, P).
match_property(propmatch:pref, P):- rdf_equal(skos:prefLabel, P).
match_property(propmatch:alt,  P):- rdf_equal(skos:altLabel, P).

justification(Cand, _Options, MatchType) :-
	match_property(MatchType, Property),
	memberchk(rdf(_,align:entity1, C1), Cand),
	memberchk(rdf(_,align:entity2, C2), Cand),
	rdf_has(C1, Property, Value),
	rdf_has(C2, Property, Value).

justification(Cand, _Options, MatchType) :-
	match_property(MatchType, Property),
	memberchk(rdf(_,align:entity1, C1), Cand),
	memberchk(rdf(_,align:entity2, C2), Cand),
	rdf_has(C1, Property, literal(lang(Lang1, Value))),
	rdf_has(C2, Property, literal(lang(Lang2, Value))),
	lang_matches(Lang1, Lang2).



