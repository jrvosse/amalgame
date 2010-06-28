:- module(skosranker,
	  [
	   rank_candidates/3
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../edoal/edoal').

match_property(propmatch:def,  P):- rdf_equal(skos:definition, P).
match_property(propmatch:pref, P):- rdf_equal(skos:prefLabel, P).
match_property(propmatch:alt,  P):- rdf_equal(skos:altLabel, P).

rank_candidates(C1, S2, Options) :-
	findall(Cand,
		(   rdf_has(Cand, align:entity1, C1),
		    rdf_has(Cand, align:entity2, C2),
		    rdf_has(C2, skos:inScheme, S2)
		),
		Candidates
	       ),
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


justification(Cell, _Options, MatchType) :-
	match_property(MatchType, Property),
	rdf_has(Cell, align:entity1, E1),
	rdf_has(Cell, align:entity2, E2),
	rdf_has(E1, Property, Value),
	rdf_has(E2, Property, Value).

justification(Cell, _Options, MatchType) :-
	match_property(MatchType, Property),
	rdf_has(Cell, align:entity1, E1),
	rdf_has(Cell, align:entity2, E2),
	rdf_has(E1, Property, literal(lang(Lang1, Value))),
	rdf_has(E2, Property, literal(lang(Lang2, Value))),
	lang_equiv(Lang1, Lang2).

lang_equiv(L1, L2) :-
	downcase_atom(L1, Lower),
	downcase_atom(L2, Lower).







