:- module(skosranker,
	  [
	   rank_candidates/2
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../edoal/edoal').

match_property(propmatch:def,  P):- rdf_equal(skos:definition, P).
match_property(propmatch:pref, P):- rdf_equal(skos:prefLabel, P).
match_property(propmatch:alt,  P):- rdf_equal(skos:altLabel, P).

rank_candidates(C1, Options) :-
	edoal_match([entity(C1)], Options, Candidates),
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


justification(Cell, Options, MatchType) :-
	match_property(MatchType, Property),
	edoal_match([cell(Cell), entity1(E1), entity2(E2)], Options),
	rdf_has(E1, Property, Value),
	rdf_has(E2, Property, Value).

justification(Cell, Options, MatchType) :-
	match_property(MatchType, Property),
	edoal_match([cell(Cell), entity1(E1), entity2(E2)], Options),
	rdf_has(E1, Property, literal(lang(Lang1, Value))),
	rdf_has(E2, Property, literal(lang(Lang2, Value))),
	lang_matches(Lang1, Lang2).



