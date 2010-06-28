:- module(skosalign,
	  [
	   align_schemes/3
	  ]).

:- use_module(library(semweb/rdf_db)).

:- use_module('../matchers/skosmatcher').
:- use_module('../rankers/skosranker').

align_schemes(Scheme1, Scheme2, Options) :-
	findall(Concept, rdf_has(Concept, skos:inScheme, Scheme1), SourceConcepts),
	forall(member(Concept, SourceConcepts),
	       align_concept(Concept, Scheme2, [labels_must_match(true)|Options])
	      ).

align_concept(SourceConcept, TargetScheme, Options) :-
	rdf_transaction(skos_find_candidates(SourceConcept, TargetScheme, Options)),
	rdf_transaction(     rank_candidates(SourceConcept, TargetScheme, Options)).

