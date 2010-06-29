:- module(skosalign,
	  [
	   align_schemes/3
	  ]).

:- use_module(library(semweb/rdf_db)).

:- use_module('../matchers/skosmatcher').
:- use_module('../rankers/skosranker').

align_schemes(Scheme1, Scheme2, Options) :-
	debug(align, 'Finding source concepts from scheme ~w~n', [Scheme1]),
	findall(Concept,
		(   rdf_has(Concept, skos:inScheme, Scheme1),
		    rdf(Concept, rdf:type, wn30:'AdverbSynset')
		),
		SourceConcepts),

	debug(align, 'Finding candidates mappings from scheme ~w~n', [Scheme2]),
	forall(member(Concept, SourceConcepts),
	       align_concept(Concept, Scheme2, Options)
	      ).
%%	 align_concept(+C1, +S2, +Options) is det.
%
align_concept(SourceConcept, TargetScheme, Options) :-
	rdf_transaction(skos_find_candidates(SourceConcept, TargetScheme, Options)),!,
	rdf_transaction(  justify_candidates(SourceConcept, TargetScheme, Options)),!.

