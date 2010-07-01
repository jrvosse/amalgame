:- module(skosmatcher,
	  [skos_find_candidates/3 % +SourceConcept, +TargetScheme, +Options, -Results
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../edoal/edoal').

%%	skos_find_candidates(+C, +S, +Options, -Result) is det.
%
%	Return all correspondences candidates for for SKOS concept C
%	from SKOS scheme S. Result is an RDF graph in (extended) EDOAL format
%
%	Options include:
%	- labels_must_match(true):
%           Find candidates with match labels (cheap)

skos_find_candidates(SourceConcept, TargetConceptScheme, Options):-
	forall(candidate(SourceConcept, TargetConceptScheme, Options),
	       true
	      ).

%%	candidate(+C, +S, +Options) is det.
%
%	Asserts a correspondence as an EDOAL cell
%	describing a match from C to a target concept from
%	ConceptScheme S.
%
%       Correspondence has a low confidence level,
%       to indicate that further checking will be needed to
%       evaluate the quality of this candidate.
%
%       The method will list which (pref/alt) labels properties
%       where used for the match.
%
%       For descriptions of possible Options, see above.

candidate(SourceConcept, TargetConceptScheme, Options) :-
	ground(SourceConcept),
	ground(TargetConceptScheme),
	ground(Options),
	option(candidate_matchers(Matchers), Options, []),
	memberchk(labelmatch, Matchers),

	rdf_has(SourceConcept, rdfs:label, literal(lang(_, Label)), RealLabel1Predicate),
	rdf_has(TargetConcept, rdfs:label, literal(lang(_, Label)), RealLabel2Predicate),
	rdf_has(TargetConcept, skos:inScheme, TargetConceptScheme),
	format(atom(Method), 'exact match: ~p-~p', [RealLabel1Predicate, RealLabel2Predicate]),
	CellOptions = [measure(0.001), % Only label match, this is just a candidate
		       method(Method)
		       |Options
		      ],
	assert_cell(SourceConcept, TargetConcept, CellOptions).
