:- module(skosmatcher,
	  [skos_find_candidates/4 % +SourceConcept, +TargetScheme, +Options, -Results
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../mapping_io/edoal').

%%	skos_find_candidates(+C, +S, +Options, -Result) is det.
%
%	Return all correspondences candidates for for SKOS concept C
%	from SKOS scheme S. Result is an RDF graph in (extended) EDOAL format
%
%	Options include:
%	- labels_must_match(true):
%           Find candidates with match labels (cheap)

skos_find_candidates(SourceConcept, TargetConceptScheme, Options, Correspondences):-
	findall(Correspondence,
		candidate(SourceConcept, TargetConceptScheme, Options, Correspondence),
		Correspondences
	       ).

%%	candidate(+C, +S, +Options, -Correspondence) is det.
%
%	Evaluates to true if Correspondence describes an EDOAL cell
%	with a match from SourceConcept to a target concept from
%	TargetConceptScheme
%
%       Correspondence has a low confidence level,
%       to indicate that further checking will be needed to
%       evaluate the quality of this candidate.
%
%       The method will list which (pref/alt) labels properties
%       where used for the match.
%
%       For descriptions of possible Options, see above.

candidate(SourceConcept, TargetConceptScheme, Options, Correspondence) :-
	ground(SourceConcept),
	ground(TargetConceptScheme),
	ground(Options),
	option(labels_must_match(LabelMatch), Options, true),
	LabelMatch = true,
	rdf_has(SourceConcept, rdfs:label, literal(lang(_, Label)), RealLabel1Predicate),
	rdf_has(TargetConcept, rdfs:label, literal(lang(_, Label)), RealLabel2Predicate),
	rdf_has(TargetConcept, skos:inScheme, TargetConceptScheme),
	format(atom(Method), '~p-~p', [RealLabel1Predicate, RealLabel2Predicate]),
	CellOptions = [measure(0.001), % Only label match, this is just a candidate
		       method(Method)
		       |Options
		      ],
	create_cell(SourceConcept, TargetConcept, CellOptions, Correspondence).
