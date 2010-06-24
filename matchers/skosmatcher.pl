:- module(skosmatcher,
	  [skos_find_candidates/4 % +SourceConcept, +TargetScheme, +Options, -Results
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module('../mapping_io/edoal').

%%	skos_find_correspondences(+C, +S, +Options, -Result) is det.
%
%	Return all correspondences candidates for for SKOS concept C
%	from SKOS scheme S. Result is an RDF graph in (extended) EDOAL format

skos_find_candidates(SourceConcept, TargetConceptScheme, Options, Correspondences):-
	ground(SourceConcept),
	ground(TargetConceptScheme),
	ground(Options),
	findall(Correspondence,
		is_correspondence(SourceConcept, TargetConceptScheme, Options, Correspondence),
		Correspondences
	       ).

is_correspondence(SourceConcept, TargetConceptScheme, Options, Correspondence) :-
	option(labels_must_match(LabelMatch), Options, true),
	LabelMatch = true,
	rdf_has(SourceConcept, rdfs:label, literal(Label)),
	forall((rdf_has(TargetConcept, rdfs:label, literal(Label)),
		rdf_has(TargetConcept, skos:inScheme, TargetConceptScheme)
	       ),
	       create_cell(SourceConcept, TargetConcept, Options, Correspondence)
	      ),
	true.
