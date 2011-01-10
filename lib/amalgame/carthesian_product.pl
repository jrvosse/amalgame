:- module(carthesian_product,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).

:- public candidate_generator/3.
:- multifile amalgame:component/2.

amalgame:component(candidate_generator, carthesian_product(schemes(uri, uri), candidate(uri, uri), [])).
amalgame:component(candidate_generator, carthesian_product(graphs(uri, uri), candidate(uri, uri), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate_generator(schemes(SourceScheme, TargetScheme), candidate(Source, Target), _Options) :-
	rdf_has(Source, skos:inScheme, SourceScheme),
	rdf_has(Target, skos:inScheme, TargetScheme).

candidate_generator(graphs(SourceGraph, TargetGraph), candidate(Source, Target), _Options) :-
	rdf(Source, rdf:type, skos:'Concept', SourceGraph),
	rdf(Target, rdf:type, skos:'Concept', TargetGraph).


