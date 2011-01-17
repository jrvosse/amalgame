:- module(source_candidates,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, source_candidates(schemes(uri, uri), align(uri, uri, provendence_list), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate(schemes(SourceScheme, _), align(Source, _, ProvList), _Options) :-
	(   var(ProvList)
	->  ProvList = []
	;   true
	),
	rdf_has(Source, skos:inScheme, SourceScheme).



