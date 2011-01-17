:- module(source_candidates,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, source_candidates(schemes(uri, uri), align(uri, uri, provendence_list), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate(schemes(SourceScheme, _), A, Options) :-
	(   ground(A)
	->  A = align(Source, _, _)
	;   A = align(Source, _, [])
	),
 	rdf_has(Source, skos:inScheme, SourceScheme),
	(   option(exclude(Graph), Options)
	->  \+ member(align(Source,_,_), Graph)
	;   true
	).

