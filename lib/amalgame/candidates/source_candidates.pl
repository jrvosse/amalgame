:- module(source_candidates,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, carthesian_product(schemes(uri, uri), align(uri, uri, provendence_list), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate(schemes(SourceScheme, _), align(Source, _, []), Options) :-
 	rdf_has(Source, skos:inScheme, SourceScheme),
	(   option(exclude(Graph), Options)
	->  \+ member(align(Source,_,_), Graph)
	;   true
	).


