:- module(target_candidates, []).

:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, target_candidates(schemes(uri, uri), align(uri, uri, provendence_list), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate(schemes(_, TargetScheme), align(_, Target, []), _Options) :-
	rdf_has(Target, skos:inScheme, TargetScheme).


