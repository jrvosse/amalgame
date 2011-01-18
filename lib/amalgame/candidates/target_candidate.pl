:- module(target_candidate,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate, target_candidate(align_source, align_source, align(uri, uri, provendence_list), [])).

%%	candidate(+Source, +Target, -Alignment, +Options)
%
%	Enumerate over members of Target.

candidate(_Source, Target, A, _Options) :-
	(   ground(A)
	->  A = align(_, T, _)
	;   A = align(_, T, [])
	),
	graph_member(T, Target).
