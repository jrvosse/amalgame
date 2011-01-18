:- module(source_candidate,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate, source_candidate(align_source, align_source, align(uri, uri, provendence_list), [])).

%%	candidate(+Source, +Target, -Alignment, +Options)
%
%	Enumerate over members of Source.

candidate(Source, _Target, A, _Options) :-
	(   ground(A)
	->  A = align(S, _, _)
	;   A = align(S, _, [])
	),
	graph_member(S, Source).
