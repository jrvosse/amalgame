:- module(unmapped_candidate,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/map)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate, unmapped_candidate(align_source, align_source, align(uri, uri, provendence_list), [])).

%%	candidate(+Source, +Target, -Alignment, +Options)
%
%	Enumerate over members of Source and Target that have not been mapped.

candidate(Source, Target, A, _Options) :-
	(   ground(A)
	->  A = align(S, T, _)
	;   A = align(S, T, [])
	),
	graph_member(S, Source),
	\+ has_map(S,_,_),
	graph_member(T, Target),
	\+ has_map(_,T,_).
