:- module(alignment_element,
	 []).

:- use_module(library(semweb/rdf_db)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate, alignment_element(align_source, align_source, align(uri, uri, provendence_list), [])).

%%	candidate(+Source, ?Target, +Alignment, +Options)
%
%

candidate(List, _, A, _Options) :-
	is_list(List),
	!,
	member(A, List).



