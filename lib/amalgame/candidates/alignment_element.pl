:- module(alignment_element,
	 []).

:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, alignment_element(alignment_graph, align(uri, uri, provendence_list), [])).

%%	candidate(+Input, -Output, +Options)
%
%

candidate(Graph, A, _Options) :-
	is_list(Graph),
	!,
	member(A, Graph).



