:- module(ag_rdf_util, [
	      cp_graphs/2,
	      cp_graph/3
	  ]).

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).


%%	cp_graphs(+GraphList, Target) is det.
%
%	Copy all triples in the named graphs in GraphList to the named
%	graph Target.

cp_graphs([], _Target) :- !.
cp_graphs([Head|Tail], Target) :-
	cp_graph(Head, Target, false),
	cp_graphs(Tail, Target).

%%	cp_graph(+Source, +Target, +Overwrite) is det.
%
%	Copy all triples from Source to Target.
%	If Overwrite is true, existing triples in Target are removed
%	first.

cp_graph(Source, Target, true) :-
	rdf_unload_graph(Target), % Delete old graphs under the same name
	cp_graph(Source, Target, false).

cp_graph(Source, Target, false) :-
	findall(rdf(S,P,O), rdf(S,P,O,Source), Triples),
	forall(member(rdf(S,P,O), Triples),
	       rdf_assert(S,P,O,Target)).

