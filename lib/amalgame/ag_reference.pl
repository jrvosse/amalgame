:- module(ag_reference,
	  [ reference_mappings/2,
	    is_reference/2
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/expand_graph)).

%%	reference_mappings(+Strategy, -References) is det.
%
%	Reference contains all reference (ground truth) correspondences
%	for Strategy or the empty list if no refs are available.

reference_mappings(Strategy, References) :-
	findall(R, is_reference(Strategy, R), RefGraphs),
	maplist(expand_node(Strategy), RefGraphs, MappingLists),
	append(MappingLists, References).

%%	is_reference(?Strategy, ?Reference) is nondet.
%%	is_reference(+Strategy, +Reference) is semidet.
%
%	Evaluates to true if Reference is a reference graph for
%	Strategy.
is_reference(Strategy, Reference) :-
	rdf(Reference, amalgame:status, amalgame:reference, Strategy).
