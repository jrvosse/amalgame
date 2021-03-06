:- module(ag_reference,
	  [ reference_mappings/2,
	    is_reference_mapping/2
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(amalgame/expand_graph)).

%%	reference_mappings(+Strategy, -References) is det.
%
%	Reference contains all reference (ground truth) correspondences
%	for Strategy or the empty list if no refs are available.

reference_mappings(Strategy, References) :-
	findall(R, is_reference_mapping(Strategy, R), RefGraphs),
	maplist(expand_node(Strategy), RefGraphs, MappingLists),
	append(MappingLists, References0),
	sort(References0, References).

%%	is_reference(?Strategy, ?Reference) is nondet.
%%	is_reference(+Strategy, +Reference) is semidet.
%
%	Evaluates to true if Reference is a reference graph for
%	Strategy.
is_reference_mapping(Strategy, Reference) :-
	rdf(Reference, amalgame:status, amalgame:reference, Strategy).
