:- module(ag_reference,
	  [ reference_mappings/2
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
	findall(R, rdf(R, amalgame:status, amalgame:reference, Strategy), RefGraphs),
	maplist(expand_node(Strategy), RefGraphs, MappingLists),
	append(MappingLists, References).
