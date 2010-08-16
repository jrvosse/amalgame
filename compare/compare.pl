:- module(ag_compare,
	  [
	   find_overlap/1
	  ]
	 ).

/** <module> Amalgame compare mapping module

This module compares mappings as they are found by different matchers.
It assumes matchers assert mappings in different name graphs.

@author Jacco van Ossenbruggen
@license GPL
*/

:- use_module(library(assoc)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).

:- use_module(components(label)).
:- use_module('../namespaces').


:- http_handler(amalgame(list_alignments),    list_alignments,     []).

%%	has_map(+E1, +E2, -Graph) is non_det.
%
%	Intended to be used to find graphs that contain a mapping from
%	E1 to E2 in one of the following formats:
%	* Alignment map format (EDOAL)
%	* SKOS Mapping Relation
%	* dc:replaces
%
%	@see EDOAL: http://alignapi.gforge.inria.fr/edoal.html

has_map([E1, E2], Graph) :-
	rdf(Cell, align:entity1, E1, Graph),
	rdf(Cell, align:entity2, E2, Graph).

has_map([E1, E2], Graph) :-
	rdf_has(E1, skos:mappingRelation, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], Graph) :-
	rdf_has(E1, dcterms:replaces, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps to be compared.
%
%	This is a stub implementation.
%	TBD: make this configurable over a web interface

map_iterator([E1,E2]) :-
	has_map([E1, E2], _).


%%	find_graphs(+Map, -Graphs) is det.
%
%	Find all Graphs that have a mapping Map.

find_graphs(Map, Graphs) :-
	findall(Graph,
		has_map(Map, Graph:_),
		Graphs).


%%	find_overlap(-Statistics) is det.
%
%	Find overlap statistics for a predefined mapping.

find_overlap([TotalNr, CountList]) :-
	find_overlap(TotalNr, ResultAssoc),
	assoc_to_keys(ResultAssoc, KeyList),
	maplist(count_mappings(ResultAssoc), KeyList, CountList).

find_overlap(TotalNr, ResultAssoc) :-
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, TotalNr),
	empty_assoc(EmptyAssoc),
	find_overlap(AllMaps, EmptyAssoc, ResultAssoc).

find_overlap([], Assoc, Assoc).

find_overlap([Map|Tail], In, Out) :-
	find_graphs(Map, Graphs),
	(   get_assoc(Graphs, In, OldMappings)
	->  append(OldMappings, [Map], NewMappings)
	;   NewMappings = [Map]
	),
	Map = [E1, E2],
	debug(compare, '~p ~p: ~w', [E1, E2, Graphs]),
	put_assoc(Graphs, In, NewMappings, NewAssoc),
	find_overlap(Tail, NewAssoc, Out).

count_mappings(Assoc, Key, Key:Count) :-
	get_assoc(Key, Assoc, MapList),
	length(MapList, Count).

list_alignments(_Request) :-
	findall(Graph, has_map(_, Graph:_), Graphs),
	sort(Graphs, SortedGraphs),
	reply_html_page(cliopatria(default),
			title('Alignments'),
			[ h4('Alignments in the RDF store'),
			  ol([\show_alignments(SortedGraphs)])
			]).


show_alignments([]) --> !.
show_alignments([H|Tail]) -->
	{
	 http_link_to_id(list_graph, [graph(H)], VLink)
	},
	html(li(a([href(VLink)],\turtle_label(H)))),
	show_alignments(Tail).


