:- module(ag_compare,
	  [
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
:- http_handler(amalgame(find_overlap),       find_overlap,        []).

%%	has_map(+E1, +E2, -Format, -Graph) is non_det.
%
%	Intended to be used to find graphs that contain a mapping from
%	E1 to E2 in one of the following formats:
%	* edoal: Alignment map format (EDOAL)
%	* skos:  SKOS Mapping Relation
%	* dc:    dc:replaces
%
%	@see EDOAL: http://alignapi.gforge.inria.fr/edoal.html

has_map([E1, E2], edoal, Graph) :-
	rdf(Cell, align:entity1, E1, Graph),
	rdf(Cell, align:entity2, E2, Graph).

has_map([E1, E2], skos, Graph) :-
	rdf_has(E1, skos:mappingRelation, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], dc, Graph) :-
	rdf_has(E1, dcterms:replaces, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps to be compared.
%
%	This is a stub implementation.
%	TBD: make this configurable over a web interface

map_iterator([E1,E2]) :-
	has_map([E1, E2], _, _).


%%	find_graphs(+Map, -Graphs) is det.
%
%	Find all Graphs that have a mapping Map.

find_graphs(Map, Graphs) :-
	findall(Graph,
		has_map(Map, _, Graph:_),
		Graphs).

count_alignments(Format, Graph, Count) :-
	findall(Map, has_map(Map, Format, Graph), Graphs),
	length(Graphs, Count).

count_alignments(_,_,-1).

find_overlap(TotalNr, ResultAssoc) :-
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, TotalNr),
	debug(compare, 'Found ~w mappings', [TotalNr]),
	empty_assoc(EmptyAssoc),
	find_overlap(AllMaps, EmptyAssoc, ResultAssoc).

spyme.
find_overlap([], Assoc, Assoc) :- spyme.

find_overlap([Map|Tail], In, Out) :-
	find_graphs(Map, Graphs),
	(   get_assoc(Graphs, In, OldMappings)
	->  append(OldMappings, [Map], NewMappings)
	;   NewMappings = [Map]
	),
	put_assoc(Graphs, In, NewMappings, NewAssoc),
	find_overlap(Tail, NewAssoc, Out).

count_mappings(Assoc, Key, Key:Count) :-
	get_assoc(Key, Assoc, MapList),
	length(MapList, Count).

list_alignments(_Request) :-
	findall(Format:Graph, has_map(_, Format,Graph:_), Graphs),
	sort(Graphs, SortedGraphs),
	reply_html_page(cliopatria(default),
			title('Alignments'),
			[ h4('Alignments in the RDF store'),
			  table([tr([th(format),
				     th('# maps'),
				     th('named graph')
				    ]),
				 \show_alignments(SortedGraphs)])
			]).


show_alignments([]) --> !.
show_alignments([Format:Graph|Tail]) -->
	{
	 count_alignments(Format, Graph, Count)
	},
	html(tr([td(Format),
		 td(Count),
		 td(\show_graph(Graph))
		])),
	show_alignments(Tail).


show_graph(Graph) -->
	{
	 http_link_to_id(list_graph, [graph(Graph)], VLink)
	},
	html(a([href(VLink)],\turtle_label(Graph))).

%%	find_overlap(+Request) is det.
%
%	Find overlap statistics for a predefined mapping.

find_overlap(_Request) :-
	find_overlap(TotalNr, ResultAssoc),
	assoc_to_keys(ResultAssoc, KeyList),
	maplist(count_mappings(ResultAssoc), KeyList, CountList),
	reply_html_page(cliopatria(default),
			title('Alignment overlap'),
			[
			 h4('Alignment overlap'),
			 p([TotalNr]),
			 table(\show_countlist(CountList))
			]).

show_countlist([]) --> !.
show_countlist([L:C|T]) -->
	html(tr([td(C), td(\show_graphs(L))])),
	show_countlist(T).


show_graphs([]) --> !.
show_graphs([H|T]) -->
	show_graph(H),
	show_graphs(T).
