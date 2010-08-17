:- module(ag_compare,
	  [
	   % HTTP entry points:
	   http_list_alignments/1, % +Request
	   http_find_overlap/1,    % +Request

	   % misc hand predicates:
	   map_iterator/1,	   % -Map
	   has_map/3,              % +Map, -Format -Graph
	   find_graphs/2           % +Map, -GraphList

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


:- http_handler(amalgame(list_alignments),    http_list_alignments,     []).
:- http_handler(amalgame(find_overlap),       http_find_overlap,        []).

%%	http_list_alignments(+Request) is det.
%
%	HTTP handler returning list of all alignments in HTML.

http_list_alignments(_Request) :-
	clear_nicknames,
	reply_html_page(cliopatria(default),
			title('Alignments'),
			[ h4('Alignments in the RDF store'),
			  \show_alignments
			]).


%%	http_find_overlap(+Request) is det.
%
%	HTTP handler generating a page with mapping overlap statistics.

http_find_overlap(_Request) :-
	find_overlap(TotalNr, CountList),
	reply_html_page(cliopatria(default),
			[
			     title('Alignment overlap'),
			     style([type('text/css')],
				   ['#aligntable { padding: .3%; border: solid grey;  float: left }',
				    '#nicktable  { padding: .3%; border: dashed grey; float: left; margin-left: 5% }',
				    '#totals td  { border-top: solid grey; font-weight: bold }'
				   ])
			],
			[
			 h4('Alignment overlap'),
			 table([id(aligntable)],
			       [
				\show_countlist(CountList),
				tr([id(totals)],[td(TotalNr), td('Total')])
				]
			      ),
			 table([id(nicktable)],\show_alignments)
			]).

%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps to be compared. Map is currently of the
%	form [C1, C2], simply meaning there is a mapping from C1 to C2.
%	What other information is available about this mapping depends
%	on the format it is stored in, see has_map/3 for details.
%
%	This is a stub implementation.
%	@tbd make this configurable over a web interface so that we can
%	restrict the source and target vocabulary.

map_iterator([E1,E2]) :-
	has_map([E1, E2], _, _).

%%	has_map(+Map, -Format, -Graph) is non_det.
%
%	Intended to be used to find graphs that contain Map, and in what
%	Format. Map can be stored in the triple store in several
%	formats. We currently support the following formats:
%
%	* edoal: Alignment map format (EDOAL)
%	* skos: SKOS Mapping Relation
%       * dc: dc:replaces
%
%	@see EDOAL: http://alignapi.gforge.inria.fr/edoal.html

has_map([E1, E2], edoal, Graph) :-
	% FIXME: workaround rdf/4 index bug
	rdf(Cell, align:entity1, E1),
	rdf(Cell, align:entity1, E1, Graph),
	rdf(Cell, align:entity2, E2),
	rdf(Cell, align:entity2, E2, Graph).

has_map([E1, E2], skos, Graph) :-
	rdf_has(E1, skos:mappingRelation, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], dc, Graph) :-
	rdf_has(E1, dcterms:replaces, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

%%	find_graphs(+Map, -Graphs) is det.
%
%	Find all Graphs that have a mapping Map.

find_graphs(Map, Graphs) :-
	findall(Graph,
		has_map(Map, _, Graph:_),
		Graphs).

count_alignments(Format, Graph, Count) :-
	findall(Map, has_map(Map, Format, Graph), Graphs),
	length(Graphs, Count),!.

count_alignments(_,_,-1).

find_overlap(TotalNr, ResultsSorted) :-
	findall(Map, map_iterator(Map), AllMaps),
	length(AllMaps, TotalNr),
	find_overlaps(AllMaps, [], Overlaps),
	count_overlaps(Overlaps, [], Results),
	sort(Results, ResultsSorted).

find_overlaps([], Doubles, Uniques) :- sort(Doubles, Uniques).
find_overlaps([Map|Tail], Accum, Out) :-
	find_graphs(Map, Graphs),
	find_overlaps(Tail, [Graphs:Map|Accum], Out).

count_overlaps([], Results, Results).
count_overlaps([Graphs:_|T], Accum, Results) :-
	member(_:Graphs, Accum),!,
	count_overlaps(T, Accum, Results).
count_overlaps([Graphs:_|Tail], Accum, Results) :-
	findall(Graphs, member(Graphs:_, Tail), Members),
	length(Members, NrMembers),
	Count is NrMembers + 1,
	debug(compare, '~w: ~w', [Count, Graphs]),
	count_overlaps(Tail, [Count:Graphs|Accum], Results).


clear_nicknames :-
% work around bug in rdf/4
	% rdf_retractall(_, amalgame:nickname, _, amalgame_nicknames).
	rdf_retractall(_, amalgame:nickname, _).
has_nickname(Graph,Nick) :-
	% work around bug in rdf/4
	% rdf(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
	rdf(Graph, amalgame:nickname, literal(Nick)).
nickname(Graph, Nick) :-
	has_nickname(Graph,Nick), !.
nickname(Graph, Nick) :-
	coin_nickname(Graph, Nick),
	rdf_assert(Graph, amalgame:nickname, literal(Nick), amalgame_nicknames).
coin_nickname(_Graph, Nick) :-
	char_type(Nick, alpha),
	\+ has_nickname(_, Nick).

show_graph(Graph, Options) -->
	{
	 member(nick(true), Options),!,
	 nickname(Graph, Nick),
	 http_link_to_id(list_graph, [graph(Graph)], VLink)
	},
	html(a([href(VLink),title(Graph)],[Nick, ' '])).

show_graph(Graph, _Options) -->
	{
	 http_link_to_id(list_graph, [graph(Graph)], VLink)
	},
	html(a([href(VLink)],\turtle_label(Graph))).

show_countlist([]) --> !.
show_countlist([C:L|T]) -->
	html(tr([td(C), td(\show_graphs(L, [nick(true)]))])),
	show_countlist(T).


show_graphs([],_) --> !.
show_graphs([H|T], Options) -->
	show_graph(H, Options),
	show_graphs(T, Options).

show_nicknames -->
	{
	 findall(Nick:Graph, has_nickname(Graph,Nick), AllNicks),
	 sort(AllNicks, Nicks)
	},
	html(tr([th('Legend'), th('Graph')])),
	show_nicknames(Nicks).

show_nicknames([]) --> !.
show_nicknames([Nick:Graph|Tail]) -->
	html(tr([td(Nick), td(\show_graph(Graph, [nick(false)]))])),
	show_nicknames(Tail).

show_alignments -->
	{
	 findall(Format:Graph,
		 has_map(_, Format,Graph:_),
		 Graphs),
	 sort(Graphs, UniqueGraphs),
	 findall(Count:Format:Graph,
		 (   member(Format:Graph, UniqueGraphs),
		     count_alignments(Format, Graph, Count)
		 ),
		 CountedGraphs),
	 sort(CountedGraphs, SortedGraphs)
	},
	html(table([tr([
			th('abbrev'),
			th(format),
			th('# maps'),
			th('named graph')
		       ]),
		    \show_alignments(SortedGraphs)
		   ])).

show_alignments([]) --> !.
show_alignments([Count:Format:Graph|Tail]) -->
	html(tr([
		 td(\show_graph(Graph, [nick(true)])),
		 td(Format),
		 td(Count),
		 td(\show_graph(Graph, [nick(false)]))
		])),
	show_alignments(Tail).







