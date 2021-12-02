:- module(skos_browser_hooks, []).

:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/expand_graph)).

cliopatria:concept_property(class, Concept, Graphs0, Class, Options) :-
	graph_mappings(Graphs0, Graphs, Options),
	(   is_mapped(Concept, Graphs, Options)
	->  Class = mapped
	;   Class = unmapped
	).
cliopatria:concept_property(count, Concept, Graphs0, Count, Options) :-
	graph_mappings(Graphs0, Graphs, Options),
	mapped_descendant_count(Concept, Graphs, Count, Options).

graph_mappings([], Graphs, Options) :-
	option(strategy(Strategy), Options), !,
	graph_mappings([Strategy], Graphs, Options).

graph_mappings([Strategy], Graphs, _Options) :-
	rdf(Strategy, rdf:type, amalgame:'AlignmentStrategy'),
	!,
	findall(Mapping, rdf(Mapping, rdf:type, amalgame:'Mapping', Strategy), Graphs).
graph_mappings(Graphs, Graphs, _Options).


mapped_descendant_count(Concept, Graphs, Count, Options) :-
	findall(C, skos_descendant_of(Concept, C), Descendants0),
	sort(Descendants0, Descendants),
	(   Descendants	= []
	->  Count = @(null)
	;   mapped_chk(Descendants, Graphs, Mapped, Options),
	    length(Descendants, Descendant_Count),
	    length(Mapped, Mapped_Count),
	    atomic_list_concat([Mapped_Count, '/', Descendant_Count], Count)
	).

mapped_chk([], _, [], _ ).
mapped_chk([C|T], Graphs, [C|Rest], Options) :-
	is_mapped(C, Graphs, Options),
	!,
	mapped_chk(T, Graphs, Rest, Options).
mapped_chk([_|T], Graphs, Rest, Options) :-
	mapped_chk(T, Graphs, Rest, Options).

is_mapped(Concept, Mappings, Options) :-
	option(strategy(Strategy), Options),
	Type=source,
	all_mapped(Strategy, Type, Mappings, Concepts, _Sorted),
	get_assoc(Concept, Concepts, _Value),!.
