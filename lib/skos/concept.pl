:- module(ag_skos_concept, [
			    assert_concept_stats/2
			   ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).
:- use_module(vocabularies).

assert_concept_stats(Voc, Graph) :-
	ground(Voc),
	ground(Graph),
	(   rdf_graph(Graph) -> rdf_unload(Graph); true),
	topconcepts(Voc, TopConcepts),
	forall(member(Top, TopConcepts),
	       assert_subtree_stats(Top, [graph(Graph)], _Stats)
	      ).

mapped(Concept, Mapped) :-
	(   ( has_map([Concept, _], _, _)
	    ; has_map([_, Concept], _, _)
	    )
	->  Mapped = 1
	;   Mapped = 0
	),!.


assert_subtree_stats(Root, Options, []) :-
	option(path(Path), Options),
        memberchk(Root, Path), % Oops, cycle, done this already
        !,
        debug(stats, 'Cycle detected for ~w~n', [Root]),
        rdf_assert(Root, amalgame:cycle, literal(true), stats),
	fail.

assert_subtree_stats(Root, Options, []) :-
	rdf(Root, amalgame:height, _, Graph),% oops, multiple parents, done this already
	option(graph(Graph), Options, amalgame_concepts),
	!,
	fail.

assert_subtree_stats(Root, Options, Stats) :-
	\+ has_child(Root, _), % No child, this is a leave node ...
	option(graph(Graph), Options, amalgame_concepts),
	option(depth(Depth), Options, -1),

	mapped(Root, Mapped),
	rdf_assert(Root, amalgame:height,    literal(type(xsd:int, 0)),      Graph),
	rdf_assert(Root, amalgame:depth,     literal(type(xsd:int, Depth)),  Graph),
	rdf_assert(Root, amalgame:isMapped, literal(type(xsd:int, Mapped)), Graph),

	Stats = [max_height(0),
		 avg_height(0.0),
		 min_height(0),
		 size(1),
		 mapped(Mapped)
		],
	!.



assert_subtree_stats(Root, Options, Stats) :-
	% Assume we have a normal Concept with #children > 0
	option(depth(Depth), Options, 0),
	option(graph(Graph), Options, amalgame_concepts),
	option(path(Path), Options, []),
	NewPath = [Root|Path],
	succ(Depth, NewDepth),

	merge_options([depth(NewDepth), path(NewPath)], Options, ChildOptions),


	findall(Child, has_child(Root, Child), Children),
	findall(ChildStat,
		(member(Child, Children),
		 assert_subtree_stats(Child, ChildOptions, ChildStat)
		),
	       ChildStatsList
	       ),
	mapped(Root, Mapped),
	rdf_assert(Root, amalgame:isMapped, literal(type(xsd:int, Mapped)), Graph),
	calculate_child_stats([mapped(Mapped)|ChildStatsList], Stats),
	assert_stats(Root, Options, [depth(Depth)|Stats]),
	!.

assert_stats(_Node, _, []).
assert_stats(Node, Options, Stats) :-
	option(graph(Graph), Options, amalgame_concepts),
        debug(stats, 'Stats: ~w~n', [Stats]),
        option(depth(Depth), Stats),
        option(size(Size), Stats),
        option(max_height(MaxHeight), Stats),
        option(min_height(MinHeight), Stats),
        option(avg_height(AvgHeight), Stats),
	option(mapped(Mapped), Stats),
        rdf_assert(Node, amalgame:depth,     literal(type(xsd:int, Depth)),     Graph),
        rdf_assert(Node, amalgame:size,      literal(type(xsd:int, Size)),      Graph),
        rdf_assert(Node, amalgame:maxHeight, literal(type(xsd:int, MaxHeight)), Graph),
        rdf_assert(Node, amalgame:minHeight, literal(type(xsd:int, MinHeight)), Graph),
        rdf_assert(Node, amalgame:avgHeight, literal(type(xsd:float, AvgHeight)), Graph),
        rdf_assert(Node, amalgame:mapped,    literal(type(xsd:int, Mapped)), Graph).

calculate_child_stats([[]], []).
calculate_child_stats(List, Stats) :-
        flatten(List, FlatList),
        findall(Max, member(max_height(Max), FlatList), Maxes),
        max_list(Maxes, MaxHeight),

        findall(Min, member(min_height(Min), FlatList), Mins),
        min_list(Mins, MinHeight),

        findall(Avg, member(avg_height(Avg), FlatList), Avgs),
        sumlist(Avgs, AvgSum),
        length(Avgs, Length),
        AvgHeight is AvgSum/Length,

        findall(Size, member(size(Size), FlatList), Sizes),
        sumlist(Sizes, SizeSum),

	findall(Mapped, member(mapped(Mapped), FlatList), Mappeds),
        sumlist(Mappeds, MappedSum),

        AvgHeight1 is AvgHeight + 1,
        MaxHeight1 is MaxHeight + 1,
        MinHeight1 is MinHeight + 1,
        TotalSize  is SizeSum + 1,

        Stats=[max_height(MaxHeight1),
               min_height(MinHeight1),
               avg_height(AvgHeight1),
               size(TotalSize),
	       mapped(MappedSum)
              ].

has_child(Root, Child) :-
	rdf_has(Child, skos:broader, Root).


