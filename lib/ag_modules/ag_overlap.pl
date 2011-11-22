:- module(overlap_analyzer,
	  [ overlap/2
	  ]).

%:- use_module(library(semweb/rdf_db)).
%:- use_module(library(semweb/rdf_label)).
%:- use_module(library(amalgame/vocabulary)).

:- use_module(library(amalgame/expand_graph)).

:- public amalgame_module/1.

amalgame_module(amalgame:'Overlap').

test(Overlaps) :-
	Strategy='http://localhost/ns/strategy1',
	Id1='http://localhost/ns/dataset5',
	Id2='http://localhost/ns/dataset6',

	expand_mapping(Strategy, Id1, Mapping1),
	expand_mapping(Strategy, Id2, Mapping2),

	overlap([Id1:Mapping1, Id2:Mapping2], Overlaps),
	print_overlaps(Overlaps).

print_overlaps([]).
print_overlaps([Key-Value|T]) :-
	length(Value, ValueN),
	format('~w: ~w~n', [Key, ValueN]),
	print_overlaps(T).

overlap(MappingList, Overlaps) :-
	create_pairs(MappingList, Pairs),
	append(Pairs, FlatPairs),
	length(FlatPairs, FlatPairsN),
	debug(ag_overlap, 'total mappings for overlap: ~w', [FlatPairsN]),
	keysort(FlatPairs, SortedPairs),
	group_pairs_by_key(SortedPairs, GroupedPairs),
	my_transpose_pairs(GroupedPairs, TransposedPairs),
	keysort(TransposedPairs,TransposedPairsSorted),
	group_pairs_by_key(TransposedPairsSorted, Overlaps).

my_transpose_pairs([], []).
my_transpose_pairs([(S:T)-L|Tail], [IDs-Es|Result]) :-
	findall(Id, member((Id:align(S,T,_)), L), IDs),
	findall(E,  member((_I:align(S,T,E)), L), Es),
	my_transpose_pairs(Tail, Result).
create_pairs([], []).
create_pairs([Id:L1|T], [P1|PT]) :-
	create_pairs(Id, L1, P1),
	create_pairs(T,PT).

create_pairs(_, [], []).
create_pairs(Id, [align(S,T,E)|Tail], [H|Pairs]) :-
	H = (S:T)-(Id:align(S,T,E)),
	create_pairs(Id, Tail, Pairs).

