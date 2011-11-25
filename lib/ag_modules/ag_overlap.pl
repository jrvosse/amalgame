:- module(overlap_analyzer,
	  [ overlap/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).

:- use_module(library(amalgame/expand_graph)).

:- public amalgame_module/1.
:- public analyzer/5.

amalgame_module(amalgame:'OverlapComponent').

analyzer(Inputs, Process, Strategy, overlap(Results), _Options) :-
	maplist(expander(Strategy), Inputs, ExpandedInputs),
	overlap(ExpandedInputs, Overlaps),
	maplist(create_overlap_output(Process, Strategy), Overlaps, Results).

expander(Strategy, Id, Id:Expanded) :-
	expand_mapping(Strategy, Id, Expanded).

create_overlap_output(Process, Strategy, OverlapId-Mapping, OutputUri-MappingFlat) :-
	rdf_equal(Type, amalgame:'Mapping'),
	rdf_equal(Pred, opmv:wasGeneratedBy),
	append(Mapping, MappingFlat),
	new_output(Type, Process, Pred, Strategy, OutputUri),
	format(atom(Label), 'Mappings found only in: ~p', [OverlapId]),
	rdf_assert(OutputUri, rdfs:comment, literal(Label), Strategy).

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
	findall(align(S,T,E),  member((_I:align(S,T,E)), L), Es),
	my_transpose_pairs(Tail, Result).
create_pairs([], []).
create_pairs([Id:L1|T], [P1|PT]) :-
	create_pairs(Id, L1, P1),
	create_pairs(T,PT).

create_pairs(_, [], []).
create_pairs(Id, [align(S,T,E)|Tail], [H|Pairs]) :-
	H = (S:T)-(Id:align(S,T,E)),
	create_pairs(Id, Tail, Pairs).

