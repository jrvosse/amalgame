:- module(overlap_analyzer,
	  [
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public analyzer/5.

amalgame_module(amalgame:'OverlapComponent').

analyzer(Inputs, Process, Strategy, overlap(Results), _Options) :-
	maplist(input_expander(Strategy), Inputs, ExpandedInputs),
	overlap(ExpandedInputs, Overlaps),
	maplist(output_expander(Process, Strategy), Overlaps, Results).

input_expander(Strategy, Id, Id:Expanded) :-
	expand_mapping(Strategy, Id, Expanded, _).

output_expander(Process, Strategy, OverlapId-Mapping, OutputUri-MappingMerged) :-
	append(Mapping, MappingFlat),
	merge_provenance(MappingFlat, MappingMerged),
	rdf(OutputUri, opmv:wasGeneratedBy, Process, Strategy),
	rdf(OutputUri, amalgame:overlap_set, literal(OverlapId), Strategy),!.

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

