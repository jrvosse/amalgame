:- module(arity_select,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/partition/source_ambiguity)).
:- use_module(library(amalgame/partition/target_ambiguity)).

:- public amalgame_module/1.
:- public selecter/5.
:- public input/1.

amalgame_module(amalgame:'AritySelect').
input(amalgame:'Mapping').


%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%

selecter(AlignmentGraph, Sel, Dis, [], Options) :-
	predsort(ag_map:compare_align(sourceplus), AlignmentGraph, SourceSorted),
	predsort(ag_map:compare_align(targetplus), AlignmentGraph, TargetSorted),

	target_ambiguity:partition(SourceSorted, Lsource, Options),
	source_ambiguity:partition(TargetSorted, Ltarget, Options),

	memberchk(unambiguous(Source1), Lsource),
	memberchk(  ambiguous(SourceN),	Lsource),
	memberchk(unambiguous(Target1), Ltarget),
	memberchk(  ambiguous(TargetN),	Ltarget),

	sort(Source1, S1), sort(SourceN, SN),
	sort(Target1, T1), sort(TargetN, TN),

	ord_intersection(S1, T1, Sel),
	ord_union(SN, TN, Dis).
