:- module(arity_select,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/partition/source_ambiguity)).
:- use_module(library(amalgame/partition/target_ambiguity)).

:- public selecter/5.
:- public partition/3.

%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%

selecter(AlignmentGraph, Sel, Dis, [], Options) :-
	predsort(compare_align(sourceplus), AlignmentGraph, SourceSorted),
	predsort(compare_align(targetplus), AlignmentGraph, TargetSorted),

	target_ambiguity:partition(SourceSorted, Lsource, Options),
	source_ambiguity:partition(TargetSorted, Ltarget, Options),

	memberchk(unambiguous(Source1), Lsource),
	memberchk(  ambiguous(SourceN),	Lsource),
	memberchk(unambiguous(Target1), Ltarget),
	memberchk(  ambiguous(TargetN),	Ltarget),

	sort(Source1, S1), sort(SourceN, SN),
	sort(Target1, T1), sort(TargetN, TN),

	ord_intersection(S1, T1, Sel),
	ord_union(SN, TN, Dis),

	% length(SourceSorted, SSN),
	% length(TargetSorted, TSN),
	% length(Source1, S1N),
	% length(SourceN, SNN),
	% length(Target1, T1N),
	% length(TargetN, TNN),
	% length(AlignmentGraph, N0),
	% length(Sel, SelN),
	% length(Dis, DisN),
	% 
	% debug(align, 'arity select on AlignmentGraph of length: ~w', N0),
	% debug(align, 'S1/SN: ~w/~w T1/TN: ~w/~w', [S1N, SNN, T1N, TNN]),
	% debug(align, 'SS/TS: ~w/~w', [SSN, TSN]),
	% debug(align, 'Sel/Dis: ~w/~w', [SelN, DisN]),
	true.


partition(AlignmentGraph, Partition, Options) :-
	% Hack: All parts are source-sorted, except for PN1,
	% (which we need to be target-sorted in the next step anyway ...)
	Partition = [p11(P11), pxn(P1N), pnx(PN1), pnm(PNM)],

	predsort(compare_align(sourceplus), AlignmentGraph, SourceSorted),
	predsort(compare_align(targetplus), AlignmentGraph, TargetSorted),

	target_ambiguity:partition(SourceSorted, Lsource, Options),
	source_ambiguity:partition(TargetSorted, Ltarget, Options),

	memberchk(unambiguous(Source1), Lsource), % ? to 1
	memberchk(  ambiguous(SourceN),	Lsource), % ? to N
	memberchk(unambiguous(Target1), Ltarget), % 1 to ?
	memberchk(  ambiguous(TargetN),	Ltarget), % N to ?

	sort(Source1, S1), sort(SourceN, P1N),
	sort(Target1, T1), sort(TargetN, PN1),

	ord_intersection(S1,  T1,   P11),		  % 1 to 1
	ord_intersection(P1N, PN1, PNM).	          % N to M seed set
	% ord_subtract(SN, PNM, P1N),
	% ord_subtract(TN, PNM, PN1).

	/*
	predsort(compare_align(targetplus), PNM_seed, PNM_seed_tsorted),
	% Move related '? to N' to S ext, remainder is 1 to N part:
	extend_same_sources(PNM_seed, SN, S_ext, P1N),
	% Move related 'N to ?' to T ext, remainder is N to 1 part:
	extend_same_targets(PNM_seed_tsorted, TargetN, T_ext, PN1),

	% sort(PN1_tsorted, PN1),
	sort(S_ext, S_ext_sorted),
	sort(T_ext, T_ext_sorted),
	ord_union(S_ext_sorted, T_ext_sorted, PNM_nocluster),
	predsort(compare_align(targetplus), PNM_nocluster, PNM_nocluster_tsorted),

	cluster_pnm(PNM_nocluster, PNM_nocluster_tsorted, PNM).
	% group_pairs_by_key(PNMpre, PNMgrouped),
	% pairs_values(PNMgrouped, PNMuf),
	% maplist(flatten, PNMuf, PNM).
*/

extend_same_sources([], L, [], L) :- !.
extend_same_sources(L, [], L, []) :- !.
extend_same_sources([Seed1|SeedTail], [Source1|SourceTail], Ext, Rest) :-
	compare_align(source, Order, Seed1, Source1),
	(   Order == <
	->  extend_same_sources(SeedTail, [Source1|SourceTail], Ext0, Rest),
	    Ext = [Seed1|Ext0]
	;   Order == >
	->  extend_same_sources([Seed1|SeedTail], SourceTail, Ext, Rest0),
	    Rest = [Source1|Rest0]
	;   Order == =
	->  Source1 = align(S1,_,_),
	    same_source(SeedTail, S1, Same1, Rest1),
	    same_source(SourceTail, S1, Same2, Rest2),
	    extend_same_sources(Rest1, Rest2, ExtR, Rest),
	    append([[Seed1|Same1],[Source1|Same2] , ExtR], ExtPre),
	    flatten(ExtPre, ExtUn),
	    sort(ExtUn, Ext)
	).

extend_same_targets([], L, [], L) :- !.
extend_same_targets(L, [], L, []) :- !.
extend_same_targets([H1|T1], [H2|T2], Ext, Rest) :-
	compare_align(target, Order,H1, H2),
	(   Order == <
	->  extend_same_targets(T1, [H2|T2], Ext0, Rest),
	    Ext = [H1|Ext0]
	;   Order == >
	->  extend_same_targets([H1|T1], T2, Ext, Rest0),
	    Rest = [H2|Rest0]
	;   Order == =
	->  H1 = align(_,Targ1,_),
	    same_target(T1, Targ1, Same1, Rest1),
	    same_target(T2, Targ1, Same2, Rest2),
	    extend_same_targets(Rest1, Rest2, ExtR, Rest),
	    append([[H1|Same1],[H2|Same2]], Cluster0),
	    predsort(compare_align(targetplus), Cluster0, Cluster),
	    ExtPre = [Cluster|ExtR], flatten(ExtPre, Ext)
	).
same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,
	same_target(As, T, Same, Rest).
same_target(As, _T, [], As).



