:- module(select_1_1,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.

amalgame_module(amalgame:'Select11').

%%	selecter(+Mapping, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the unique correspondences between
%	a source and target concept.

selecter(Mapping, Sel, Dis, [], Options) :-
	select_n_1:selecter(Mapping, Source1, SourceN, _, Options),
	select_1_n:selecter(Mapping, Target1, TargetN, _, Options),

	sort(Source1, S1), sort(SourceN, SN),
	sort(Target1, T1), sort(TargetN, TN),

	ord_intersection(S1, T1, Sel),
	ord_union(SN, TN, Dis).
