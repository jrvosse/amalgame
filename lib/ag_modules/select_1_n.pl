:- module(select_1_n,[]).

:- use_module(library(semweb/rdf_db)).

:- public amalgame_module/1.
:- public selecter/5.

amalgame_module(amalgame:'Select1N').


%%	selecter(+Mapping, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the correspondance where a target
%	concept is mapped to only one source.

selecter(Mapping, Sel, Dis, [], _Options) :-
	predsort(ag_map:compare_align(targetplus), Mapping, TargetSorted),
 	select_(TargetSorted, Sel0, Dis0),
	sort(Sel0, Sel),
	sort(Dis0, Dis).


select_([], [], []).
select_([align(S,T,P)|As], A1, A2) :-
 	same_target(As, T, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_(Rest, A1Rest, A2Rest).

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,
	same_target(As, T, Same, Rest).
same_target(As, _T, [], As).
