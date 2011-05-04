:- module(select_1_n,[]).

:- use_module(library(semweb/rdf_db)).

:- public amalgame_module/1.
:- public selecter/5.

amalgame_module(amalgame:'Select1N').


%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the correspondance where each target
%	concept is mapped to only one source.

selecter(AlignmentGraph, Sel, Dis, [], _Options) :-
 	select_(AlignmentGraph, Sel, Dis).


select_([], [], []).
select_([align(S,T,P)|As], A1, A2) :-
 	same_target(As, T, Same, Rest),
	(   Same = []
	->  A2 = [align(S,T,P)|A2Rest],
	    A1 = A1Rest
	;   append([align(S,T,P)|Same], A1Rest, A1),
	    A2 = A2Rest
	),
	select_(Rest, A1Rest, A2Rest).

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,
	same_target(As, T, Same, Rest).
same_target(As, _T, [], As).
