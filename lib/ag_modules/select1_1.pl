:- module(select1_1, []).

:- use_module(library(semweb/rdf_db)).

:- public amalgame_module/1.
:- public selecter/5.

amalgame_module(amalgame:'Select_1_1').


%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the correspondance where a source has one
%	target.

selecter(AlignmentGraph, Sel, Dis, [], _Options) :-
 	select_(AlignmentGraph, Sel, Dis).

select_([], [], []).
select_([align(S,T,P)|As], A1, A2) :-
	same_source(As, S, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_(Rest, A1Rest, A2Rest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).


