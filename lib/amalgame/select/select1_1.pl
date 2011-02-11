:- module(select1_1,[]).

:- use_module(library(semweb/rdf_db)).

:- public selecter/5.

%%	selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%

selecter(AlignmentGraph, Sel, Dis, [], _Options) :-
 	select_(AlignmentGraph, Sel, Dis).

select_([], [], []).
select_([align(S,T,P)|As], A1, A2) :-
	same_source(As, S, Same, Rest),
	(   Same = []
	->  A2 = [align(S,T,P)|A2Rest],
	    A1 = A1Rest
	;   append([align(S,T,P)|Same], A1Rest, A1),
	    A2 = A2Rest
	),
	select_(Rest, A1Rest, A2Rest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).
