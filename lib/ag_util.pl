:- module(ag_util,
	  [ list_offset/3,
	    list_limit/4,
	    sort_by_arg/3,
	    sort_by_arg/4,
	    group_by_arg/3
	  ]).


%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	N < 0,
	!.
list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).


%%	sort_by_arg(+ListOfTerms, +Arg, -SortedList)
%
%	SortedList contains the Terms from ListOfTerms sorted by their
%	nth Arg.

sort_by_arg(List, Arg, Sorted) :-
	maplist(arg_key(Arg), List, Pairs),
	keysort(Pairs, SortedPairs),
	pairs_values(SortedPairs, Sorted).

%%	sort_by_arg(+ListOfTerms, +Arg, +Direction, -SortedList)
%
%	SortedList contains the Terms from ListOfTerms sorted by their
%	nth Arg.

sort_by_arg(List, Arg, Direction, Sorted) :-
	sort_by_arg(List, Arg, Sorted0),
	(   Direction == desc
	->  reverse(Sorted0, Sorted)
	;   Sorted = Sorted0
	).

%%	group_by_arg(+ListOfTerms, +Arg, -GroupedList)
%
%	GroupedList contains the Terms from ListOfTerms grouped by their
%	nth Arg.

group_by_arg(List, Arg, Sorted) :-
	maplist(arg_key(Arg), List, Pairs),
	keysort(Pairs, SortedPairs),
	group_pairs_by_key(SortedPairs, Sorted).

arg_key(Args, Term, Keys-Term) :-
	is_list(Args),
	!,
	args(Args, Term, Keys).
arg_key(Arg, Term, Key-Term) :-
	arg(Arg, Term, Key).

args([A], Term, [Key]) :- !,
	arg(A, Term, Key).
args([A|As], Term, [Key|Ks]) :-
	arg(A, Term, Key),
	args(As, Term, Ks).
