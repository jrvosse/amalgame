:- module(ag_util,
	  [ find_unique/4,              % +Var, +Goal, +Max, -Results
	    list_offset/3,
	    list_limit/4,
	    sort_by_arg/3,
	    sort_by_arg/4,
	    group_by_arg/3,
	    remove_resource/2 % +Resource, +Graph
	  ]).

:- use_module(library(semweb/rdf_db)).

:- meta_predicate
        find_unique(-, 0, +, -).

%%      find_unique(Var, :Goal, +MaxResults, -SortedSet)
%
%       Find at most MaxResults distinct solutions for Var in Goal.

find_unique(T, G, inf, Ts) :- !,
        findall(T, G, Raw),
        sort(Raw, Ts).
find_unique(T, G, Max, Ts) :-
        empty_nb_set(Set),
        State = count(0),
        (       G,
                add_nb_set(T, Set, true),
                arg(1, State, C0),
                C is C0 + 1,
                nb_setarg(1, State, C),
                C == Max
        ->      true
        ;       true
        ),
        nb_set_to_list(Set, Ts).


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

%%	remove_resource(+Resource, +Graph) is det.
%
%	Remove all references to Resource from Graph,
%	including (recursively) all blank nodes that
%	Resource uniquely referred to.

remove_resource(R, G) :-
	ground(R),
	ground(G),
	findall(Blank,
		(   rdf(R,_,Blank, G),
		    rdf_is_bnode(Blank),
		    \+ (rdf(R2, _, Blank, G), R2 \= R)
		),
		BlankNodes),
	forall(member(B, BlankNodes),
	       remove_resource(B, G)
	      ),
	rdf_retractall(R,_,_,G),
	rdf_retractall(_,R,_,G),
	rdf_retractall(_,_,R,G).

