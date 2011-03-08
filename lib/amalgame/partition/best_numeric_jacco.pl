:- module(best_numeric, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/alignment_graph)).

:- public partition/3.
:- multifile amalgame:component/2.

amalgame:component(partition, best_numeric(alignment_graph, [selected(alignment_graph),
							   discarded(alignment_graph),
							   undecided(alignment_graph)
							  ], [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition(AlignmentGraph, ListOfGraphs, _Options) :-
	ListOfGraphs = [selected(S),
			discarded(D),
			undecided(U)
		      ],

	% FIXME: Logic is not right
	predsort(compare_align(sourceplus), AlignmentGraph, FirstRunInput),
	partition_source(FirstRunInput, Ss, Ds, Us),

	predsort(alignment_graph:compare_align(targetplus), Ss, SecondRunInput),
	partition_target(SecondRunInput, St, Dt, Ut),

	sort(St, S),
	append(Ds, Dt, D0), sort(D0, D),
	append(Us, Ut, U0), sort(U0, U).

partition_source([], [], [], []).
partition_source([align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   best_numeric([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_source(Rest, SelRest, DisRest, UndRest).

partition_target([], [], [], []).
partition_target([align(S,T,P)|As], Sel, Dis, Und) :-
	same_target(As, T, Same, Rest),
	(   best_numeric([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_target(Rest, SelRest, DisRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,
	same_target(As, T, Same, Rest).
same_target(As, _T, [], As).

best_numeric([Singleton|[]], Singleton, []) :- !.
best_numeric(As, Selected, [A|T]) :-
	group_match(As, Counts0),
	!,
	sort(Counts0, Counts),
	reverse(Counts, [N-Selected,N1-A|T0]),
	N > N1,
	pairs_values(T0, T).

group_match([], []).
group_match([Align|As], [Match-Align|Ts]) :-
	Align = align(_,_,Provenance),
	member(P, Provenance),
	memberchk(method(jaccard), P),
	memberchk(match(Match), P),
  	group_match(As, Ts).


