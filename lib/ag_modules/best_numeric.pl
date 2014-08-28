:- module(best_numeric, []).

:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof([source,target]), target,
	  'source = best source for each target, target = best target for each source').

amalgame_module(amalgame:'BestNumeric').


%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%

selecter(Mapping, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, target),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, Mapping, Sel, Disc, Und)
	;   predsort(ag_map:compare_align(target), Mapping, TSorted),
	    partition_(SourceOrTarget, TSorted, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Disc0, Disc),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).


partition_(_, [], [], [], []) :- !.
partition_(SourceOrTarget, [align(S,T,P)|As], Sel, Dis, Und) :-
	(   SourceOrTarget = target
	->  same_source(As, S, Same, Rest)
	;   same_target(As, T, Same, Rest)
	),
	(   best_numeric([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(SourceOrTarget, Rest, SelRest, DisRest, UndRest).

best_numeric(As, Selected, Discarded) :-
	group_match(As, Counts0),
	!,
	(   Counts0 = [_N-Selected]
	->  Discarded = []
	;   sort(Counts0, Counts),
	    reverse(Counts, [N-Selected,N1-A|T0]),
	    N > N1,
	    pairs_values(T0, T),
	    Discarded = [A|T]
	).

group_match([], []).
group_match([Align|As], [Match-Align|Ts]) :-
	Align = align(_,_,Provenance),
	member(P, Provenance),
	% memberchk(method(M), P),
	% memberchk(M, [jaccard, isub]),
	memberchk(match(Match), P),
	group_match(As, Ts).
