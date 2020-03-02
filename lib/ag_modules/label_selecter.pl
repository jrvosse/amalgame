:- module(label_selecter,
	  [ label_selecter/6
	  ]).

/* This module provides a meta predicate label_selecter/7, which implements the selecter/5 predicate of the
 * label matching partitioners.
 */

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(amalgame/map)).

:- meta_predicate label_selecter(3, +, -, -, -, +).

label_selecter(Matcher, In, Sel, Dis, Und, Options) :-
	option(type(SourceOrTarget), Options, all),
	(   SourceOrTarget \= source
	->  label_selecter(SourceOrTarget, Matcher, In, Sel, Dis, Und, Options)
	;   sort_align(target, In, InT),
	    label_selecter(SourceOrTarget, Matcher, InT, Sel0, Dis0, Und0, Options),
	    sort_align(source, Sel0,  Sel),
	    sort_align(source, Dis0,  Dis),
	    sort_align(source, Und0,  Und)
	).

label_selecter(  _, _, [],  [],  [],  [], _).
label_selecter(all, Matcher, [Head|Tail], Sel, Dis, [], Options) :-
	!,
	(   call(Matcher, Head, Match, Options)
	->  Sel = [Match|TSel],
	    Dis = TDis
	;   Sel = TSel,
	    Head = align(S,T,P),
	    MisEv = [method(Matcher), score([result(discarded)])],
	    MisMatch = align(S,T, [MisEv|P]),
	    Dis = [MisMatch|TDis]
	),
	label_selecter(all, Matcher, Tail, TSel, TDis, [], Options).

label_selecter(Type, Matcher, [Head|Tail], Sel, Dis, Und, Options) :-
	Head = align(S,T,_),
	(   Type == target
	->  same_source(Tail, S, Same, Rest)
	;   same_target(Tail, T, Same, Rest)
	),
	% Fix me: make this tail recursive
	label_selecter(Type, Matcher, Rest, TailSel, TailDis, TailUnd, Options),

	Candidates = [Head|Same],
	(   pick_best(Candidates, Matcher, SelectedSame, DisgardedSame, Options)
	->  append([SelectedSame,  TailSel], Sel),
	    append([DisgardedSame, TailDis], Dis),
	    Und = TailUnd
	;   Sel = TailSel,
	    Dis = TailDis,
	    append([Candidates, TailUnd], Und)
	).

pick_best(Candidates, Matcher, [Selected], Disgarded, Options) :-
	length(Candidates, N), N > 1, % do not pick a best if we have only one alternative
	maplist(label_count(Matcher, Options), Candidates, Counts0),
	keysort(Counts0, Sorted),
	append([DiscardedPairs, [SecondBest-Value2], [Best-Selected]], Sorted),
	Best > SecondBest,
	Best > 0,
	pairs_values([SecondBest-Value2|DiscardedPairs], Disgarded).

label_count(Matcher, Options, Corr, Count-Merged) :-
	findall(Match,
		call(Matcher, Corr, Match, Options),
		Matches),
	length(Matches, Count),
	(   Count > 0
	->  merge_provenance(Matches, [Merged])
	;   Merged = Corr
	).
