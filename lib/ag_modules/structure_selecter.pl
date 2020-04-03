:- module(structure_selecter,
	  [ selecter/6
	  ]).

/* This module provides a meta predicate selecter/6, which implements the selecter/5 predicate of the
 *  skos ancestor, descendent and related partitioners.
 */

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).

:- use_module(library(amalgame/correspondence)).

:- meta_predicate selecter(4, +, -, -, -, +).

selecter(Matcher, In, Sel, Dis, Und, Options) :-
	option(snd_input(SecList), Options),
	option(type(SourceOrTarget), Options, all),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	(   SourceOrTarget \= source
	->  selecter_(SourceOrTarget, Matcher, In, BackgroundMatches, Sel, Dis, Und, Options)
	;   sort_align(target, In, InT),
	    selecter_(SourceOrTarget, Matcher, InT, BackgroundMatches, Sel0, Dis0, Und0, Options),
	    sort_align(source, Sel0,  Sel),
	    sort_align(source, Dis0,  Dis),
	    sort_align(source, Und0,  Und)
	).

selecter_(_, _, [], _, [], [], [], _).
selecter_(all, Matcher, [Head|Tail], BackgroundMatches, Sel, Dis, [], Options) :-
	(   call(Matcher, Head, BackgroundMatches, Match, Options)
	->  Sel = [Match|TSel],
	    Dis = TDis
	;   Sel = TSel,
	    Dis = [Head|TDis]
	),
	selecter_(all, Matcher, Tail, BackgroundMatches, TSel, TDis, [], Options).
selecter_(Type, Matcher, [Head|Tail], BackgroundMatches, Sel, Dis, Und, Options) :-
	Head = align(S,T,_),
	(   Type == target
	->  same_source(Tail, S, Same, Rest)
	;   same_target(Tail, T, Same, Rest)
	),
	% Fix me: make this tail recursive
	selecter_(Type, Matcher, Rest, BackgroundMatches, TailSel, TailDis, TailUnd, Options),

	Candidates = [Head|Same],
	(   pick_best(Candidates, Matcher, BackgroundMatches, SelectedSame, DisgardedSame, Options)
	->  append([SelectedSame,  TailSel], Sel),
	    append([DisgardedSame, TailDis], Dis),
	    Und = TailUnd
	;   Sel = TailSel,
	    Dis = TailDis,
	    append([Candidates, TailUnd], Und)
	).

pick_best(Candidates, Matcher, BackgroundMatches, [Selected], Disgarded, Options) :-
	length(Candidates, N), N > 1, % do not pick a best if we have only one alternative
	maplist(structure_count(Matcher, BackgroundMatches, Options), Candidates, Counts0),
	keysort(Counts0, Sorted),
	append([DiscardedPairs, [SecondBest-Value2], [Best-Selected]], Sorted),
	Best > SecondBest,
	Best > 0,
	pairs_values([SecondBest-Value2|DiscardedPairs], Disgarded).

structure_count(Matcher, BackgroundMatches, Options, Corr, Count-Merged) :-
	findall(Match,
		call(Matcher, Corr, BackgroundMatches, Match, Options),
		Matches),
	length(Matches, Count),
	(   Count > 0
	->  merge_provenance(Matches, [Merged])
	;   Merged = Corr
	).
