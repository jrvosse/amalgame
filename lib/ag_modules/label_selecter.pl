:- module(label_selecter,
	  [ label_selecter/6
	  ]).

/* This module provides a meta predicate label_selecter/7, which implements the selecter/5 predicate of the
 * label matching partitioners.
 */

:- meta_predicate label_selecter(3, +, -, -, -, +).

label_selecter(Matcher, In, Sel, Dis, Und, Options) :-
	option(type(SourceOrTarget), Options, all),
	(   SourceOrTarget \= source
	->  label_selecter(SourceOrTarget, Matcher, In, Sel, Dis, Und, Options)
	;   predsort(ag_map:compare_align(target), In, InT),
	    label_selecter(SourceOrTarget, Matcher, InT, Sel0, Dis0, Und0, Options),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Dis0,  Dis),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).

label_selecter(  _, _, [],  [],  [],  [], _).
label_selecter(all, Matcher, [Head|Tail], Sel, Dis, [], Options) :-
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
