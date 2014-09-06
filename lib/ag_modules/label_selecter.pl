:- module(label_selecter,
	  [ label_selecter/7
	  ]).

/* This module provides a meta predicate label_selecter/7, which implements the selecter/5 predicate of the
 * label matching partitioners.
 */

:- meta_predicate label_selecter(+, 3, +, -, -, -, +).

label_selecter(  _, _, [],  [],  [],  [], _).
label_selecter(all, Matcher, [Head|Tail], Sel, Dis, [], Options) :-
	(   call(Matcher, Head, Match, Options)
	->  Sel = [Match|TSel],
	    Dis = TDis
	;   Sel = TSel,
	    Dis = [Head|TDis]
	),
	label_selecter(all, Matcher, Tail, TSel, TDis, [], Options).
