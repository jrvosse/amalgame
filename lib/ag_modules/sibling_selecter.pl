:- module(sibling_selecter,[]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/correspondence)).
:- use_module(library(semweb/rdf_label)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof([source, target]), target,
	  'Select siblings from alternative targets (or sources)').

parameter(depth,
	  integer, 2,
	  'Sibling depth').

amalgame_module(amalgame:'SiblingSelecter').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%       Source should be sorted (e.g. on source).

selecter(SSorted, Sel, [], Und, Options) :-
	option(depth(MaxDepth), Options, 2),
	option(type(SourceOrTarget), Options, source),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, SSorted, MaxDepth, Sel, Und)
	;   sort_correspondences(target, SSorted, TSorted),
	    partition_(SourceOrTarget, TSorted, MaxDepth, Sel0, Und0),
	    sort_correspondences(source, Sel0,  Sel),
	    sort_correspondences(source, Und0,  Und)
	).
ac(Result, Type, Length, align(S,T,P), align(S,T,Pnew)) :-
	append(P, [[method(sibling_select),
		    score([
			result(Result),
			type(Type),
			nr_compared(Length)])]
		  ], Pnew).
ac(Result, Type, Length, Depth, Parent, align(S,T,P), align(S,T,Pnew)) :-
	rdf_display_label(Parent, Label),
	append(P, [[method(sibling_select),
		    score([
			result(Result),
			type(Type),
			depth(Depth),
			nr_compared(Length),
			parent_label(Label),
			parent(Parent)])]
		  ], Pnew).


partition_(_, [], _, [], []).
partition_(target, [A|As], MaxDepth, Sel, Und) :-
	A = align(S,T,_),
	same_source(As, S, Same, Rest),
	(   Same \= [],
	    skos_descendant_of(Parent,T, MaxDepth,  Depth),
	    siblings(target, Same, Parent, MaxDepth)
	->  Selected = [A|Same],
	    length(Selected, Length),
	    maplist(ac(selected, target, Length, Depth, Parent), Selected, SelectedE),
	    append(SelectedE, SelRest, Sel),
	    Und = UndRest
	;   Undecided = [A|Same],
	    length(Undecided, Length),
	    maplist(ac(undecided, target, Length), Undecided, UndecidedE),
	    append(UndecidedE, UndRest, Und),
	    Sel = SelRest
	),
	partition_(target, Rest, MaxDepth, SelRest, UndRest).

partition_(source, [A|As], Depth, Sel, Und) :-
	A = align(S,T,_),
	same_target(As, T, Same, Rest),
	(   Same \= [],
	    skos_descendant_of(Parent, S, Depth, _),
	    siblings(source, Same, Parent, Depth)
	->  Selected = [A|Same],
	    length(Selected, Length),
	    maplist(ac(selected, source, Length, Depth, Parent), Selected, SelectedE),
	    append(SelectedE, SelRest, Sel),
	    Und = UndRest
	;   Undecided = [A|Same],
	    length(Undecided, Length),
	    maplist(ac(undecided, source, Length), Undecided, UndecidedE),
	    append(UndecidedE, UndRest, Und),
	    Sel = SelRest
	),
	partition_(source, Rest, Depth, SelRest, UndRest).

siblings(_, [], _, _).
siblings(target, [A|As], Parent, Depth) :-
	A = align(_,T,_),
	skos_descendant_of(Parent, T, Depth, _),
	!,
	siblings(target, As, Parent, Depth).

siblings(source, [A|As], Parent, Depth) :-
	A = align(S,_,_),
	skos_descendant_of(Parent, S, Depth, _),
	!,
	siblings(source, As, Parent, Depth).

