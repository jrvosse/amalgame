:- module(sibling_selecter,[]).

:- use_module(library(skos/util)).
:- use_module(library(amalgame/map)).

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
%

selecter(SSorted, SelP, [], Und, Options) :-
	option(depth(Depth), Options, 2),
	option(type(SourceOrTarget), Options, source),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, SSorted, Depth, Sel, Und)
	;   predsort(ag_map:compare_align(target), SSorted, TSorted),
	    partition_(SourceOrTarget, TSorted, Depth, Sel0, Und0),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	),
	maplist(ap(SourceOrTarget, Depth), Sel, SelP).

ap(Type, Depth, align(S,T,P), align(S,T,Pnew)) :-
	append(P, [[method(sibling_select),
		    score([type(Type), maxdepth(Depth)])]], Pnew).



partition_(_, [], _, [], []).
partition_(target, [A|As], Depth, Sel, Und) :-
	A = align(S,T,_),
	same_source(As, S, Same, Rest),
	(   Same \= [],
	    skos_descendant_of(Parent,T, Depth, _),
	    siblings(target, Same, Parent, Depth)
	->  append([A|Same], SelRest, Sel),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest
	),
	partition_(target, Rest, Depth, SelRest, UndRest).

partition_(source, [A|As], Depth, Sel, Und) :-
	A = align(S,T,_),
	same_target(As, T, Same, Rest),
	(   Same \= [],
	    skos_descendant_of(Parent, S, Depth, _),
	    siblings(source, Same, Parent, Depth)
	->  append([A|Same], SelRest, Sel),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
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

