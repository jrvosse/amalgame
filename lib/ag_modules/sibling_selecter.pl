:- module(sibling_selecter,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof(target, tbd), target,
	  'Select siblings from alternative targets').

parameter(depth,
	  integer, 2,
	  'Sibling depth').

amalgame_module(amalgame:'SiblingSelecter').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%

selecter(AlignmentGraph, Sel, [], Und, Options) :-
	option(depth(Depth), Options, 2),
	option(type(SourceOrTarget), Options, source),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, AlignmentGraph, Depth, Sel, Und)
	;   predsort(ag_map:compare_align(targetplus), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, SortedAlignmentGraph, Depth, Sel0, Und0),
	    predsort(ag_map:compare_align(sourceplus), Sel0,  Sel),
	    predsort(ag_map:compare_align(sourceplus), Und0,  Und)
	).



partition_(_, [], _, [], []).
partition_(target, [A|As], Depth, Sel, Und) :-
	A = align(S,T,_),
	same_source(As, S, Same, Rest),
	(   rdf_reachable(T, skos:broader, Parent, Depth, _),
	    siblings(Same, Parent, Depth)
	->  append([A|Same], SelRest, Sel),
	    Und = UndRest
	;   append([A|Same], UndRest, Und),
	    Sel = SelRest
	),
	partition_(target, Rest, Depth, SelRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

siblings([], _, _).
siblings([A|As], Parent, Depth) :-
	A = align(_,T,_),
	rdf_reachable(T, skos:broader, Parent, Depth, _),
	!,
	siblings(As, Parent, Depth).

