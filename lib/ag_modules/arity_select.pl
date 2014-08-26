:- module(arity_select,[]).

:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'AritySelect').

parameter(type, oneof(['source','target','both']), 'both',
	  'Type of ambiguity to resolve: both = select only correspondences with a unique source and target, target = require unique source for every target, source = require unique target for every source').

%%	selecter(+Mapping, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the unique correspondences between
%	a source and target concept.

selecter(Mapping, Sel, Dis, [], Options) :-
	(   option(type('both'), Options)
	->  select_1_1(Mapping, Sel, Dis)
	;   option(type('source'), Options)
	->  select_n_1(Mapping, Sel, Dis)
	;   option(type('target'), Options)
	->  select_1_n(Mapping, Sel, Dis)
	).

ap(Result, Arity, align(S,T,P), align(S,T,NP)) :-
	NP = [NewP|P],
	NewP = [method(ambiguity_remover),
		score([result(Result), condition(Arity)])
	       ].

%%	select_1_1(+Mapping, -Mapping_1_1, -Rest)
%
%	Mapping_1_1 contains all unique correspondences.

select_1_1(Mapping, Sel, Dis) :-
	select_n_1_raw(Mapping, Source1, SourceN),
	select_1_n_raw(Mapping, Target1, TargetN),
	ord_intersection(Source1, Target1, Sel0),
	ord_union(SourceN, TargetN, Dis0),
	maplist(ap(selected, 1-1), Sel0, Sel),
	maplist(ap(discarded,1-1), Dis0, Dis).

%%	select_n_1(+Mapping, -Mapping_n_1, -Rest)
%
%	Selected contains all correspondance where a source is mapped to
%	only one target.
select_n_1(Mapping, Sel, Dis) :-
	select_n_1_raw(Mapping, Sel0, Dis0),
	maplist(ap(selected, n-1), Sel0, Sel),
	maplist(ap(discarded,n-1), Dis0, Dis).

%%	select_1_n(+Mapping, -Mapping_1_n, -Rest)
%
%	Mapping_1_n contains all correspondences where a target is
%	mapped to only one source.
select_1_n(Mapping, Sel, Dis) :-
	predsort(ag_map:compare_align(target), Mapping, TargetSorted),
	select_1_n_raw(TargetSorted, Sel0, Dis0),
	predsort(ag_map:compare_align(source),Sel0, Sel1),
	predsort(ag_map:compare_align(source),Dis0, Dis1),
	maplist(ap(selected, 1-n), Sel1, Sel),
	maplist(ap(discarded,1-n), Dis1, Dis).

select_n_1_raw([], [], []).
select_n_1_raw([align(S,T,P)|As], A1, A2) :-
	same_source(As, S, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_n_1_raw(Rest, A1Rest, A2Rest).

select_1_n_raw([], [], []).
select_1_n_raw([align(S,T,P)|As], A1, A2) :-
	same_target(As, T, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_1_n_raw(Rest, A1Rest, A2Rest).



