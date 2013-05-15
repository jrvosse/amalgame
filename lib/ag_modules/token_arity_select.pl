:- module(token_arity_select,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'TokenAritySelect').

parameter(type, oneof(['11','1N','N1']), '11',
	  'Type of arity to select:
	  1-1 = unique source and target,
	  1-N = unique source for a target').

%%	selecter(+Mapping, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the unique correspondences between
%	a source and target concept.

selecter(Mapping, Sel, Dis, [], Options) :-
	(   option(type('11'), Options)
	->  select_1_1(Mapping, Sel, Dis)
	;   option(type('N1'), Options)
	->  select_n_1(Mapping, Sel, Dis)
	;   option(type('1N'), Options)
	->  select_1_n(Mapping, Sel, Dis)
	).

%%	select_1_1(+Mapping, -Mapping_1_1, -Rest)
%
%	Mapping_1_1 contains all unique correspondences.

select_1_1([],[],[]).
select_1_1([Head|Tail], Sel, Dis) :-
        select_1_1(Tail, SelT, DisT),
        Head = align(_,_,Prov),
        member(token_ambiguity(N), Prov),
        (   N > 1
        ->  Sel = SelT, Dis = [Head|DisT]
        ;   Dis = DisT, Sel = [Head|SelT]
        ).

%%	selecter(+Mapping, -Mapping_n_1, -Rest)
%
%	Selected contains all correspondance where a source is mapped to
%	only one target.

select_n_1([], [], []).
select_n_1([align(S,T,P)|As], A1, A2) :-
	same_source(As, S, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_n_1(Rest, A1Rest, A2Rest).

%%	select_1_n(+Mapping, -Mapping_1_n, -Rest)
%
%	Mapping_1_n contains all correspondences where a target is
%	mapped to only one source.

select_1_n(Mapping, Sel, Dis) :-
	predsort(ag_map:compare_align(target), Mapping, TargetSorted),
	select_1_n_(TargetSorted, Sel0, Dis0),
	sort(Sel0, Sel),
	sort(Dis0, Dis).

select_1_n_([], [], []).
select_1_n_([align(S,T,P)|As], A1, A2) :-
	same_target(As, T, Same, Rest),
	(   Same = []
	->  A1 = [align(S,T,P)|A1Rest],
	    A2 = A2Rest
	;   append([align(S,T,P)|Same], A2Rest, A2),
	    A1 = A1Rest
	),
	select_1_n_(Rest, A1Rest, A2Rest).
