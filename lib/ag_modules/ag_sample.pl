:- module(ag_sample, [
		     ]).

:- use_module(library(random)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'Sampler').

parameter(sample_size, integer, 25, 'Sample size').
parameter(method, oneof([random]), random , 'Sample method').

selecter(In, Out, [], [], Options) :-
	option(sample_size(Size), Options, 5),
	option(method(Method), Options, random),
	length(In, Length),
	N is min(Length, Size),
	randset(N, Length, RandSet),
	assert_from_list(Method, In, 1, RandSet, Out).

assert_from_list(_,[],_,_,[]):- !.
assert_from_list(_,_,_,[],[]):- !.
assert_from_list(Method, [Head|Tail], Nr, [Rand|RandSet], RandomMaps) :-
	(   Rand = Nr
	->  NewRandSet = RandSet,
	    RandomMaps = [Head|Maps]
	;   NewRandSet = [Rand|RandSet],
	    RandomMaps = Maps
	),
	NewNr is Nr + 1,
	assert_from_list(Method, Tail, NewNr, NewRandSet, Maps).

