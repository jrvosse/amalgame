:- module(ag_sample, [
		     ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'Sampler').

parameter(sample_size, integer, 5, 'Sample size').
parameter(method, oneof([random]), random , 'Sample method').

selecter(In, Out, _, _, Options) :-
	option(sample_size(Size), Options, 5),
	option(method(Method), Options, random),
	length(In, Length),
	randset(Size, Length, RandSet),
	assert_from_list(Method, In, 1, RandSet, Out).

assert_from_list(_,_,_,[], _).
assert_from_list(Method, In, Nr, [Rand|RandSet], [[E1,E2]|Maps]) :-
	(   Rand = Nr
	->  has_map([E1,E2], _, Options, Graph),!,
	    (	Method = random
	    ->	AltMaps = [E1-E2-Options]
	    ),
	    assert_map_list(AltMaps, Name),
	    NewRandSet = RandSet
	;   NewRandSet = [Rand|RandSet]
	),
	NewNr is Nr + 1,
	assert_from_list(Method, In, NewNr, NewRandSet, Maps).

assert_map_list([],_).
assert_map_list([H|T], Graph) :-
	H=E1-E2-Options,
	(   has_map([E1,E2], edoal, Graph)
	->  true
	;   assert_cell(E1,E2, [graph(Graph), alignment(Graph) | Options])
	),
	assert_map_list(T,Graph).
