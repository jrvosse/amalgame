:- module(descendent_match,
	  []).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(descendent).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'DescendentMatcher').
amalgame_module(amalgame:'DescendentFilter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct children only').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on descendent matches in snd_input.

filter(In, Out, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	filter_(In, BackgroundMatches, Out, Options).

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	findall(M, align(Source, Target, BackgroundMatches, M, Options), Mappings0),
	sort(Mappings0, Mappings).

filter_([], _, [], _).
filter_([align(S,T,P)|Cs], BackgroundMatches, [C|Mappings], Options) :-
	(   T = scheme(_)
	->  descendent_match(align(S,_,P), BackgroundMatches, C, Options),
	    C=align(_,T2,_),
	    vocab_member(T2, T)
	;   descendent_match(align(S,T,P), BackgroundMatches, C, Options)
	),
	!,
	filter_(Cs, BackgroundMatches, Mappings, Options).
filter_([_|Cs], BackgroundMatches, Mappings, Options) :-
	filter_(Cs, BackgroundMatches, Mappings, Options).


align(Source, Target, BackgroundMatches, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	descendent_match(align(S,T,[]), BackgroundMatches, Match, Options).
