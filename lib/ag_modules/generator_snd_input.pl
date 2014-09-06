:- module(generator_snd_input,
	  [
	      generator_snd_input/5
	  ]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(amalgame/vocabulary)).

:- meta_predicate
	generator_snd_input(4, +, +, -, +).

generator_snd_input(Matcher, Source, Target, Mappings, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	findall(M, align(Matcher, Source, Target,
			 BackgroundMatches, M, Options),
		Mappings0),
	sort(Mappings0, Mappings).

align(Matcher, Source, Target, BackgroundMatches, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	call(Matcher, align(S,T,[]), BackgroundMatches, Match, Options).
