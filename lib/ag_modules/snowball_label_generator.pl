:- module(snowball_label_generator,
	  []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(snowball)).
:- use_module(library(amalgame/vocabulary)).

:- use_module(string_match_util).
:- use_module(snowball_label_match).

:- public amalgame_module/1.
:- public parameter/4.
:- public matcher/4.

amalgame_module(amalgame:'SnowballMatcher').

parameter(sourcelabel, oneof(LabelProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, amalgame:label),
	label_list(LabelProps).
parameter(targetlabel, oneof(LabelProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, amalgame:label),
	label_list(LabelProps).
parameter(source_language, oneof(['any'|L]), 'any', 'Language of source label') :-
	amalgame_vocabulary_languages(L).
parameter(matchacross_lang, boolean, true,
	  'Allow labels from different language to be matched').
parameter(snowball_language, oneof(Languages), english,
	  'Language to use for stemmer') :-
	findall(Alg, snowball_current_algorithm(Alg), Languages).
parameter(prefix, integer, 4,
	  'Optimise performence by first generating candidates by matching the prefix.Input is an integer for the prefix length.').
parameter(edit_distance, integer, 0,
	  'When >0 allow additional differences between labels').

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	findall(A, align(Source, Target, A, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
	vocab_member(S, Source),
	snowball_label_match(align(S,_,[]), Match,
			     [target_scheme(Target)|Options]).
