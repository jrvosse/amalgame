:- module(snowball_label_selecter,
	  []).

:- use_module(library(amalgame/vocabulary)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(snowball)).
:- use_module(string_match_util).
:- use_module(snowball_label_match).
:- use_module(label_selecter).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/5.

amalgame_module(amalgame:'SnowballLabelSelecter').

parameter(type,
	  oneof([source,target, all]), all,
	 'Select all exact label matches or pick best source/target to disambiguate').

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
parameter(match_qualified_only, boolean, false,
	  'Match only on the fully qualified label').


selecter(In, Sel, Dis, Und, Options) :-
	label_selecter(snowball_label_match, In, Sel, Dis, Und, Options).




