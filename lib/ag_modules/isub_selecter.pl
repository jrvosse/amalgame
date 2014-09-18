:- module(isub_selecter,
	  []).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

:- use_module(library(amalgame/vocabulary)).
:- use_module(library(semweb/rdf_db)).
:- use_module(label_selecter).
:- use_module(string_match_util).
:- use_module(isub_match).

amalgame_module(amalgame:'IsubSelecter').

parameter(type,
	  oneof([source,target, all]), all,
	 'Select all exact label matches or pick best source/target to disambiguate').

parameter(sourcelabel, oneof(LabelProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(targetlabel, oneof(LabelProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(threshold, float, 0.7,
	  'threshold edit distance').
parameter(source_language, oneof(['any'|L]), 'any', 'Language of source label') :-
	amalgame_vocabulary_languages(L).
parameter(matchacross_lang,
	  boolean, true,
	  'Allow labels from different language to be matched').
parameter(normalize,
	  boolean, false,
	  '(Case) normalize strings as described in the isub article').

selecter(In, Sel, Dis, Und, Options) :-
	label_selecter(isub_match, In, Sel, Dis, Und, Options).
