:- module(exact_label_selecter,
	  []).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

:- use_module(library(amalgame/vocabulary)).
:- use_module(library(semweb/rdf11)).
:- use_module(label_selecter).
:- use_module(exact_label_match).
:- use_module(string_match_util).

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
parameter(source_language, oneof(['any'|L]), 'any',
	  'Language of source label') :-
	amalgame_vocabulary_languages(L).
parameter(matchacross_lang, boolean, true,
	  'Allow labels from different language to be matched').
parameter(matchacross_type, boolean, true,
	  'Allow labels from different types to be matched').
parameter(case_sensitive, boolean, false,
	  'When true the case of labels must be equal').
parameter(match_qualified_only, boolean, false,
	  'Match only on the fully qualified label').

amalgame_module(amalgame:'ExactLabelSelecter').

selecter(In, Sel, Dis, Und, Options) :-
	label_selecter(exact_label_match, In, Sel, Dis, Und, Options).
