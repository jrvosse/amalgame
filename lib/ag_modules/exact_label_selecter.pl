:- module(exact_label_selecter,
	  []).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

:- use_module(library(sort)).
:- use_module(library(amalgame/map)).
:- use_module(label_selecter).
:- use_module(exact_label_match).
:- use_module(string_match_util).

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
parameter(source_language, oneof(['any'|L]), 'any',
	  'Language of source label') :-
	strategy_languages(_S,L).
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
	option(type(SourceOrTarget), Options, all),
	(   SourceOrTarget \= source
	->  label_selecter(SourceOrTarget, exact_label_match, In, Sel, Dis, Und, Options)
	;   predsort(ag_map:compare_align(target), In, InT),
	    label_selecter(SourceOrTarget, exact_label_match, InT, Sel0, Dis0, Und0, Options),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Dis0,  Dis),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).
