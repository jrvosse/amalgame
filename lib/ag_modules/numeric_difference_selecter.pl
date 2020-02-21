:- module(numeric_similarity_selecter,
	  []).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

:- use_module(library(semweb/rdf_db)).

:- use_module(library(amalgame/rdf_util)).
:- use_module(numeric_difference_match).
:- use_module(label_selecter).

parameter(type,
	  oneof([source,target, all]), all,
	 'Select all matches or pick best source/target to disambiguate').

parameter(threshold, float, 0.05,
           'threshold absolute difference').

parameter(sourcelabel, oneof(LiteralProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, amalgame:label),
	rdf_literal_predicates(LiteralProps).

parameter(targetlabel, oneof(LiteralProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, amalgame:label),
	rdf_literal_predicates(LiteralProps).

amalgame_module(amalgame:'NumericDifferenceSelecter').

selecter(In, Sel, Dis, Und, Options) :-
	label_selecter(numeric_difference_match, In, Sel, Dis, Und, Options).
