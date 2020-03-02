:- module(isub_generator,
	  []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(amalgame/vocabulary)).

:- use_module(isub_match).
:- use_module(string_match_util).

:- public matcher/4.
:- public parameter/4.
:- public amalgame_module/1.

amalgame_module(amalgame:'IsubMatcher').

parameter(sourcelabel, oneof(LabelProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, amalgame:label),
	label_list(LabelProps).
parameter(targetlabel, oneof(LabelProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, amalgame:label),
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

%%      matcher(+Source, +Target, -Mappings, +Options)
%
%       Mappings is a list of matches between instances of Source and
%       Target.

matcher(Source, Target, Mappings, Options) :-
        findall(M, align(Source, Target, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
        vocab_member(S, Source),
        vocab_member(T, Target),
        isub_match(align(S,T,[]), Match, Options).
