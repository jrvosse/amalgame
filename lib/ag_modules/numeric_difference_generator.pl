:- module(numeric_difference_generator,
	  []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/rdf_util)).

:- use_module(numeric_difference_match).


:- public matcher/4.
:- public parameter/4.
:- public amalgame_module/1.

amalgame_module(amalgame:'NumericDifferenceMatcher').

parameter(sourcelabel, oneof(LiteralProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, amalgame:label),
	rdf_literal_predicates(LiteralProps).
parameter(targetlabel, oneof(LiteralProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, amalgame:label),
	rdf_literal_predicates(LiteralProps).

parameter(threshold, float, 0.05,
	  'threshold absolute difference').

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
        numeric_difference_match(align(S,T,[]), Match, Options).
