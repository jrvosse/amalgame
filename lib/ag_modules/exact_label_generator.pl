:- module(exact_label_generator, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(exact_label_match).
:- use_module(string_match_util).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'ExactLabelMatcher').
amalgame_module(amalgame:'ExactLabelFilter').

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

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter([], [], _).
filter([align(S,T,P)|Cs], [C|Mappings], Options) :-
	(   T = scheme(TargetScheme)
	->  exact_label_match(align(S,_,P), C, [target_scheme(TargetScheme)|Options])
	;   exact_label_match(align(S,T,P), C, Options)
	),
	!,
	filter(Cs, Mappings, Options).
filter([_|Cs], Mappings, Options) :-
	filter(Cs, Mappings, Options).


%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a sorted list of matches between instances of Source
%	and Target.

matcher(Source, Target, Mappings, Options) :-
	findall(M, align(Source, Target, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
	vocab_member(S, Source),
	exact_label_match(align(S,_,[]), Match, [target_scheme(Target)|Options]).
