:- module(snowball_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(snowball)).
:- use_module(library(amalgame/lit_distance)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

:- public amalgame_module/1.
:- public parameter/4.
:- public filter/3.
:- public matcher/4.

amalgame_module(amalgame:'SnowballMatcher').
amalgame_module(amalgame:'SnowballFilter').

parameter(sourcelabel, oneof(LabelProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(targetlabel, oneof(LabelProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(language, oneof(['any'|L]), 'any', 'Language of source label') :-
	strategy_languages(_,L).
parameter(matchacross_lang, boolean, true,
	  'Allow labels from different language to be matched').
parameter(snowball_language, oneof(Languages), english,
	  'Language to use for stemmer') :-
	findall(Alg, snowball_current_algorithm(Alg), Languages).
parameter(prefix, integer, 4,
	  'Optimise performence by first generating candidates by matching the prefix.Input is an integer for the prefix length.').
parameter(edit_distance, integer, 0,
	  'When >0 allow additional differences between labels').


%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on matching stemmed labels.

filter([], [], _).
filter([C0|Cs], [C|Mappings], Options) :-
	match(C0, C, Options),
	!,
	filter(Cs, Mappings, Options).
filter([_|Cs], Mappings, Options) :-
	filter(Cs, Mappings, Options).

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	findall(A, align(Source, Target, A, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
	vocab_member(S, Source),
	match(align(S,_,[]), Match, [target_scheme(Target)|Options]).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(rdfs:label,DefaultP),
	option(snowball_language(Snowball_Language), Options, english),
	option(prefix(PrefixLength), Options, 4),
	option(sourcelabel(MatchProp1), Options, DefaultP),
	option(targetlabel(MatchProp2), Options, DefaultP),
	option(matchacross_lang(MatchAcross), Options, true),
	option(language(Lang),Options, any),
	option(edit_distance(Edit_Distance), Options, 0),

	(   Lang == 'any'
	->  SourceLang = _
	;   SourceLang = Lang
	),
	% If we can't match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	skos_match(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp, Options),

	(   sub_atom(SourceLabel, 0, PrefixLength, _, Prefix)
	->  true
	;   Prefix=SourceLabel
	),
	downcase_atom(SourceLabel, SourceLabel0),
	snowball(Snowball_Language, SourceLabel0, SourceStem),

	skos_match(Target, MatchProp2, literal(prefix(Prefix), lang(TargetLang, TargetLabel)), TargetProp, Options),
	(   option(target_scheme(TargetScheme), Options)
	->  vocab_member(Target, TargetScheme)
	;   true
	),

	downcase_atom(TargetLabel, TargetLabel0),
	snowball(Snowball_Language, TargetLabel0, TargetStem),
	(   Edit_Distance == 0
	->  TargetStem == SourceStem, Distance = 0
	;   literal_distance(SourceStem, TargetStem, Distance),
	    Distance =< Edit_Distance
	),
	Match is 1 / (1 + Distance),
	Prov = [method(snowball),
		prefix(Prefix),
		source_stem(SourceStem),
		target_stem(TargetStem),
		match(Match),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ],
	debug(align_result, 'snowball match: ~p ~p', [Source,Target]).
