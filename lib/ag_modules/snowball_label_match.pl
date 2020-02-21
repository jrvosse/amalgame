:- module(snowball_label_match,
	  [ snowball_label_match/3 ]).

:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(snowball)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/lit_distance)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

snowball_label_match(align(Source, Target, Prov0),
		     align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(amalgame:label,DefaultP),
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

	skos_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp, Options),

	(   sub_atom(SourceLabel, 0, PrefixLength, _, Prefix)
	->  true
	;   Prefix=SourceLabel
	),
	downcase_atom(SourceLabel, SourceLabel0),
	snowball(Snowball_Language, SourceLabel0, SourceStem),

	% Target candidate generation based on prefixes...
	% This should be replaced by hash lookup on preprocessed stem table FIXME
	% Current implementation can miss stemmed matches because the prefix of the unstemmed labes do not match

	% backtrack over all candidates with prefix match:
	skos_has(Target, MatchProp2, literal(prefix(Prefix), lang(_TargetLang, _TargetLabel)), _TargetProp, Options),

	% backtrack over all labels of the current target candidate:
	skos_has(Target, MatchProp2, literal(lang(TargetLang, TargetLabel)), TargetProp, Options),
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
