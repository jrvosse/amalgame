:- module(isub_match,
	  [isub_match/3]).

:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(isub)).
:- use_module(string_match_util).


isub_match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(skos:definition, DefaultProp),
	option(threshold(Threshold), Options, 0.0),
	option(sourcelabel(MatchPropS), Options, DefaultProp),
	option(targetlabel(MatchPropT), Options, DefaultProp),
	option(matchacross_lang(MatchAcross), Options, true),
	option(normalize(Normalize), Options, false),
	option(source_language(Lang), Options, 'any'),

	(   Lang == 'any'
	->  SourceLang = _
	;   SourceLang = Lang
	),

	% If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),


	skos_has(Source, MatchPropS, SourceLit@SourceLang, SourceProp, Options),
	skos_has(Target, MatchPropT, TargetLit@TargetLang, TargetProp, Options),
	Source \== Target,
	literal_text(SourceLit, SourceTxt),
	literal_text(TargetLit, TargetTxt),
	isub(SourceTxt, TargetTxt, Normalize, Similarity),
	Similarity > Threshold,
	Prov = [method(isub),
		match(Similarity),
		graph([rdf(Source, SourceProp, SourceLit),
		       rdf(Target, TargetProp, TargetLit)])
	       ].
