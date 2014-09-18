:- module(isub_match,
	  [isub_match/3]).

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(isub)).

isub_match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(skos:definition, DefaultProp),
	option(threshold(Threshold), Options, 0.0),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
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

	SearchTarget=literal(lang(TargetLang, TargetLit)),

	(   rdf_has(Source, MatchProp1,  literal(lang(SourceLang, SourceLit)), SourceProp),
	    rdf_has(Target, MatchProp2, SearchTarget, TargetProp),
	    Source \== Target
	->  literal_text(SourceLit, SourceTxt),
	    literal_text(TargetLit, TargetTxt),
	    isub(SourceTxt, TargetTxt, Normalize, Similarity)
	;   Similarity = 0
	),
	Similarity > Threshold,
	Prov = [method(isub),
		match(Similarity),
		graph([rdf(Source, SourceProp, SourceLit),
		       rdf(Target, TargetProp, TargetLit)])
	       ].
