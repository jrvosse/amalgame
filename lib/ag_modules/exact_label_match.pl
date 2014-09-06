:- module(exact_label_match, [
	      exact_label_match/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

exact_label_match(align(Source, Target, Prov0),
		  align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(sourcelabel(MatchPropS), Options, RdfsLabel),
	option(targetlabel(MatchPropT), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(matchacross_type(IgnoreType),  Options, true),
	option(case_sensitive(CaseSensitive), Options, false),
	option(source_language(Lang), Options, 'any'),
	(   Lang == 'any'
	->  SourceLang = _UnBound
	;   SourceLang = Lang
	),

	(   CaseSensitive
	->  SearchTarget=literal(lang(TargetLang, SourceLabel)),
	    TargetLabel = SourceLabel
	;   SearchTarget=literal(exact(SourceLabel), lang(TargetLang, TargetLabel))
	),

        % If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	skos_match(Source, MatchPropS,
		   literal(lang(SourceLang, SourceLabel)),
		   SourceProp, Options),
	SourceLabel \= '',
	skos_match(Target, MatchPropT, SearchTarget,
		TargetProp, Options),

	(   option(target_scheme(TargetScheme), Options)
	->  vocab_member(Target, TargetScheme)
	;   true
	),

	(   IgnoreType
	->  true
	;   matching_types(Source, Target)
	),

	% if matching label has no lang tag, these are still not grounded:
	(   var(SourceLang)
	->  SourceTerm = literal(SourceLabel)
	;   SourceTerm = literal(lang(SourceLang, SourceLabel))
	),

	(   var(TargetLang)
	->  TargetTerm = literal(TargetLabel)
	;   TargetTerm = literal(lang(TargetLang, TargetLabel))
	),

	Prov = [method(exact_label),
		graph([rdf(Source, SourceProp, SourceTerm),
		       rdf(Target, TargetProp, TargetTerm)])
	       ].
