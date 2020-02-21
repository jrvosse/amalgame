:- module(compound_label_match,
	  [ compound_label_match/3 ]).

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

%%	compound_label_match(?In, -Out, +Options) is non det.
%
%	In and Out are align(S,T,P) terms.
%	Typically, there are to modes for calling this predicate:
%
%       * Both S and T of In are instantiated, and the predicate is
%	  called to find a match between S and T, or
%	* Only S is ground and the predicate is called to find a
%	  matching T.
compound_label_match(align(Source, Target, Prov0),
		  align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(amalgame:label, RdfsLabel),
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

	% If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	(   CaseSensitive
	->  SearchTarget=literal(lang(TargetLang, Token))
	;   SearchTarget=literal(exact(Token), lang(TargetLang, TargetLabel))
	),

	skos_has(Source, MatchPropS,
		   literal(lang(SourceLang, SourceLabel)),
		   SourceProp, Options),
	SourceLabel \= '',
	rdf_tokenize_literal(SourceLabel, Tokens),
	length(Tokens, TokenLength), TokenLength > 0,
	member(Token, Tokens), atom(Token),
	skos_has(Target, MatchPropT, SearchTarget, TargetProp, Options),

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

	Prov = [method(compound_label),
		score(score{match:Token, nrOfTokens:TokenLength}),
		graph([rdf(Source, SourceProp, SourceTerm),
		       rdf(Target, TargetProp, TargetTerm)])
	       ].
