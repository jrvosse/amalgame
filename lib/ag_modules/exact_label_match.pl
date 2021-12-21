:- module(exact_label_match, [
	      exact_label_match/3
	  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

%%	exact_label_match(?In, -Out, +Options) is non det.
%
%	In and Out are align(S,T,P) terms.
%	Typically, there are to modes for calling this predicate:
%
%       * Both S and T of In are instantiated, and the predicate is
%	  called to find a match between S and T, or
%	* Only S is ground and the predicate is called to find a
%	matching T.
exact_label_match(align(Source, Target, Prov0),
		  align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(amalgame:label, RdfsLabel),
	option(sourcelabel(MatchPropS), Options, RdfsLabel),
	option(targetlabel(MatchPropT), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(matchacross_type(IgnoreType),  Options, true),
	option(case_sensitive(CaseSensitive), Options, false),
	option(source_language(Lang), Options, 'any'),
        debug(exact_label_match, 'Source ~p', [Source]),
	(   Lang == 'any'
	->  SourceLang = _UnBound
	;   SourceLang = Lang
	),

	skos_has(Source, MatchPropS, SourceLabel@SourceLang, SourceProp, Options),
	SourceLabel \= '',

	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	SearchTarget = TargetLabel@TargetLang,
	(   CaseSensitive
	->  TargetLabel = SourceLabel
	;   rdf11:rdf_where(icase(TargetLabel, SourceLabel))
	),

	skos_has(Target, MatchPropT, SearchTarget, TargetProp, Options),
	Source \= Target,

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
	->  SourceTerm = SourceLabel^^xsd:string
	;   SourceTerm = SourceLabel@SourceLang
	),

	(   var(TargetLang)
	->  TargetTerm = TargetLabel^^xsd:string
	;   TargetTerm = TargetLabel@TargetLang
	),

	Prov = [method(exact_label),
		graph([rdf(Source, SourceProp, SourceTerm),
		       rdf(Target, TargetProp, TargetTerm)])
	       ].
