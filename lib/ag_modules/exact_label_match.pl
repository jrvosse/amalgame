:- module(exact_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/voc_stats)).

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
parameter(language, oneof(['any'|L]), 'any', 'Language of source label') :-
	voc_property(all, languages(L)).
parameter(matchacross_lang, boolean, true,
	  'Allow labels from different language to be matched').
parameter(matchacross_type, boolean, true,
	  'Allow labels from different types to be matched').
parameter(case_sensitive, boolean, false,
	  'When true the case of labels must be equal').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter([], [], _).
filter([align(S,T,P)|Cs], [C|Mappings], Options) :-
	(   T = scheme(_)
	->  match(align(S,_,P), C, Options),
	    C=align(_,T2,_),
	    vocab_member(T2, T)
	;   match(align(S,T,P), C, Options)
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
	match(align(S,T,[]), Match, Options),
	vocab_member(T, Target).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(sourcelabel(MatchProp1), Options, RdfsLabel),
	option(targetlabel(MatchProp2), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(matchacross_type(IgnoreType),  Options, true),
	option(case_sensitive(CaseSensitive), Options, false),
	option(language(Lang), Options, 'any'),

	(   Lang == 'any'
	->  var(SourceLang)
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

	Prov = [method(exact_label),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ],

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	rdf_has(Target, MatchProp2, SearchTarget, TargetProp),
	Source \== Target,

	(   IgnoreType
	->  true
	;   matching_types(Source, Target)
	),

	% if matching label has no lang tag, these are still not grounded:
	(   var(SourceLang) -> SourceLang = undefined; true),
	(   var(TargetLang) -> TargetLang = undefined; true).
