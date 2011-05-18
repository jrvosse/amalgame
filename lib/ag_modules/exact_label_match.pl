:- module(exact_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'ExactLabelMatcher').
amalgame_module(amalgame:'ExactLabelFilter').

parameter(sourcelabel, uri, P,
	  'Property to get label of the source by') :-
	rdf_equal(rdfs:label, P).
parameter(targetlabel, uri, P,
	  'Property to get the label of the target by') :-
	rdf_equal(rdfs:label, P).
parameter(language, atom, '', 'Language of source label').
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
	option(language(Lang), Options, ''),

	(   Lang == ''
	->  var(SourceLang)
	;   SourceLang = Lang
	),

	(   CaseSensitive
	->  SearchTarget=literal(lang(TargetLang, SourceLabel))
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
	;   match_type(Source, Target)
	).


match_type(S1, _) :- untyped(S1), true, !.
match_type(_ ,S2) :- untyped(S2), true, !.
match_type(S1, S2) :-
	findall(Type,
		(rdf(S1, rdf:type, Type),
		 \+ rdf_equal(Type,  skos:'Concept')
		), Types1),
	findall(Type,
		(rdf(S2, rdf:type, Type),
		 \+ rdf_equal(Type,  skos:'Concept')
		), Types2),
	member(T1, Types1), member(T2, Types2),
	(   T1 == T2
	->  true
	;   rdf_has(T1, skos:exactMatch, T2)
	->  true
	;   debug(ex_expand, 'Non matching types ~p/~p', [T1,T2]),
	    false
	),
	!.

untyped(S) :-
	rdf_equal(SkosConcept, skos:'Concept'),
	findall(Type, rdf(S, rdf:type, Type),[SkosConcept]).

