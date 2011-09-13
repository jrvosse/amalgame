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
	;   matching_types(Source, Target)
	).

%%	matching_types(+S, +T) is semidet.
%
%	Fails if S and T have conflicting types.
%       Succeeds if
%	* S or T have no types other than skos:Concept, or,
%	* S and T have equal types other than skos:Concept, or,
%	* S and T have different types, other than skos:Concept,
%         and one is the subclass of the other.

matching_types(S1, S2) :-
	(   (rdf(S1, rdf:type, T1),  \+ rdf_equal(T1, skos:'Concept'))
	->  ((rdf(S2, rdf:type, T2), \+ rdf_equal(T2, skos:'Concept'))
	    ->  T1 == T2
	    ->  true
	    ;   rdfs_subclass_of(T1, T2)
	    ->  true
	    ;   rdfs_subclass_of(T2, T1)
	    ->  true
	    ;   debug(ex_expand, 'Non matching types ~p/~p', [T1,T2]),
		false
	    )
	;   true)
	,!.
