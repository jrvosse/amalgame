:- module(compound_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'CompoundMatcher').
%amalgame_module(amalgame:'CompoundFilter').

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
	flatten(Mappings0, MappingsFlat),
	sort(MappingsFlat, Mappings).

align(Source, TargetScheme, Match, Options) :-
	vocab_member(S, Source),
	match(align(S,TargetScheme,[]), Match, Options).

match(align(Source, TargetScheme, []), Results, Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(sourcelabel(MatchProp1), Options, RdfsLabel),
	option(language(Lang), Options, ''),

	(   Lang == ''
	->  var(SourceLang)
	;   SourceLang = Lang
	),

	Prov = [method(compound_label_match),
		score(Score),
		match(Match),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel)))])
	       ],

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	rdf_tokenize_literal(SourceLabel, Tokens),
	findall(Target-TargetProv,
		(   member(Token, Tokens),
		    term_to_atom(Token, Label),
		    match_label(Source, Label, Target, TargetProv,
				[sourcelang(SourceLang)|Options]),
		    vocab_member(Target, TargetScheme)
		),
		Targets),
	length(Tokens, TokenLength),
	length(Targets, NrMatched),
	NrMatched > 1,
	Match is NrMatched/TokenLength,
	format(atom(Score), 'Matched ~w out of ~w parts', [NrMatched, TokenLength]),
	create_results(Targets, Source, Prov, Results).

create_results([], _, _, []).
create_results([Target-TargetProvGraph|Tail], Source,
	       OverallProv, [align(Source, Target, [graph(ProvGraph)|Rest])|Results]) :-
	select_option(graph(Graph), OverallProv, Rest),
	append(Graph, TargetProvGraph, ProvGraph),

	create_results(Tail, Source, OverallProv, Results).

match_label(Source, Label, Target, ProvGraph, Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(targetlabel(MatchProp), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(matchacross_type(IgnoreType),  Options, true),
	option(case_sensitive(CaseSensitive), Options, false),
	option(sourcelang(SourceLang), Options),

	(   CaseSensitive
	->  SearchTarget=literal(lang(TargetLang, Label))
	;   SearchTarget=literal(exact(Label), lang(TargetLang, TargetLabel))
	),

        % If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	ProvGraph = [rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))],

	rdf_has(Target, MatchProp, SearchTarget, TargetProp),
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

matching_types(S1, _) :- untyped(S1), true, !.
matching_types(_ ,S2) :- untyped(S2), true, !.
matching_types(S1, S2) :-
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
	;   rdfs_subclass_of(T1, T2)
	->  true
	;   rdfs_subclass_of(T2, T1)
	->  true
	;   debug(ex_expand, 'Non matching types ~p/~p', [T1,T2]),
	    false
	),
	!.

%%	untyped(+C) is semidet.
%
%	C is considered untyped if skos:Concept is its only type.

untyped(S) :-
	ground(S),
	rdf_equal(SkosConcept, skos:'Concept'),
	findall(Type, rdf(S, rdf:type, Type),[SkosConcept]).


