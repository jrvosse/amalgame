:- module(compound_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'CompoundMatcher').

parameter(sourcelabel, oneof(LabelProps), Default,
	  '(Super)Property to get label of the source by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(targetlabel, oneof(LabelProps), Default,
	  '(Super)Property to get the label of the target by') :-
	rdf_equal(Default, rdfs:label),
	label_list(LabelProps).
parameter(language, oneof(['any'|L]), 'any', 'Language of source label') :-
	strategy_languages(_,L).
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

match(align(Source, TargetScheme, Prov0), Results, Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(sourcelabel(MatchProp1), Options, RdfsLabel),
	option(language(Lang), Options, 'any'),

	(   Lang == 'any'
	->  SourceLang = _
	;   SourceLang = Lang
	),

	Prov = [method(compound_label_match),
		score(Score),
		match(Match),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel)))])
	       ],

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	rdf_tokenize_literal(SourceLabel, Tokens),
	findall(Targets-Token-LabelAmbScore,
		(   member(Token, Tokens),
		    atom(Token),
		    match_label(Source, Token, Targets,
				[scheme(TargetScheme), sourcelang(SourceLang)|Options]),
		    length(Targets, LabelAmbScore)

		),
		Targets),
	length(Tokens, TokenLength),
	length(Targets, NrMatched),
	NrMatched > 0,
	Match is NrMatched/TokenLength,
	format(atom(Score), 'Matched ~w out of ~w parts', [NrMatched, TokenLength]),
	create_results(Targets, Source, Prov0, Prov, Results).

create_results([], _, _, _, []).
create_results([Targets|Tail], Source, OldProv, MatchProv, Results):-
	create_result_list(Targets, Source, OldProv, MatchProv, Results0),
	create_results(Tail, Source, OldProv, MatchProv, Results1),
	append(Results0, Results1, Results).

create_result_list([]-_-_, _, _, _, []).
create_result_list([T-TProv|Tail]-L-Count, Source, OldProv, MatchProv, [A|Results]):-
	create_result(T-L-TProv-Count, Source, OldProv, MatchProv, A),
	create_result_list(Tail-L-Count, Source, OldProv, MatchProv, Results).

create_result(Target-L-TargetProvGraph-Count, Source, OldProv, MatchProv,
	      align(Source, Target, [[token(L),token_ambiguity(Count),graph(ProvGraph)|Rest]|OldProv])) :-
	select_option(graph(Graph), MatchProv, Rest),
	append(Graph, TargetProvGraph, ProvGraph).

match_label(Source, Label, Targets, Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(targetlabel(MatchProp), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(matchacross_type(IgnoreType),  Options, true),
	option(case_sensitive(CaseSensitive), Options, false),
	option(sourcelang(SourceLang), Options),
	option(scheme(TargetScheme), Options),

	(   CaseSensitive
	->  SearchTarget=literal(lang(TargetLang, Label))
	;   SearchTarget=literal(exact(Label), lang(TargetLang, TargetLabel))
	),

        % If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	findall(Target-ProvGraph,
		(   rdf_has(Target, MatchProp, SearchTarget, TargetProp),
		    Source \== Target,  % fix me, replace by target vocab check as in exact_label example
		    vocab_member(Target, TargetScheme),
		    (   IgnoreType
		    ->  true
		    ;   matching_types(Source, Target)
		    ),
		    ProvGraph = [rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))]
		),
		Targets),
	Targets \= [].
