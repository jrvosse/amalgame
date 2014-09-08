:- module(compound_label_match,
	  [ compound_label_match/3 ]).

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(string_match_util).

compound_label_match(align(Source, _Target, Prov0), Results, Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
	option(sourcelabel(MatchProp1), Options, RdfsLabel),
	option(source_language(Lang), Options, 'any'),

	(   Lang == 'any'
	->  SourceLang = _
	;   SourceLang = Lang
	),

	Prov = [method(compound_label_match),
		score(Score),
		match(Match),
		graph([rdf(Source, SourceProp,
			   literal(lang(SourceLang, SourceLabel)))])
	       ],

	skos_match(Source, MatchProp1,
		   literal(lang(SourceLang, SourceLabel)), SourceProp, Options),
	rdf_tokenize_literal(SourceLabel, Tokens),
	findall(Targets-Token-LabelAmbScore,
		(   member(Token, Tokens),
		    atom(Token),
		    match_label(Source, Token, Targets,
				[source_lang(SourceLang)|Options]),
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
	option(source_lang(SourceLang), Options),

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
		(   skos_match(Target, MatchProp, SearchTarget,
			       TargetProp, Options),
		    (	option(target_scheme(TargetScheme), Options)
		    ->	vocab_member(Target, TargetScheme)
		    ;	true
		    ),
		    (   IgnoreType
		    ->  true
		    ;   matching_types(Source, Target)
		    ),
		    (   var(TargetLang)
		    ->  TargetTerm = literal(TargetLabel)
		    ;   TargetTerm = literal(lang(TargetLang, TargetLabel))
		    ),
		    ProvGraph = [rdf(Target, TargetProp, TargetTerm)]
		),
		Targets),
	Targets \= [].
