:- module(snowball_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(snowball)).
:- use_module(library(lit_distance)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidate)).

:- public amalgame_module/2.
:- public parameter/4.
:- public filter/3.
:- public matcher/4.

amalgame_module(amalgame:'Matcher', amalgame:'Snowball_Matcher').

parameter(sourcelabel, uri, P,
	  'Property to get label of the source by') :-
	rdf_equal(rdfs:label, P).
parameter(targetlabel, uri, P,
	  'Property to get the label of the target by') :-
	rdf_equal(rdfs:label, P).
parameter(language, atom, '',
	  'Language of source label').
parameter(matchacross_lang, boolean, true,
	  'Allow labels from different language to be matched').
parameter(case_senstitive, boolean, false,
	  'When true the case of labels must be equal').
parameter(snowball_language, atom, dutch,
	  'Language to use for stemmer').
parameter(edit_distance, integer, 0,
	  'When >0 allow additional differences between labels').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on matching stemmed labels.

filter([], [], _).
filter([C0|Cs], [C|Mappings], Options) :-
	match(C0, C, Options),
	!,
	filter(Cs, Mappings, Options).
filter([_|Cs], Mappings, Options) :-
	filter(Cs, Mappings, Options).

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	findall(A, align(Source, Target, A, Options), Mappings).

align(Source, Target, Match, Options) :-
	option(prefix(0), Options),
	!,
	graph_member(S, Source),
	match(align(S,T,[]), Match, Options),
	graph_member(T, Target).

align(Source, Target, Match, Options) :-
 	prefix_candidate(Source, Target, Match0, Options),
	match(Match0, Match, Options).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(rdfs:label,DefaultP),
  	option(snowball_language(Snowball_Language), Options, dutch),
 	option(sourcelabel(MatchProp1), Options, DefaultP),
	option(targetlabel(MatchProp2), Options, DefaultP),
	option(matchacross_lang(MatchAcross), Options, true),
	option(language(SourceLang),Options, _),
	option(edit_distance(Edit_Distance), Options, 0),

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),

        % If we can't match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),
	\+ Source == Target,
	snowball(Snowball_Language, SourceLabel, SourceStem0),
	downcase_atom(SourceStem0, SourceStem),
	rdf_has(Target, MatchProp2, literal(lang(TargetLang, TargetLabel)), TargetProp),
	snowball(Snowball_Language, TargetLabel, TargetStem0),
	downcase_atom(TargetStem0, TargetStem),
	(   Edit_Distance == 0
	->  TargetStem == SourceStem
	;   literal_distance(SourceStem, TargetStem, Distance),
	    Distance =< Edit_Distance
	),
 	Prov = [method(snowball),
 		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ],
	debug(align_result, 'snowball match: ~p ~p', [Source,Target]).
