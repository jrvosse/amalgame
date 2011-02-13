:- module(snowball_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(snowball)).
:- use_module(library(lit_distance)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidate)).

:- public parameter/3.
:- public filter/3.
:- public matcher/4.


parameter(sourcelabel,
	  [uri, default(P)],
	  'Property to get label of the source by') :-
	rdf_equal(rdfs:label, P).
parameter(targetlabel,
	  [uri, default(P)],
	  'Property to get the label of the target by') :-
	rdf_equal(rdfs:label, P).
parameter(language,
	  [atom, optional(true)],
	  'Language of source label').
parameter(matchacross_lang,
	  [boolean, default(true)],
	  'Allow labels from different language to be matched').
parameter(case_senstitive,
	  [boolean, default(false)],
	  'When true the case of labels must be equal').
parameter(snowball_language,
	  [atom, default(dutch)],
	  'Language to use for stemmer').
parameter(edit_distance,
	  [integer, default(0)],
	  'When >0 allow additional differences between labels').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter(Alignments, Mappings, Options) :-
	findall(M, ( member(A, Alignments),
		     match(A, M, Options)
		   ),
		Mappings).

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
  	option(snowball_language(Snowball_Language), Options),
 	option(sourcelabel(MatchProp1), Options),
	option(targetlabel(MatchProp2), Options),
	option(matchacross_lang(MatchAcross), Options),
	option(language(SourceLang),Options, _),
	option(edit_distance(Edit_Distance), Options),

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
	debug(align, 'snowball match: ~p ~p', [Source,Target]).
