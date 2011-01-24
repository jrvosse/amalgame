:- module(snowball_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(snowball)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(match, snowball(align(uri, uri, provenance), align(uri,uri,provenance),
					      [sourcelabel(uri, [default(rdfs:label)]),
					       targetlabel(uri, [default(rdfs:label)])
					      ])).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
 	rdf_equal(rdfs:label, DefaultProp),
 	option(language(Language), Options, dutch),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(matchacross_lang(MatchAcross), Options, _),
	option(language(SourceLang),Options, _),

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	(   var(SourceLang)
	->  LangString = 'all'
	;   LangString = SourceLang
	),

        % If we can't match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),
	\+ Source == Target,
	snowball(Language, SourceLabel, SourceStem0),
	downcase_atom(SourceStem0, SourceStem),
	rdf_has(Target, MatchProp2, literal(lang(TargetLang, TargetLabel)), TargetProp),
	snowball(Language, TargetLabel, TargetStem),
	downcase_atom(TargetStem, SourceStem),
 	Prov = [method(snowball),
 		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ].
