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
	option(prefixLength(PrefixLength), Options, 3),
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
	snowball(Language, SourceLabel, Stem),
	sub_atom(Stem, 0, PrefixLength, _, Prefix),
	rdf_find_literals(prefix(Prefix), Ls),
	member(TargetLabel, Ls),
	snowball(Language, TargetLabel, Stem),
	rdf_has(Target, MatchProp2, literal(lang(TargetLang, TargetLabel)), TargetProp),
 	Source \== Target,
	Prov = [method(snowball),
		match(Language),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ].
