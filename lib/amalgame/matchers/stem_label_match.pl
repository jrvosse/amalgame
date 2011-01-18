:- module(stem_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, stem_label_match(align(uri, uri, provenance), align(uri,uri,provenance),
					      [sourcelabel(uri, [default(rdfs:label)]),
					       targetlabel(uri, [default(rdfs:label)])
					      ])).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
 	rdf_equal(rdfs:label, DefaultProp),
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
	rdf_find_literals(stem(SourceLabel), Ls),
	member(TargetLabel, Ls),
	rdf_has(Target, MatchProp2, literal(lang(TargetLang, TargetLabel)), TargetProp),
 	Source \== Target,
	Prov = [method(stem_label),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ].
