:- module(exact_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, exact_label_match(candidate(uri, uri), graph, [sourcelabel(uri, [default(rdfs:label)]),
									   targetlabel(uri, [default(rdfs:label)])
									  ])).

match(candidate(Source, Target), Output, Options) :-
	rdf_equal(rdfs:label, DefaultProp),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(matchacross_lang(MatchAcross), Options, _),
	option(language(SourceLang),Options, _),
	option(case_sensitive(CaseSensitive), Options, false),

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	(   var(SourceLang)
	->  LangString = 'all'
	;   LangString = SourceLang
	),
	(   CaseSensitive==true
	->  CaseString=cs;
	    CaseString=ci
	),

        % If we can't match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),
	rdf_has(Target, MatchProp2, literal(exact(SourceLabel),lang(TargetLang, TargetLabel)), TargetProp),
	Output = [rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		  rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))].

