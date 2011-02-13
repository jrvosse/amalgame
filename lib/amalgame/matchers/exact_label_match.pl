:- module(exact_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public matcher/4.
:- public params/1.

matcher(Source, Target, Mappings, Options) :-
	findall(A, align(Source, Target, A, Options), Mappings).

params([sourcelabel(_, [default(DefaultProp)]),
	targetlabel(_, [default(DefaultProp)])
       ]) :-
	rdf_equal(rdfs:label, DefaultProp).

align(Source, Target, Match, Options) :-
	graph_member(S, Source),
	match(align(S,T,[]), Match, Options),
	graph_member(T, Target).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
 	rdf_equal(rdfs:label, DefaultProp),
 	(   memberchk(sourcelabel(MatchProp1, _), Options)
	->  true
	;   option(sourcelabel(MatchProp1), Options, DefaultProp)
	),
	(   memberchk(targetlabel(MatchProp2, _), Options)
	->  true
	;   option(targetlabel(MatchProp2), Options, DefaultProp)
	),
	option(matchacross_lang(MatchAcross), Options, _),
	option(language(SourceLang),Options, _),
	option(case_sensitive(CaseSensitive), Options, false),

	(   var(SourceLang)
	->  LangString = 'all'
	;   LangString = SourceLang
	),
	(   CaseSensitive==true
	->  CaseString=cs;
	    CaseString=ci
	),

        % If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	rdf_has(Target, MatchProp2, literal(exact(SourceLabel),lang(TargetLang, TargetLabel)), TargetProp),
 	Source \== Target,
	Prov = [method(exact_label),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ].
