:- module(exact_label_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public filter/3.
:- public matcher/4.
:- public parameter/3.

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
parameter(case_sensitive,
	  [boolean, default(false)],
	  'When true the case of labels must be equal').


%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter(Alignments, Mappings, Options) :-
	findall(M, align(Alignments, M, Options), Mappings).

align(Alignments, M, Options) :-
	graph_member(A, Alignments),
	A = align(S,T,P),
	(   T = scheme(TargetVoc) ->
	    match(align(S,T2,P), M, Options),
	    graph_member(T2, TargetVoc)
	;   match(A,M,Options)
	).



%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	findall(M, align(Source, Target, M, Options), Mappings).

align(Source, Target, Match, Options) :-
	graph_member(S, Source),
	match(align(S,T,[]), Match, Options),
	graph_member(T, Target).



match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(rdfs:label, RdfsLabel),
 	option(sourcelabel(MatchProp1), Options, RdfsLabel),
 	option(targetlabel(MatchProp2), Options, RdfsLabel),
	option(matchacross_lang(MatchAcross), Options, true),
	option(language(SourceLang), Options, _),
	option(case_sensitive(CaseSensitive), Options, false),

	% FIX ME, case option not yet used in matching!
	(   CaseSensitive
	->  CaseString=cs, SearchTarget=literal(lang(TargetLang, TargetLabel))
	;   CaseString=ci, SearchTarget=literal(exact(SourceLabel), lang(TargetLang, TargetLabel))
	),

        % If we cannot match across languages, set target language to source language
	(   MatchAcross == false
	->  TargetLang = SourceLang
	;   true
	),

	Prov = [method(exact_label/CaseString),
		graph([rdf(Source, SourceProp, literal(lang(SourceLang, SourceLabel))),
		       rdf(Target, TargetProp, literal(lang(TargetLang, TargetLabel)))])
	       ],

	rdf_has(Source, MatchProp1, literal(lang(SourceLang, SourceLabel)), SourceProp),
	rdf_has(Target, MatchProp2, SearchTarget, TargetProp),
 	Source \== Target.

