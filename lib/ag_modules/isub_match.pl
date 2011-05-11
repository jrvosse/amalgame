:- module(isub_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(isub)).
:- use_module(library(amalgame/vocabulary)).

:- public filter/3.
:- public matcher/4.
:- public parameter/4.
:- public amalgame_module/1.

amalgame_module(amalgame:'IsubMatcher').
amalgame_module(amalgame:'IsubFilter').

parameter(sourcelabel, uri, P,
	  'Property to get label of the source by') :-
	rdf_equal(rdfs:label, P).
parameter(targetlabel, uri, P,
	  'Property to get the label of the target by') :-
	rdf_equal(rdfs:label, P).
parameter(threshold, float, -1.0,
	  'threshold edit distance').
parameter(language, atom, '',
	  'Language of source label').
parameter(matchacross_lang,
	  boolean, true,
	  'Allow labels from different language to be matched').
parameter(case_sensitive, boolean, false,
	  'When true the case of labels must be equal').

%%      filter(+MappingsIn, -MappingsOut, +Options)
%
%       Filter mappings based on exact matching of labels.

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


%%      matcher(+Source, +Target, -Mappings, +Options)
%
%       Mappings is a list of matches between instances of Source and
%       Target.

matcher(Source, Target, Mappings, Options) :-
        findall(M, align(Source, Target, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
        vocab_member(S, Source),
        vocab_member(T, Target),
        match(align(S,T,[]), Match, Options).


match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
 	rdf_equal(skos:definition, DefaultProp),
	option(threshold(Threshold), Options, 0.0),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	(   rdf_has(Source, MatchProp1, SourceLit, SourceProp),
	    rdf_has(Target, MatchProp2, TargetLit, TargetProp),
	    Source \== Target
	->  literal_text(SourceLit, SourceTxt),
	    literal_text(TargetLit, TargetTxt),
	    isub(SourceTxt, TargetTxt, true, Similarity)
	;   Similarity = 0
	),
	Similarity > Threshold,
 	Prov = [method(isub),
		match(Similarity),
		graph([rdf(Source, SourceProp, SourceLit),
		       rdf(Target, TargetProp, TargetLit)])
	       ].
