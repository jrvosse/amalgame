:- module(ancestor_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'AncestorMatcher').
amalgame_module(amalgame:'AncestorFilter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct parents only').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter(In, Out, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	filter_(In, BackgroundMatches, Out, Options).

filter_([], _, [], _).
filter_([align(S,T,P)|Cs], BackgroundMatches, [C|Mappings], Options) :-
	(   T = scheme(_)
	->  match(align(S,_,P), BackgroundMatches, C, Options),
	    C=align(_,T2,_),
	    vocab_member(T2, T)
	;   match(align(S,T,P), BackgroundMatches, C, Options)
	),
	!,
	filter_(Cs, BackgroundMatches, Mappings, Options).
filter_([_|Cs], BackgroundMatches, Mappings, Options) :-
	filter_(Cs, BackgroundMatches, Mappings, Options).


%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	findall(M, align(Source, Target, BackgroundMatches, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, BackgroundMatches, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	match(align(S,T,[]), BackgroundMatches, Match, Options).


match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	ancestor(S, MaxSteps, AncS, R1, Steps1),
	ancestor(T, MaxSteps, AncT, R2, Steps2),
	get_assoc(AncS-AncT, BackgroundMatches, _),
	Prov = [method(ancestor_match),
		source_ancestor(AncS),
		target_ancestor(AncT),
		source_steps(Steps1),
		target_steps(Steps2),
		graph([R1,R2])
	       ].

ancestor(R, MaxSteps, Parent, rdf(R, Prop, Parent), Steps) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(R, Prop, Parent, MaxSteps, Steps),
	\+ R == Parent.
ancestor(R, MaxSteps, Parent, rdf(R, Broader, Parent), Steps) :-
	rdf_equal(skos:narrower, Narrower),
	rdf_equal(skos:broader, Broader),
	rdf_reachable(Parent, Narrower, R, MaxSteps, Steps),
	\+ R == Parent,
	\+ rdf_reachable(R, Broader, Parent).
