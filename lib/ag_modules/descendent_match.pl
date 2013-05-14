:- module(descendent_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'DescendentMatcher').
amalgame_module(amalgame:'DescendentFilter').

parameter(steps, integer, 1, 'depth of search, defaults to 1, e.g. direct children only').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter(In, Out, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	list_to_assoc(KeyValueList, BackgroundMatches),
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
	list_to_assoc(SecList, BackgroundMatches),
	findall(M, align(Source, Target, BackgroundMatches, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, BackgroundMatches, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	match(align(S,T,[]), BackgroundMatches, Match, Options).


match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	descendent(S, MaxSteps, DescS, R1, Steps1),
	descendent(T, MaxSteps, DescT, R2, Steps2),
	get_assoc(DescS-DescT, BackgroundMatches, _),
	Prov = [method(descendent_match),
		source_descendent(DescS),
		target_descendent(DescT),
		source_steps(Steps1),
		target_steps(Steps2),
		graph([R1,R2])
	       ].

	/* FIXME: need to make a decision about what to do with align:relation ...
        */

descendent(R, MaxSteps, Child, rdf_reachable(R, Prop, Child), Steps) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(R, Prop, Child, MaxSteps, Steps),
	\+ R == Child.
descendent(R, MaxSteps, Child, rdf_reachable(Child, Prop, R), Steps) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(Child, Prop, R, MaxSteps, Steps),
	\+ R == Child,
	\+ rdf_reachable(R, skos:narrower, Child).
