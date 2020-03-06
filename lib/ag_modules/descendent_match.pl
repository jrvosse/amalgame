:- module(descendent_match_util,
	  [ descendent_match/4
	  ]).

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(skos/util)).

descendent_match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	descendent(S, MaxSteps, DesS, RS, StepsS),
	descendent(T, MaxSteps, DesT, RT, StepsT),
	(   DesS == DesT
	;   get_assoc(DesS-DesT, BackgroundMatches, _)
	),
	Prov = [method(descendent_match),
		source(DesS),
		target(DesT),
		steps((StepsS, StepsT)),
		graph([RS,RT])
	       ].

descendent(R, MaxSteps, Descendent, rdf(R, Prop, Descendent), Steps) :-
	skos_descendant_of(R, Descendent, MaxSteps, Steps),
	rdf_equal(amalgame:descendant, Prop).
