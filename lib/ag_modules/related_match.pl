:- module(related_match,
	  [ related_match/4]).

:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(skos/util)).

related_match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	related(S, MaxSteps, RelS, RS, StepsS),
	related(T, MaxSteps, RelT, RT, StepsT),
	(   RelS == RelT
	;   get_assoc(RelS-RelT, BackgroundMatches, _)
	),
	Prov = [method(related_match),
		source(RelS),
		target(RelT),
		steps((StepsS,StepsT)),
		graph([RS,RT])
	       ].

related(R, MaxSteps, Related, rdf(R, Prop, Related), Steps) :-
	skos_related_to(R, Related, MaxSteps, Steps),
	rdf_equal(amalgame:relatedTransitive, Prop).
