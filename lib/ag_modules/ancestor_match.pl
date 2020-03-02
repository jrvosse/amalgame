:- module(ancestor_match,
	  [ ancestor_match/4
	  ]).

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(skos/util)).

ancestor_match(align(S, T, Prov0), BackgroundMatches,
	       align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	ancestor(S, MaxSteps, AncS, RS, StepsS),
	ancestor(T, MaxSteps, AncT, RT, StepsT),
	get_assoc(AncS-AncT, BackgroundMatches, _),
	Prov = [method(ancestor_match),
		source(AncS),
		target(AncT),
		steps((StepsS, StepsT)),
		graph([RS,RT])
	       ].

ancestor(R, MaxSteps, Parent, rdf(R, Prop, Parent), Steps) :-
	skos_descendant_of(Parent, R, MaxSteps, Steps),
	rdf_equal(amalgame:ancestor, Prop).
