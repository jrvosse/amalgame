:- module(ancestor_match_util,
	  [ ancestor_match/4
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(skos/util)).

ancestor_match(align(S, T, Prov0), BackgroundMatches,
	       align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	ancestor(S, MaxSteps, AncS, R1, Steps1),
	ancestor(T, MaxSteps, AncT, R2, Steps2),
	get_assoc(AncS-AncT, BackgroundMatches, _),
	Prov = [method(ancestor_match),
		source(AncS),
		target(AncT),
		steps(Steps1/Steps2),
		graph([R1,R2])
	       ].

ancestor(R, MaxSteps, Parent, rdf(R, Prop, Parent), Steps) :-
	skos_descendant_of(Parent, R, MaxSteps, Steps),
	rdf_equal(amalgame:ancestor, Prop).
