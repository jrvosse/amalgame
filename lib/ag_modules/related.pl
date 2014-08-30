:- module(related_match_util,
	  [ related_match/4]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(skos/util)).

related_match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	related(S, MaxSteps, AncS, R1, Steps1),
	related(T, MaxSteps, AncT, R2, Steps2),
	get_assoc(AncS-AncT, BackgroundMatches, _),
	Prov = [method(related_match),
		source(AncS),
		target(AncT),
		steps(Steps1/Steps2),
		graph([R1,R2])
	       ].

related(R, MaxSteps, Related, rdf(R, Prop, Related), Steps) :-
	skos_related_to(R, Related, MaxSteps, Steps),
	rdf_equal(amalgame:relatedTransitive, Prop).
