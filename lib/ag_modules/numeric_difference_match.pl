:- module(numeric_difference_match,
	  [numeric_difference_match/3]).

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

numeric_difference_match(align(Source, Target, Prov0),
			 align(Source, Target, [Prov|Prov0]), Options) :-
	rdf_equal(skos:definition, DefaultProp),
	option(threshold(Threshold), Options, 0.05),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),

	SearchTarget=literal(TargetLit),

	(   rdf_has(Source, MatchProp1,  literal(SourceLit), SourceProp),
	    rdf_has(Target, MatchProp2, SearchTarget, TargetProp),
	    Source \== Target
	->  literal_text(SourceLit, SourceTxt),
	    atom_number(SourceTxt, SourceNumber),
	    literal_text(TargetLit, TargetTxt),
	    atom_number(TargetTxt, TargetNumber),
	    Difference is abs(SourceNumber-TargetNumber)
	;   Difference = -1
	),
	Difference >= 0,
	Difference =< Threshold,
	Score is 0-Difference,
	Prov = [method(numeric_similarity),
		match(Score),
		graph([rdf(Source, SourceProp, SourceLit),
		       rdf(Target, TargetProp, TargetLit)])
	       ].
