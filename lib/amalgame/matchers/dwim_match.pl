:- module(dwim_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, dwim_match(align(uri, uri, provenance), align(uri,uri,provenance),
					      [sourceprop(uri, [default(skos:definition)]),
					       targetprop(uri, [default(skos:definition)])
					      ])).

match(align(Source, Target, Prov0), align(Source, Target, [Prov|Prov0]), Options) :-
 	rdf_equal(skos:definition, DefaultProp),
 	option(sourceprop(MatchProp1), Options, DefaultProp),
	option(targetprop(MatchProp2), Options, DefaultProp),
	rdf_has(Source, MatchProp1, SourceLit, SourceProp),
	rdf_has(Target, MatchProp2, TargetLit, TargetProp),
	Source \== Target
	literal_text(SourceLit, SourceTxt),
	literal_text(TargetLit, TargetTxt),
	dwim_match(SourceTxt, TargetTxt, Difference)
 	Prov = [method(dwim_string_match),
		match(Difference),
		graph([rdf(Source, SourceProp, SourceLit),
		       rdf(Target, TargetProp, TargetLit)])
	       ].

