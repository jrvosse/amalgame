:- module(prefix_candidate,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_db)).

:- public candidate/3.
:- multifile amalgame:component/2.

amalgame:component(candidate, prefix_candidate(schemes(uri, uri), align(uri, uri, provendence_list), [])).

%%	candidate_generator(+Input, -Output, +Options)

candidate(schemes(SourceScheme, TargetScheme), align(Source, Target, []), Options) :-
	rdf_equal(rdfs:label, DefaultProp),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(prefixLength(PrefixLength), Options, 2),
 	rdf_has(Source, skos:inScheme, SourceScheme),
	rdf_has(Source, MatchProp1, Lit),
	literal_text(Lit, Label),
	sub_atom(Label, 0, PrefixLength, _, Prefix),
	rdf_has(Target, MatchProp2, literal(prefix(Prefix), _)),
	rdf_has(Target, skos:inScheme, TargetScheme),
 	(   option(exclude(Graph), Options)
	->  \+ member(align(Source,_,_), Graph)
	;   true
	).


