:- module(candidate,
	  [ prefix_candidate/4
	  ]).

:- use_module(library(amalgame/vocabulary)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_db)).

prefix_candidate(Source, Target, align(S, T, []), Options) :-
	rdf_equal(rdfs:label, DefaultProp),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(prefixLength(PrefixLength), Options, 4),
	vocab_member(S, Source),
 	rdf_has(S, MatchProp1, Lit),
	literal_text(Lit, Label),
	sub_atom(Label, 0, PrefixLength, _, Prefix),
	rdf_has(T, MatchProp2, literal(prefix(Prefix), _)),
	vocab_member(T, Target).
