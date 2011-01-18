:- module(prefix_candidate,
	  [
	  ]
	 ).

:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_db)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate, prefix_candidate(align_source, align_source, align(uri, uri, provendence_list), [])).

%%	candidate(+Source, +Target, -Alignment, +Options)

candidate(Source, Target, align(S, T, []), Options) :-
	rdf_equal(rdfs:label, DefaultProp),
 	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(prefixLength(PrefixLength), Options, 4),
	graph_member(S, Source),
 	rdf_has(S, MatchProp1, Lit),
	literal_text(Lit, Label),
	sub_atom(Label, 0, PrefixLength, _, Prefix),
	rdf_has(T, MatchProp2, literal(prefix(Prefix), _)),
	graph_member(T, Target).


