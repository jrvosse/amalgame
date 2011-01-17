:- module(ancestor_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, ancestor_match(align(uri,uri,provenance), align(uri,uri,provenance), [])).

match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	option(graph(Graph), Options, _),
	ancestor(S, AncS, R1),
	ancestor(T, AncT, R2),
	has_map([AncS, AncT], _, Graph),
	Prov = [method(ancestor_match),
		graph([R1,
		       R2
		      ])
	       ].

ancestor(R, Parent, rdf_reachable(R, Prop, Parent)) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(R, Prop, Parent).
ancestor(R, Parent, rdf_reachable(Parent, Prop, R)) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(Parent, Prop, R),
	\+ rdf_reachable(R, skos:broader, Parent).
