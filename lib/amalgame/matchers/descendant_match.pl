:- module(descendant_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, descendant_match(align(uri,uri,provenance), align(uri,uri,provenance), [])).

match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	option(graph(Graph), Options, _),
	descendant(S, DescS, R1),
	descendant(T, DescT, R2),
	has_map([DescS, DescT], _, Graph),
	Prov = [method(descendant_match),
		graph([R1,
		       R2
		      ])
	       ].

descendant(R, Child, rdf_reachable(Child, Prop, R)) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(Child, Prop, R),
	\+ Child == R.
descendant(R, Child, rdf_reachable(R, Prop, Child)) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(R, Prop, Child),
	\+ Child == R,
	\+ rdf_reachable(Child, skos:broader, R).
