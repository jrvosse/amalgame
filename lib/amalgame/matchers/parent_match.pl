:- module(parent_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, parent_match(align(uri,uri,provenance), align(uri,uri,provenance), [])).

match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	option(graph(Graph), Options, _),
	parent(S, ParentS, R1),
	parent(T, ParentT, R2),
	has_map([ParentS, ParentT], _, Graph),
	Prov = [method(parent_match),
		graph([R1,
		       R2
		       % add alignment
		      ])
	       ].

parent(R, Parent, rdf(R, Prop, Parent)) :-
	rdf_has(R, skos:broader, Parent, Prop).
parent(R, Parent, rdf(Parent, Prop, R)) :-
	rdf_has(Parent, skos:narrower, R, Prop),
	\+ rdf_has(R, skos:broader, Parent).
