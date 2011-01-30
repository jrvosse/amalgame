:- module(related_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).

:- public match/3.
:- multifile amalgame:component/2.

amalgame:component(matcher, related_match(align(uri,uri,provenance), align(uri,uri,provenance), [])).

match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	option(graph(Graph), Options, _),
	related(S, RelS, R1),
	related(T, RelT, R2),
	has_map([RelS, RelT], _, Graph),
	Prov = [ method(related_match), graph([R1, R2 ]) ].

related(C, R, rdf(C, P, R)) :-
	rdf_has(C, skos:related, R, P),
	\+ C == R.
related(C, R, rdf(R, P, C)) :-
	rdf_has(R, skos:related, C, P),
	\+ C == R.
