:- module(opm_graph, []).


:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).

:- set_setting_default(graphviz:format, svg).
:- rdf_meta
        transitive_context(r).

cliopatria:context_graph(URI, RDF) :-
	rdfs_individual_of(URI, opmv:'Artifact'),
	findall(T, context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []), 	% Create bags of similar resources
	append(RDF3, Bags, RDF).

context_triple(URI, Triple) :-
	transitive_context(CP),
	parents(URI, CP, Triples, [URI], 5),
	member(Triple, Triples).

context_triple(URI, rdf(URI, RP, Process)) :-
	rdf_has(URI, opmv:wasGeneratedBy, Process, RP).

parents(URI, Up, [rdf(URIx, P, Parentx)|T], Visited, MaxD) :-
	succ(MaxD2, MaxD),
	(   rdf_has(URI, Up, Parent, P), URIx = URI, Parentx = Parent
	;   rdf_has(Parent, Up, URI, P), URIx = Parent, Parentx = URI
	),
	\+ memberchk(Parent, Visited),
	parents(Parent, Up, T, [Parent|Visited], MaxD2).
parents(_, _, [], _, _).

transitive_context(opmv:wasDerivedFrom).
