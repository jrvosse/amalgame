:- module(opm_graph, []).


:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).

:- set_setting_default(graphviz:format, svg).
:- rdf_meta
        context_triple(r, t),
        transitive_context(r).

cliopatria:context_graph(URI, RDF) :-
	(   rdfs_individual_of(URI, opmv:'Artifact')
	;   rdfs_individual_of(URI, opmv:'Process')
	;   rdfs_individual_of(URI, opmv:'Agent')
	;   rdfs_individual_of(URI, skos:'ConceptScheme')
	;   rdfs_individual_of(URI, amalgame:'Alignment')
	),
	findall(T, opm_context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []), 	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

cliopatria:context_graph(URI, RDF) :-
	rdfs_individual_of(URI, align:'Cell'),
	findall(Triple, cell_context_triple(URI, Triple), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []), 	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

cell_context_triple(URI, rdf(URI, P, E1)):- rdf_has(URI, align:entity1, E1, P).
cell_context_triple(URI, rdf(URI, P, E1)):- rdf_has(URI, align:entity2, E1, P).
cell_context_triple(URI, rdf(URI, P, E1)):- rdf_has(URI, align:relation, E1, P).
cell_context_triple(URI, rdf(Al , P, URI)):- rdf_has(Al, align:map,    URI, P).
cell_context_triple(URI, rdf(Other, P, E)):- other(URI, rdf(Other, P, E)).
cell_context_triple(URI, rdf(Other, P, R)) :-
	other(URI, rdf(Other, _, _)),
	rdf_has(Other, align:relation, R, P).

other(URI, rdf(Other, P, E1)):-
	rdf_has(URI, align:entity1, E1),
	rdf_has(Other, align:entity1, E1, P),
	Other \= URI.
other(URI, rdf(Other, P, E2)):-
	rdf_has(URI, align:entity2, E2),
	rdf_has(Other, align:entity2, E2, P),
	Other \= URI.

opm_context_triple(URI, Triple) :-
	up(URI, URI, Triples, [URI], 3),
	member(Triple, Triples).
opm_context_triple(URI, Triple) :-
	down(URI, URI, Triples, [URI], 2),
	member(Triple, Triples).

opm_context_triple(URI, rdf(URI, P, Class)) :-
	rdf_has(URI, rdf:type, Class, P),
	rdf_global_id(amalgame:_, Class).

up(Orig,URI, [rdf(URI, P, Parent)|T], Visited, MaxD) :-
	succ(MaxD2, MaxD),
	transitive_context(Up),
	rdf_has(URI, Up, Parent, P),
	\+ blacklist(Orig,Parent),
	\+ memberchk(Parent, Visited),
	up(Orig, Parent, T, [Parent|Visited], MaxD2).
up(_,_, [], _, _).

down(Orig, URI, [rdf(Child, P, URI)|T], Visited, MaxD) :-
	succ(MaxD2, MaxD),
	transitive_context(Up),
	rdf_has(Child, Up, URI, P),
	\+ blacklist(Orig, Child),
	\+ memberchk(Child, Visited),
	down(Orig, Child, T, [Child|Visited], MaxD2).
down(_,_, [], _, _).

transitive_context(owl:versionInfo).
transitive_context(opmv:used).
transitive_context(opmv:wasGeneratedBy).
transitive_context(opmv:wasPerformedBy).

blacklist(Orig, Overlap) :-
	rdfs_individual_of(Overlap, amalgame:'OverlapAlignment'),
	\+ rdfs_individual_of(Orig, amalgame:'OverlapAlignment'),
	\+ rdf_has(_Overlap, opmv:wasGeneratedBy,Orig).

blacklist(Orig, OverlapProcess) :-
	rdf_has(Overlap, opmv:wasGeneratedBy, OverlapProcess),
	rdfs_individual_of(Overlap, amalgame:'OverlapAlignment'),
	\+ rdfs_individual_of(Orig, amalgame:'OverlapAlignment').


% Alignment made with amalgame:
cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, opmv:'Artifact'),
	rdfs_individual_of(URI, amalgame:'Alignment'),
	rdf_has(URI, opmv:wasGeneratedBy, _),
	Shape = [shape('Mdiamond'),fontize('20.00'), style(filled),fillcolor('#FF8888')].
% Amalgame process:
cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, opmv:'Process'),
	Shape = [shape('box'), style(filled),fillcolor('#FF8888')].

% Alignment (up)loaded, typically not made with amalgame
cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, amalgame:'Alignment'),
	Shape = [shape('Mdiamond'), style(filled),fillcolor('#AAAAAA')].

% Vocabulary (up)loaded, typically not made with amalgame
cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, skos:'ConceptScheme'),
	Shape = [shape(box3d),style(filled),fillcolor('#AAAAAA')].
