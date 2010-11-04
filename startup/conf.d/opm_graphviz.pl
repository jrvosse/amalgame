:- module(opm_graph, []).


:- use_module(cliopatria(hooks)).
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
	),
	findall(T, context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []), 	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

context_triple(URI, Triple) :-
	up(URI, URI, Triples, [URI], 4),
	member(Triple, Triples).
context_triple(URI, Triple) :-
	down(URI, URI, Triples, [URI], 2),
	member(Triple, Triples).

context_triple(URI, rdf(URI, rdf:type, Class)) :-
	rdf(URI, rdf:type, Class),
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

transitive_context(opmv:used).
transitive_context(opmv:wasGeneratedBy).

blacklist(Orig, Overlap) :-
	rdfs_individual_of(Overlap, amalgame:'OverlapAlignment'),
	\+ rdfs_individual_of(Orig, amalgame:'OverlapAlignment'),
	\+ rdf_has(_Overlap, opmv:wasGeneratedBy,Orig).

blacklist(Orig, OverlapProcess) :-
	rdf_has(Overlap, opmv:wasGeneratedBy, OverlapProcess),
	rdfs_individual_of(Overlap, amalgame:'OverlapAlignment'),
	\+ rdfs_individual_of(Orig, amalgame:'OverlapAlignment').


cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, opmv:'Artifact'),
	Shape = [shape('Mdiamond'),style(filled),fillcolor('#FF8888')].

cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, opmv:'Process'),
	Shape = [shape('box'), style(filled),fillcolor('#FF8888')].


cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, amalgame:'Alignment'),
	Shape = [shape(box3d), style(filled),fillcolor('#AAAAAA')].
cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, skos:'ConceptScheme'),
	Shape = [shape(box),style(filled),fillcolor('#AAAAAA')].
