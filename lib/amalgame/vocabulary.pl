:- module(vocab,
	  [ vocab_member/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

%%	vocab_member(?C, +VocabDef)
%
%	True if C is part of Vocabulary.
%
%	@param VocabDef is a URI of a skos:ConceptScheme or a definition
%	of a subset thereof.

vocab_member(E, and(G1,G2)) :-
	!,
	vocab_member(E,G1),
	vocab_member(E,G2).
vocab_member(E, scheme(Scheme)) :-
	!,
	rdf_has(E, skos:inScheme, Scheme).
vocab_member(E, type(Class)) :-
	!,
	rdfs_individual_of(E, Class).
vocab_member(E, Scheme) :-
	rdf(Scheme, rdf:type, skos:'ConceptScheme'),
	!,
	rdf_has(E, skos:inScheme, Scheme).
vocab_member(I, EDMGraph) :-
	rdf(_, 'http://www.europeana.eu/schemas/edm/country',_, EDMGraph),
	!,
	rdf(I, 'http://www.europeana.eu/schemas/edm/country', _, EDMGraph).
vocab_member(E, Class) :-
	rdfs_individual_of(Class, rdfs:'Class'),
	!,
	rdfs_individual_of(E, Class).


