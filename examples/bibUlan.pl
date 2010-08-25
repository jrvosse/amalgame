% Example alignment, aligningbibliopolis Persons to ULAN using amalgame

:- module(bib_ulan_align,
	 [
	  run/0,
	  prun/0,
	  load/0
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(amalgame(matchers/skosmatcher)).
:- use_module(amalgame(rankers/skosranker)).
:- use_module(amalgame(mappings/edoal)).

load:-
	load_bibliopolis,
	load_ulan,
	prepare.


load_bibliopolis:-
	rdf_load('../../demo/metadata/bibliopolis/rdf/persons.ttl'),
	rdf_load('../examples/bib2skosmap.rdfs').
load_ulan:-
	rdf_load('../examples/gettyskosmap.rdfs'),
	rdf_load('../examples/ulan.rdfs'),
	rdf_load('../examples/vp.rdfs'),
	rdf_load('../examples/ulan-dutch.rdf').

prun:- profile(run).

run:-
	runmatcher1,
	runmatcher2.

runmatcher1:-
	Graph = bibulan1,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(ulan, Ulan),
	voc(bibliopolis, Bib),
		Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Bib),
		   ontology2(Ulan),
		   candidate_matchers([labelmatch])
			  ],
	find_candidates(Options).


runmatcher2:-
	Graph = bibulan2,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(ulan, Ulan),
	voc(bibliopolis, Bib),
		Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Bib),
		   ontology2(Ulan),
		   candidate_matchers([stringdist])
			  ],
	find_candidates(Options).

voc(ulan,'http://purl.org/vocabularies/getty/ulan').
voc(bibliopolis, 'http://purl.org/collections/bibliopolis').

% rdf assert the skos:inScheme info
prepare:-
	voc(ulan,UlanScheme),
	rdf_transaction((
	forall(rdf(A, rdf:type, 'http://purl.org/vocabularies/getty/ulan#Person'),
	       rdf_assert(A, skos:inScheme, UlanScheme))
			)),

	voc(bibliopolis, BibScheme),
	rdf_transaction((
	forall(rdf(A, rdf:type, 'http://xmlns.com/foaf/0.1/Person'),
	       rdf_assert(A, skos:inScheme, BibScheme))
			)).


find_candidates(Options) :-
	voc(ulan,Ulan),
	debug(align, 'Finding source concepts.', []),
	findall(SourceConcept,
		rdfs_individual_of(SourceConcept,'http://xmlns.com/foaf/0.1/Person'),
		SourceConcepts
	       ),
	debug(align, 'Finding candidates mappings from scheme ~p~n', [ulan]),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
						    Ulan,
						    Options))
	      ).

