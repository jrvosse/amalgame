% Example alignment, aligning aquatic Thesauri that are of interest to
% Carmen
%

:- module(aquatic_align,
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
	load_gemet,
	load_asfa,
	load_agrovoc,
	prepare.


load_gemet:-
	rdf_load('../../../Vocs/Carmen/GEMET.skos.xml').
load_asfa:-
       	rdf_load('../../../Vocs/Carmen/ASFATh_Skos.xml.rdf').
load_agrovoc:-
       	rdf_load('../../../Vocs/Carmen/ag_skos_080422.rdf').


prun:- profile(run).


% Run the English label only matcher on all three combinations of
% two vocabularies.
run:-
	runmatcher1,
	runmatcher2,
	runmatcher3.



% Schema designator (obj in skos:inScheme)
voc(asfa,'http://ontologies.freshwaterlife.org/asfa/skosScheme').
voc(gemet,'http://www.eionet.eu.int/gemet/').
voc(agrovoc,'http://www.fao.org/aims/aos/agrovoc').



% rdf assert the skos:inScheme info
prepare:-
	voc(agrovoc,AgrovocScheme),
	rdf_transaction(
	forall(
	       (
	       rdf(A, rdf:type, skos:'Concept'),
	       not(rdf(A, skos:inScheme,_))
	       ),
	       rdf_assert(A, skos:inScheme, AgrovocScheme))
		       ).



% --------------------
% match gemet to asfa
% --------------------
runmatcher1:-
	Graph = gemetasfa1,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(gemet, Gemet),
	voc(asfa, Asfa),

		Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Gemet),
		   ontology2(Asfa),
		   candidate_matchers([labelmatchEN])
			  ],
	find_candidates1(Options).

find_candidates1(Options) :-
	voc(asfa,Asfa),
	voc(gemet,Gemet),

	debug(align, 'Finding source concepts.', []),
	findall(SourceConcept,
		(
		rdf(SourceConcept, skos:inScheme,Gemet),
		rdfs_individual_of(SourceConcept,skos:'Concept')
		),
		SourceConcepts
	       ),
	debug(align, 'Finding candidates mappings from scheme ~p~n', [gemet]),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
						    Asfa,
						    Options))
	      ).

% --------------------
% match agrovoc to asfa
% --------------------
runmatcher2:-
	Graph = agrovocasfa1,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(agrovoc, Agrovoc),
	voc(asfa, Asfa),

		Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Agrovoc),
		   ontology2(Asfa),
		   candidate_matchers([labelmatchEN])
			  ],
	find_candidates2(Options).

find_candidates2(Options) :-
	voc(asfa,Asfa),
        voc(agrovoc, Agrovoc),

	debug(align, 'Finding source concepts.', []),
	findall(SourceConcept,
		(
		rdf(SourceConcept, skos:inScheme,Agrovoc),
		rdfs_individual_of(SourceConcept,skos:'Concept')
		),
		SourceConcepts
	       ),
	debug(align, 'Finding candidates mappings from scheme ~p~n', [gemet]),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
						    Asfa,
						    Options))
	      ).



% --------------------
% match agrovoc to gemet
% --------------------
runmatcher3:-
	Graph = agrovocgemet1,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(agrovoc, Agrovoc),
	voc(gemet, Gemet),

		Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Agrovoc),
		   ontology2(Gemet),
		   candidate_matchers([labelmatchEN])
			  ],
	find_candidates3(Options).

find_candidates3(Options) :-
        voc(agrovoc, Agrovoc),
	voc(gemet, Gemet),

	debug(align, 'Finding source concepts.', []),
	findall(SourceConcept,
		(
		rdf(SourceConcept, skos:inScheme,Agrovoc),
		rdfs_individual_of(SourceConcept,skos:'Concept')
		),
		SourceConcepts
	       ),
	debug(align, 'Finding candidates mappings from scheme ~p~n', [gemet]),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
						    Gemet,
						    Options))
	      ).
