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
run:-   writeln('Running matcher 1'),
	runmatcher1,
        writeln('Running matcher 2'),
	runmatcher2,
	writeln('Running matcher 3 (English)'),
	runmatcher3EN,
	writeln('Running matcher 3 (Spanish)'),
	runmatcher3ES,
	writeln('Asserting Carmen\'s manual mappings'),
	assert_carmen_mappings.

clean:-
	rdf_retractall(_,_,_,gemetasfa1),
	rdf_retractall(_,_,_,agrovocasfa1),
	rdf_retractall(_,_,_,agrovocgemetEN),
        rdf_retractall(_,_,_,agrovocgemetES),
	rdf_retractall(_,_,_,carmenmanualmapping),
	writeln('All Carmen alignments cleared.').



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
		   language(en),
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
		    language(en),

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
% Using English labels
runmatcher3EN:-
	Graph = agrovocgemetEN,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(agrovoc, Agrovoc),
	voc(gemet, Gemet),

		Options = [
			   language(en),
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Agrovoc),
		   ontology2(Gemet),
		   candidate_matchers([labelmatchEN])
			  ],
	find_candidates3(Options).



% Using Spanish labels
runmatcher3ES:-
	Graph = agrovocgemetES,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(agrovoc, Agrovoc),
	voc(gemet, Gemet),

		Options = [
			   language(es),
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




% asserts the mappings Carmen made between Agrovoc and Gemet
% find skos concepts which have these labels, then add a cell
%
assert_carmen_mappings:-
	findall([A,B],mmcarmen(_,A,_,B),List),
	assert_term_mapping(List).

assert_term_mapping([]).
assert_term_mapping([[AgrCode,Gem]|L]):-
	voc(agrovoc,AgScheme),
	voc(gemet,GemScheme),
	atomic_list_concat(['http://www.fao.org/aims/aos/agrovoc#',AgrCode],SourceConcept),
		findall( U2,
		 (rdf(U2, skos:prefLabel, literal(lang(en,Gem))),
		  rdf(U2,skos:inScheme,GemScheme)
		 ),[TargetConcept]
	       ),

	CellOptions = [measure(1), % manual match, this is just a candidate
		       method(manualmapping),
		       graph(carmenmanualmapping),
		       alignment(carmenmanualmapping),
		       ontology1(AgScheme),
		       ontology2(GemScheme),
		       candidate_matchers([manualmatcher])
		      ],
       	write('o'),
	assert_cell(SourceConcept, TargetConcept, CellOptions),
        assert_term_mapping(L).
assert_term_mapping([[_Agr,_Gem]|L]):-
	write('x'),
        assert_term_mapping(L).


mmcarmen('NT', '15649','Mariculture','mariculture').
mmcarmen('NT', '7034','Shellfish culture','shellfish farming').
mmcarmen('RT', '36678','Fish farms','fish farming').
mmcarmen('BT', '4318','life sciences','life science').
mmcarmen('NT', '49890','anatomy','anatomy').
mmcarmen('NT', '4800','Microbiology','microbiology').
mmcarmen('NT', '3718','Hydrobiology','hydrobiology').
mmcarmen('NT', '5578','Parasitology','parasitology').
mmcarmen('NT', '49903','Morphology','morphology').
mmcarmen('NT', '49881','Physiology','physiology').
mmcarmen('NT', '7571','Synecology','synecology').
mmcarmen('NT', '5830','Physical oceanography','physical oceanography').
mmcarmen('BT', '9001017','resources','resource').
mmcarmen('NT', '92341','Biological resources','biological resource').
mmcarmen('RT', '16141','Nature reserves','nature protection').
mmcarmen('NT', '28012','Marine resources','sea resource').
mmcarmen('RT', '24134','Telemetry','telemetry').
mmcarmen('RT', '1665','climate','climate').
mmcarmen('BT', '925','biology','biology').
mmcarmen('BT', '2593','environment','ENVIRONMENT (natural environment, anthropic environment)').
% mmcarmen('RT', '8309','water','water').
