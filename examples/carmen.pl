% Example alignment, aligning aquatic Thesauri that are of interest to
% Carmen. Running this file will
%

:- module(carmen,
	 [
	  run/0,
	  prun/0,
	  load/0,
	  clean/0
	 ]).

%user:file_search_path(cliopatria, '../ClioPatria').

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(amalgame(matchers/skosmatcher)).
:- use_module(amalgame(mappings/edoal)).

:-

	writeln('\n\nCarmen commands: \n>load. \t loads all vocabularies (but not the skos/edoal graphs)'),
	writeln('>run. \t runs all matchers'),
	writeln('>prun. \t runs all matchers (with a profile)'),
	writeln('>clean. \t removes all alignment graphs\n').


load:-
	load_skos,
	load_gemet,
	load_asfa,
	load_agrovoc,
	prepare.

% Adjust these file locations to fit your settings

load_skos:-
	rdf_load('../ClioPatria/ontologies/base/rdfs.rdfs'),
	rdf_load('../ClioPatria/ontologies/base/skos.rdf'),
	rdf_load('../ClioPatria/ontologies/base/dcterms.rdf'),
	rdf_load('../ClioPatria/ontologies/base/owl.owl').

load_gemet:-
	rdf_load('../examples/carmenvocs/GEMET.skos.xml').
load_asfa:-
       	rdf_load('../examples/carmenvocs/ASFATh_Skos.xml').
load_agrovoc:-
       	rdf_load('../examples/carmenvocs/ag_skos_080422.rdf').


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



% --------------------
% Voc preprocessing and preparation
% --------------------

prepare:-
	prepare1,
	prepare2.

% rdf assert the skos:inScheme info for Agrovoc
prepare1:-
	voc(agrovoc,AgrovocScheme),
	rdf_transaction(
	forall(
	       (
	       rdf(A, rdf:type, skos:'Concept'),
	       not(rdf(A, skos:inScheme,_))
	       ),
	       rdf_assert(A, skos:inScheme, AgrovocScheme))
		       ).


% repair 'blank node as label value' problem with asfa

prepare2:- prepare2a,prepare2b.
prepare2a:-
	voc(asfa,AsfaScheme),
	rdf_transaction(
	forall(
	       (
	       rdf(A, rdf:type, skos:'Concept'),
	       rdf(A, skos:inScheme,AsfaScheme),
	       rdf(A, skos:prefLabel, BN1),
	       rdf(BN1, rdf:value, VAL)
	       ),
	       (
	       rdf_retractall(A, skos:prefLabel, BN1),
	       rdf_retractall(BN1, rdf:value, VAL),
	       rdf_assert(A, skos:prefLabel, VAL)
	       )
	      )
		       ).
prepare2b:-
	voc(asfa,AsfaScheme),
	rdf_transaction(
	forall(
	       (
	       rdf(A, rdf:type, skos:'Concept'),
	       rdf(A, skos:inScheme,AsfaScheme),
	       rdf(A, skos:prefLabel, BN1),
	       rdf(BN1, rdf:value, VAL)
	       ),
	       (
	       rdf_retractall(A, skos:altLabel, BN1),
	       rdf_retractall(BN1, rdf:value, VAL),
	       rdf_assert(A, skos:altLabel, VAL)
	       )
	      )
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
		   candidate_matchers([labelmatchLang])
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
		   candidate_matchers([labelmatchLang])
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
%

% Run language specific label matcher for all found
% languages in either skos:prefLabel or altLabel


runmatcher3ALL:-
	findall(
		LANG,
		(
		 voc(agrovoc,AG),
		 rdf(A,skos:inScheme,AG),
		 rdf(A,skos:prefLabel,literal(lang(LANG,_)))
		),L),
	sort(L,LangList),
       	write('Running language specific matcher for language:'),
	runmatcher3Langs(LangList).

% Run for one language
runmatcher3Langs([]).
runmatcher3Langs([CurLang|Tail]):-
	write(CurLang),write(', '),flush,
	atomic_list_concat([agrovocgemet,CurLang],Graph),
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	voc(agrovoc, Agrovoc),
	voc(gemet, Gemet),

		Options = [
			   language(CurLang),
		   graph(Graph),
		   alignment(Graph),
		   ontology1(Agrovoc),
		   ontology2(Gemet),
		   candidate_matchers([labelmatchLang])
			  ],
	find_candidates3(Options),
	runmatcher3Langs(Tail).


% Only using English labels
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
		   candidate_matchers([labelmatchLang])
			  ],
	find_candidates3(Options).



% Only using Spanish labels
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
		   candidate_matchers([labelmatchLang])
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
	debug(align, 'Finding alignments from Agrovoc to Gemet', []),
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
