% Example alignment, aligning wn30 to wn20 using amalgame

:- module(wnalign,
	 [
	  run/0,
	  prun/0
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(amalgame(matchers/skosmatcher)).
:- use_module(amalgame(rankers/skosranker)).
:- use_module(amalgame(edoal/edoal)).

wn20schema('http://www.w3.org/2006/03/wn/wn20/schema/').
wn20('http://www.w3.org/2006/03/wn/wn20/').
wn30('http://purl.org/vocabularies/princeton/wn30/').
glossmatches(F) :- wn30(WN30), atom_concat(WN30,'glossmatches-m.ttl', F).

prun:- profile(run).

run:-
	Graph=wn3020,
	rdf_persistency(Graph, false),
	rdf_retractall(_,_,_,Graph),
	rdf_retractall(_,_,_,align),
	wn30(WN30),
	wn20(WN20),

	Options = [
		   graph(Graph),
		   alignment(Graph),
		   ontology1(WN30),
		   ontology2(WN20)
		  ],

	% Step 1: prepare data sets
	prepare_data(WN30),
	prepare_data(WN20),

	% Step 2: generate candidates with cheap (exact labelmatch) justifications
	find_candidates(Options),

	% Step 3: split by marking 1-1 glossmatches as 'done'
	% label_unique_glossmatches(Options),

	% Step 4: find more expansive justifications for those not 'done' yet
	% find_hier_justifications(Options),

	% Final step: Save results as an EDAOL alignment
	%assert_alignment(Graph, [ method('amalgame hybrid') | Options ]),
	%rdf_save('wn2030.rdf', [graph(wn3020)]),
	true.

find_candidates(Options) :-
	wn20(WN20),
	debug(align, 'Finding source concepts.', []),
	findall(SourceConcept,
		rdfs_individual_of(SourceConcept, wn30:'NounSynset'),
		SourceConcepts
	       ),
	debug(align, 'Finding candidates mappings from scheme ~p~n', [wn20]),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
						    WN20,
						    [candidate_matchers([labelmatch])|Options]))
	      ).



find_hier_justifications(Options) :-
	wn20(WN20),
	rdf_transaction(
			forall(not_done(C),
			       justify_candidates(C,WN20,
						  [justifiers([hiermatch/broad, hiermatch/narrow])
						   |Options])
			      )
		       ).

label_unique_glossmatches(Options) :-
	debug(align, 'label unique qloss matches', []),
	option(graph(Graph), Options),
	rdf_transaction(
			forall((   rdfs_individual_of(Cell, align:'Cell'),
				   rdf_has(Cell, amalgame:justification, literal('exact match: wn30:hasGloss:1/wn20schema:gloss:1'))
			       ),
			       rdf_assert(Cell, amalgame:status, amalgame:done, Graph)
			      )
		       ).
not_done(Concept) :-
	rdfs_individual_of(Cell, align:'Cell'),
	\+ rdf(Cell, amalgame:status, amalgame:done),
	rdf(Cell, align:entity1, Concept).

:- dynamic
	prepare_cache/1.

clean_start:-
	retractall(prepare_cache(_)).

prepare_data(Scheme) :-
	prepare_cache(done(Scheme)),
	!.


prepare_data(Scheme) :-
	wn20(Scheme),
	remove_rdfs_label(Scheme),
	fix_gloss,
	assert(prepare_cache(done(Scheme))).

prepare_data(Scheme) :-
	wn30(Scheme),
	remove_rdfs_label(Scheme),
	assert(prepare_cache(done(Scheme))).

remove_rdfs_label(Scheme) :-
	rdf_transaction(
			forall(rdf(S, skos:inScheme, Scheme),
			       rdf_retractall(S, rdfs:label, _)
			      )
		       ).
%%	fix_gloss is det.
%
%	Removes the spurious round brackets around the wn20
%	gloss literals.

fix_gloss:-
	rdf_transaction(
			forall((   rdf(S, wn20schema:gloss, literal(lang(Lang, Gloss))),
				   sub_atom(Gloss,0,1,_,'(') % Just check if we have not done so already ...
			       )
			       ,
			       (   sub_atom(Gloss,1,_,1,NewGloss),
				   rdf_retractall(S, wn20schema:gloss, literal(lang(Lang, Gloss))),
				   rdf_assert(S, wn20schema:gloss, literal(lang(Lang, NewGloss)), fixgloss)
			       )
			      )
		       ).



