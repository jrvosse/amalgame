% New version of justifier
% Input: EDOAL Graph to be Justified/Filtered
% Parameters: Comparison Graph (if _, take all graphs)
%             Output Graph
%             Justifier type
% Output: New, filtered, altered Graph
%

:- module(skosjustifier,
	  [
	   justify_candidates/2
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(amalgame(namespaces)).

match_type(propmatch/def,  P):- rdf_equal(skos:definition, P).
match_type(propmatch/pref, P):- rdf_equal(skos:prefLabel, P).
match_type(propmatch/alt,  P):- rdf_equal(skos:altLabel, P).

match_type(hiermatch/broad,  P) :- rdf_equal(skos:broader, P).
match_type(hiermatch/narrow, P) :- rdf_equal(skos:narrow, P).

:- rdf_meta
	rel_type(+,r).

rel_type(skosClose, skos:closeMatch).
rel_type(skosExact, skos:exactMatch).
rel_type(edoalIs, literal(=)).


% test top pred.
justify1(Graph):-
	justify_candidates(Graph, [justifiers([hiermatch/broad]),relationtype([skosClose,skosExact]),outputGraph(justoutput2),secondaryGraph(Graph)]).


%%	 justify_candidates(+Graph, +Options) is det (i hope).
%
%	This takes as input the input graph for which justifications are
%	to be identified. Options include
%	    - justifiers(List), to select different justifiers,
%	    - relationtype(List), to select different type of relations
%	    for a hierarchical evidence
%	    - outputGraph (Graph), where the output is written to
%	    - secondaryGraph (Graph), where evidence may be gathered
%	    from (can be any graph, including input graph). TODO: make
%	    default 'all graphs'

justify_candidates(Graph, Options):-
	findall(CandCell,
		rdf(CandCell, rdf:type, align:'Cell',Graph),
		CCList),
	rdf_transaction(justifyall(CCList,Options,Graph)).

% For iteration purposes, can be replaced later by a find/forall higher
% up

justifyall([],_,_Graph).
justifyall([CandCell|CCList],Options,Graph):-
	justification(CandCell,Options,Graph),
	justifyall(CCList,Options,Graph).


% Justify /3: +CandCell, +Options, +Graph
% filter/rerank/justify one edoal cell based on justifier in Options



%%	justification(+Cell, +Option, +Graph) is nondet.
%
%	Finds all justifications of one or more types for the mapping
%	described in Cell and asserts a new cell based on that in the
%	provided OutputGraph
%
justification(CandCell, Options, Graph) :-


	option(justifiers(Justifiers), Options),
	member(hiermatch/MatchType, Justifiers), % use hierarchical matcher
       	option(relationtype(RelationTypes), Options),
	rdf(CandCell, align:entity1, E1, Graph),
	rdf(CandCell, align:entity2, E2, Graph),

	findall(Shared1:Shared2:MyRelType,
		(
		    match_type(hiermatch/MatchType, Property),
		    rdf_has(E1, Property, Shared1, _RealProp1),
		    rdf_has(E2, Property, Shared2, _RealProp2),
		    rdf(Cell, align:entity1, Shared1),
		    rdf(Cell, align:entity2, Shared2),
		    rdf(Cell, align:relation, RelType),
		    member(MyRelType, RelationTypes),
		    rel_type(MyRelType,RelType)
		),
		JustificationList), % Get all justifications

	option(outputGraph(OutputGraph), Options),
	length(JustificationList,JLLen),
	JLLen > 0, % If one is found, then its ok

	format(atom(Method), 'found ~p hier matches', [JLLen]),
	rdf_has(E1, skos:inScheme, S1),
	rdf_has(E2, skos:inScheme, S2),
	rdf(CandCell, align:relation, CurRelType),

	rdf(Cell, align:measure, literal(M1)),
	M2 is M1 * JLLen, %hack
	CellOptions = [measure(M2),
		       method(Method),
		       graph(OutputGraph),
		       ontology1(S1),
		       ontology2(S2),
		       relation(CurRelType),
		       candidate_matchers([labelmatchLang])
		       ],


      	assert_cell(E1, E2, CellOptions)
	.


justification(_CandCell, _Options, _Graph).
