:- module(alignmentcombiner,
	  [
	   combine_alignment/5 % +Graph1, +Graph2, +Type, +Options, +ResultGraph
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(amalgame(mappings/edoal).
:- use_module(amalgame(namespaces).


% Assert the intersection of two edoal graphs as a new graph
combine_alignment(Graph1, Graph2, union, _Options, ResultGraph):-
	atomic_list_concat([intersectionof, Graph1, Graph2],ResultGraph),
	rdf_transaction(
			common_alignment(Graph1, Graph2, ResultGraph)
		       ).


% Assert the intersection of two edoal cells as a new graph
common_alignment(Graph1, Graph2, ResultGraph):-
	rdf(Cell1, rdf:type, align:'Cell', Graph1),
	rdf(Cell2, rdf:type, align:'Cell', Graph2),
	rdf(Cell1, align:entity1, E1),
	rdf(Cell2, align:entity1, E1),
	rdf(Cell1, align:entity2, E2),
	rdf(Cell2, align:entity2, E2),

	rdf(Cell1, align:relation, Rel),
	rdf(Cell2, align:relation, Rel),

	rdf(Cell1, align:measure, literal(C1M)),
	rdf(Cell2, align:measure, literal(C2M)),
	NewMeas is (C1M + C2M) / 2,

	format(atom(Method), 'in disjunction of edoal graphs ~p-~p', [Graph1, Graph2]),

	CellOptions = [graph(ResultGraph),
		       measure(NewMeas),
		       method(Method),
		       relation(Rel)
		      		      ],
      	assert_cell(E1, E2, CellOptions),

	fail
	;
	true.




