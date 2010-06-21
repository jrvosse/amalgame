%
% Set of convenience predicates to generate mappings in the EDOAL
% format.
%
% EDOAL: Expressive and Declarative Ontology Alignment Language
% http://alignapi.gforge.inria.fr/edoal.html

:-module(edoal, [
		 assert_match/3
		]
	).
:- use_module(library(semweb/rdf_db)).

%%	assert_match(+Entity1, +Entity2, +OptionList) is det.
%
%	Creates an EDOAL map cel defining a mapping
%       between Entity 1 and 2.
%       OptionList must contain the alignment(A) this cell belongs to.
%	Optionally, it can also define:
%	- graph(G) the named graph of the cell (G defaults to 'align')
%	- measure(M) the confidence level (M defaults to 1.0).
%	= relation(R) the type of relation (R defaults to '=')

assert_match(C1, C2, Options) :-
        option(alignment(A), Options),
        option(graph(Graph), Options, align),
	option(measure(M), 1.0),
	option(relation(R), '='),
        rdf_bnode(Cell),
        rdf_assert(A, align:map, Cell),
        rdf_assert(Cell, rdf:type, align:'Cell', Graph),
        rdf_assert(Cell, align:entity1, C1, Graph),
        rdf_assert(Cell, align:entity2, C2, Graph),
        rdf_assert(Cell, align:measure, literal(M), Graph),
        rdf_assert(Cell, align:relation, literal(R), Graph).
