%
% Set of convenience predicates to generate mappings in the EDOAL
% format.
%
% EDOAL: Expressive and Declarative Ontology Alignment Language
% http://alignapi.gforge.inria.fr/edoal.html

:-module(edoal, [
		 assert_alignment/2, 	% +URI, +OptionList
		 assert_cell/3,		% +E1, +E2, +OptionList
		 create_cell/4 		% +E1, +E2, +OptionList, -Cell
		]
	).

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(align, 'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').

%%	assert_alignment(+URI, +OptionList) is det.
%
%	Create and assert an alignment with uri URI.
%	OptionList must contain:
%	- method(String),
%	- ontology1(URL),
%	- ontology2(URL).
%
%	OptionList may specify:
%	- location1(String) defaults to value ontology1
%	- location2(String) defaults to value ontology2
%	- graph(G) defaults to 'align'
%	- type(T) default to '**'

assert_alignment(URI, Options) :-
	option(method(Method), Options),
	option(ontology1(O1),  Options),
	option(ontology2(O2),  Options),
	option(location1(L1),  Options, O1),
	option(location2(L2),  Options, O2),
	option(graph(Graph),   Options, align),
	option(type(Type),     Options, '**'),

	rdf_assert(O1,  rdf:type, align:'Ontology', Graph),
        rdf_assert(O2,  rdf:type, align:'Ontology', Graph),
	rdf_assert(URI, rdf:type, align:'Alignment', Graph),

	rdf_assert(URI, align:onto1, O1, Graph),
        rdf_assert(URI, align:onto2, O2, Graph),
        rdf_assert(URI, align:method, literal(Method), Graph),
	rdf_assert(URI, align:type, literal(Type), Graph),

	rdf_assert(O1, align:location, literal(L1), Graph),
	rdf_assert(O2, align:location, literal(L2), Graph),

	true.


%%	create_cell(+Entity1, +Entity2, +OptionList, -Cell) is det.
%
%	Creates an EDOAL map cel defining a mapping
%       between Entity 1 and 2.
%	Options include:
%	- measure(M) the confidence level (M defaults to 0.00001).
%	- relation(R) the type of relation (R defaults to '=')
%	- method(M) method used if different from method set for
%	- aligmnent A (default: none)

create_cell(C1, C2, Options, TripleList) :-
	option(measure(M),   Options, 0.00001),
	option(relation(R),  Options, '='),
        rdf_bnode(Cell),
        L0 = [
	     rdf(Cell, rdf:type, align:'Cell'),
	     rdf(Cell, align:entity1, C1),
	     rdf(Cell, align:entity2, C2),
	     rdf(Cell, align:measure, literal(M)),
	     rdf(Cell, align:relation, literal(R))
	    ],
	(   option(alignment(A), Options)
	->  append(L0, [rdf(A, align:map, Cell)], L1)
	;   L0 = L1
	),
	(   option(method(Method), Options)
	->  append(L1, [rdf(Cell, align:method, literal(Method))], TripleList)
	;   TripleList = L1
	).

%%	assert_cell(+C1,+C2,+OptionList) is det.
%
%	Creates and asserts a possibly empty
%	correspondence between C1 and C2.
%
%       OptionList must contain the -
%	alignment(A) this cell belongs to. It may also define: -
%	graph(G), named graph to assert to, defaults to 'align'

assert_cell(C1, C2, Options) :-
        option(graph(Graph), Options, align),
	create_cell(C1, C2, Options, TripleList),
	rdf_assert_list(TripleList, Graph).

rdf_assert_list([], _).
rdf_assert_list([rdf(S,P,O)|Tail], Graph) :-
	rdf_assert(S,P,O,Graph),
	rdf_assert_list(Tail, Graph).









