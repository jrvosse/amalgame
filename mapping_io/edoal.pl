%
% Set of convenience predicates to generate mappings in the EDOAL
% format.
%
% EDOAL: Expressive and Declarative Ontology Alignment Language
% http://alignapi.gforge.inria.fr/edoal.html

:-module(edoal, [
		 create_alignment/2,
		 create_cell/3
		]
	).

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(align, 'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').

%%	create_alignment(+URI, +OptionList) is det.
%
%	Create an alignment with uri URI.
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

create_alignment(URI, Options) :-
	option(method(Method), Options),
	option(ontology1(O1), Options),
	option(ontology2(O2), Options),
	option(location1(L1), Options, O1),
	option(location2(L2), Options, O2),
	option(graph(Graph), Options, align),
	option(type(Type), Options, '**'),

	rdf_assert(O1,  rdf:type, align:'Ontology', Graph),
        rdf_assert(O2,  rdf:type, align:'Ontology', Graph),
	rdf_assert(URI, rdf:type, align:'Alignment', Graph),

	rdf_assert(URI, align:onto1, O1, Graph),
        rdf_assert(URI, align:onto2, O2, Graph),
        rdf_assert(URI, align:method, literal(Method)),
	rdf_assert(URI, align:type, literal(Type)),

	rdf_assert(O1, align:location, literal(L1)),
	rdf_assert(O2, align:location, literal(L2)),

	true.


%%	create_cell(+Entity1, +Entity2, +OptionList) is det.
%
%	Creates an EDOAL map cel defining a mapping
%       between Entity 1 and 2.
%       OptionList must contain the alignment(A) this cell belongs to.
%	Optionally, it can also define:
%	- graph(G) the named graph of the cell (G defaults to 'align')
%	- measure(M) the confidence level (M defaults to 1.0).
%	= relation(R) the type of relation (R defaults to '=')

create_cell(C1, C2, Options) :-
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
