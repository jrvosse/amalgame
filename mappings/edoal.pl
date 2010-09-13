:-module(edoal, [
		 assert_alignment/2, 	% +URI, +OptionList
		 assert_cell/3,	        % +E1, +E2, +OptionList
		 edoal_to_skos/3        % +EdoalGraph, +Options, +SkosGraph
		]
	).


/** <module> Generate EDOAL

Set of convenience predicates to generate mappings in the EDOAL format.

EDOAL: Expressive and Declarative Ontology Alignment Language
http://alignapi.gforge.inria.fr/edoal.html

*/


:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(amalgame(namespaces)).
:- use_module(map).

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

%%	assert_cell(+C1,+C2,+OptionList) is det.
%
%	Asserts a correspondence between C1 and C2.
%
%       OptionList must contain the
%       - alignment(A) this cell belongs to.
%
%       It may also define:
%	- graph(G), named graph to assert to, defaults to 'align'
%
%	Creates an EDOAL map cel defining a mapping
%       between Entity 1 and 2.
%	Options include:
%	- measure(M) the confidence level (M defaults to 0.00001).
%	- relation(R) the type of relation (R defaults to skos:closeMatch)
%	- method(M) method used if different from method set for
%	- aligmnent A (default: none)

assert_cell(C1, C2, Options) :-
	rdf_equal(skos:closeMatch, CloseMatch),
        option(graph(Graph), Options, align),
	option(measure(M),   Options, 0.00001),
	option(relation(R),  Options, CloseMatch),
	rdf_bnode(Cell),
	rdf_assert(Cell, rdf:type, align:'Cell', Graph),
	rdf_assert(Cell, align:entity1, C1, Graph),
	rdf_assert(Cell, align:entity2, C2, Graph),
	rdf_assert(Cell, align:measure, literal(M), Graph),
	% Relation should be a literal according to the specs, but we do not like this ...
	% rdf_assert(Cell, align:relation, literal(R), Graph),
	rdf_assert(Cell, align:relation, R, Graph),
	% FIXME. Jan, asserting this triple slows things down dramatically
	%(   option(alignment(A), Options)
	%->  rdf_assert(A, align:map, Cell, Graph)
	%;   debug(edoal, 'Warning: asserting EDOAL cell without parent alignment', [])
	%).
	(   option(method(Method), Options)
	->  term_to_atom(Method, MethodAtom),
	    rdf_assert(Cell, amalgame:method, literal(MethodAtom), Graph)
	;   true
	).

%%	edoal_to_skos(+EdoalGraph, +SkosGraph, +Options) is det.
%
%	Convert mappings in EdoalGraph to SKOS mapping relations in SkosGraph.

edoal_to_skos(EdoalGraph, SkosGraph, Options) :-
	rdf_transaction(
			forall(has_map([C1, C2], edoal, MatchOptions, EdoalGraph),
			       assert_as_skos(C1-C2-MatchOptions, Options, SkosGraph)
			      )
		       ).


assert_as_skos(C1-C2-MatchOptions, Options, SkosGraph) :-
	(   memberchk(relation(R), MatchOptions),
	    rdfs_subproperty_of(R, skos:mappingRelation)
	->  true
	;   memberchk(relation(R), Options),
	    rdfs_subproperty_of(R, skos:mappingRelation)
	->  true
	;   rdf_equal(skos:closeMatch, R)
	),
	rdf_assert(C1, R, C2, SkosGraph).
