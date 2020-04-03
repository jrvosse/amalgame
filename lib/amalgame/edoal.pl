:-module(edoal, [
		 assert_alignment/2,	% +URI, +OptionList
		 assert_cell/3,	        % +E1, +E2, +OptionList
		 edoal_to_triples/3,	% +Request, +EdoalGraph, +Options, +TargetGraph
		 inline_evidence_graphs/2   % hack
		]
	).


/** <module> Generate EDOAL

Set of convenience predicates to generate mappings in the EDOAL format.

EDOAL: Expressive and Declarative Ontology Alignment Language
http://alignapi.gforge.inria.fr/edoal.html

*/


:- use_module(library(semweb/rdf11)).
:- use_module(ag_provenance).

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
        rdf_assert(URI, align:method, Method^^xsd:string, Graph),
	rdf_assert(URI, align:type,     Type^^xsd:string, Graph),

	rdf_assert(O1, align:location, L1^^xsd:string, Graph),
	rdf_assert(O2, align:location, L2^^xsd:string, Graph).


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
%	- method(M) method used (literal)
%	- match(M)  matching value [0..1]
%	- alignment(A) (default: none)
%	- comment(C) (C default: none)
%

assert_cell(C1, C2, Options) :-
	option(graph(Graph), Options, align),
	rdf_bnode(Cell),
	rdf_assert(Cell, rdf:type, align:'Cell', Graph),
	(var(C1) -> true; rdf_assert(Cell, align:entity1, C1, Graph)),
	(var(C2) -> true; rdf_assert(Cell, align:entity2, C2, Graph)),

	(   option(conform(strict), Options)
	->  rdf_equal(skos:closeMatch, CloseMatch),
	    option(measure(M),   Options, 0.00001),
	    option(relation(R),  Options, CloseMatch),
	    rdf_assert(Cell, align:measure,  M^^xsd:float, Graph),
	    rdf_assert(Cell, align:relation, R^^xsd:string, Graph)
	;   (   option(measure(M), Options)
	    ->	rdf_assert(Cell, align:measure, M^^xsd:float, Graph)
	    ;	true
	    ),
	    (	option(relation(R), Options)
	    ->	rdf_assert(Cell, align:relation, R, Graph)
	    ;	true
	    )
	),

	(   option(alignment(A), Options)
	->  rdf_assert(A, align:map, Cell, Graph)
	;   rdf_assert(Graph, align:map, Cell, Graph)
	),

	(   option(prov(Prov), Options)
	->  assert_provlist(Prov, Cell, Graph, Options)
	;   true
	),
	(   option(method(MethodList), Options)
	->  assert_methodlist(MethodList, Cell, Graph)
	;   true
	).


assert_methodlist([], _, _).
assert_methodlist([M|MethodList], Cell, Graph) :-
	rdf_assert(Cell, amalgame:method, M, Graph),
	assert_methodlist(MethodList, Cell, Graph).


assert_provlist([], _, _,_).
assert_provlist([P|ProvList], Cell, Graph, Options) :-
	memberchk(method(direct), P), !,
	assert_provlist(ProvList, Cell, Graph, Options).
assert_provlist([[]|ProvList], Cell, Graph, Options) :-
	assert_provlist(ProvList, Cell, Graph, Options).

assert_provlist([P|ProvList], Cell, Graph, Options) :-
	rdf_bnode(B),
	rdf_assert(Cell, amalgame:evidence, B, Graph),
	forall(member(ProvElem, P),
	       (   ProvElem =.. [Key, Value],
		   assert_prov_elem(Key, Value, B, Graph, Options)
	       )
	      ),
	assert_provlist(ProvList, Cell, Graph, Options).

assert_prov_elem(graph, ValueGraph, Subject, TargetGraph, Options) :-
	!,
	(   option(evidence_graphs(enabled), Options)
	->  atom_concat(Subject, '_evidence_graph', EG),
	    rdf_assert(Subject, amalgame:evidenceGraph, EG, TargetGraph),
	    rdf_assert_triples(ValueGraph, EG)
	;   true
	).
assert_prov_elem(relation, Relation, Subject, Graph, _Options) :-
	rdf_assert(Subject, align:relation, Relation, Graph).
assert_prov_elem(user, User, Subject, Graph, _Options) :-
	rdf_assert(Subject, amalgame:user, User, Graph).

assert_prov_elem(Key, Value, Subject, Graph, _Options) :-
	rdf_global_id(amalgame:Key, Property),
	format(atom(Atom), '~w', [Value]),
	rdf_assert(Subject, Property, Atom^^xsd:string, Graph).

rdf_assert_triples([], _).
rdf_assert_triples([rdf(S,P,O)|Tail], Graph) :-
	rdf_assert(S,P,O,Graph),
	rdf_assert_triples(Tail, Graph).

%%	edoal_to_triples(+EdoalGraph, +SkosGraph, +Options) is
%%	det.
%
%	Convert mappings in EdoalGraph to some triple-based format using
%	simple mapping relations such as defined by as SKOS, owl:sameAs
%	or dc:replaces.
%
%	Options:
%	* request(Request): http request to be used in provenance data
%	* relation(URI): relation to be used, defaults to
%	skos:closeMatch
%	* min(Measure): minimal confidence level, defaults to 0.0.
%	* max(Measure): max confidence level, default to 1.0

edoal_to_triples(EdoalGraph, TargetGraph, Options) :-
	rdf_assert(TargetGraph, rdf:type, amalgame:'ExportedAlignment', TargetGraph),
	rdf_transaction(
			forall(has_correspondence(align(C1,C2,MatchOptions), EdoalGraph),
			       assert_as_single_triple(align(C1,C2,MatchOptions), Options, TargetGraph)
			      )
		       ),
	rdf_bnode(Process),
	prov_was_generated_by(Process, TargetGraph, TargetGraph,
			     [was_derived_from([EdoalGraph])
			     |Options]).

assert_as_single_triple(align(C1,C2,MatchOptions), Options, TargetGraph) :-
	rdf_equal(skos:closeMatch, DefaultRelationIfNoneGiven),
	option(default_relation(DefaultRelation), Options, DefaultRelationIfNoneGiven),
	append(MatchOptions, MatchOptionsFlat),
	append([Options, MatchOptionsFlat], AllOptions),
	option(relation(R), AllOptions, DefaultRelation),
	(   R == none
	->  Pred = DefaultRelationIfNoneGiven
	;   Pred = R
	),
	rdf_assert(C1, Pred, C2, TargetGraph).

inline_evidence_graphs([], []).
inline_evidence_graphs([In|TailIn], [Out|TailOut]) :-
	inline_evidence_graph(In, Out),
	inline_evidence_graphs(TailIn, TailOut).

inline_evidence_graph(In, [graph(Evidence)|Rest]) :-
	select(evidenceGraph(G), In, Rest),!,
	findall(rdf(S,P,O), rdf(S,P,O,G), Evidence).
inline_evidence_graph(In, In) :- !.
