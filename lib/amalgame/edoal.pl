:-module(edoal, [
		 assert_alignment/2, 	% +URI, +OptionList
		 assert_cell/3,	        % +E1, +E2, +OptionList
		 edoal_to_triples/4,	% +Request, +EdoalGraph, +Options, +TargetGraph
		 edoal_select/5	        % +Request, +EdoalGraph, +Options, +TargetGraph, +TargetRestGraph
		]
	).


/** <module> Generate EDOAL

Set of convenience predicates to generate mappings in the EDOAL format.

EDOAL: Expressive and Declarative Ontology Alignment Language
http://alignapi.gforge.inria.fr/edoal.html

*/

:- use_module(library(http/http_host)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(user(user_db)).

:- use_module(map).
:- use_module(opm).

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

	(   option(alignment(A), Options)
	->  rdf_assert(A, align:map, Cell, Graph), gtrace
	;   rdf_assert(Graph, align:map, Cell, Graph)
	),
	(   option(source(Source), Options)
	->  term_to_atom(Source, SourceAtom),
	    rdf_assert(Cell, amalgame:source, literal(SourceAtom), Graph)
	;   true
	),
	(   option(method(Methods), Options)
	->  (   is_list(Methods)
	    ->	forall(member(Method, Methods),
		       (   term_to_atom(Method, MethodAtom),
			   rdf_assert(Cell, amalgame:method, literal(MethodAtom), Graph)
		       ))
	    ;	term_to_atom(Methods, MethodAtom),
		rdf_assert(Cell, amalgame:method, literal(MethodAtom), Graph)
	    )
	;   true
	),
	(   option(match(Matches), Options)
	->  (   is_list(Matches)
	    ->	forall(member(Match, Matches),
		       (   term_to_atom(Match, MatchAtom),
			   rdf_assert(Cell, amalgame:match, literal(MatchAtom), Graph)
		       ))
	    ;	term_to_atom(Matches, MatchAtom),
		rdf_assert(Cell, amalgame:match, literal(MatchAtom), Graph)
	    )
	;   true
	),
	(   option(prov(Prov), Options)
	->  assign_prov(Cell, Prov, Options)
	;   true
	)
	.
assign_prov(_, [],_).
assign_prov(Cell, [Prov|Tail], Options):-
	option(graph(Graph), Options, align),
	rdf_bnode(Provenance),
	rdf_assert(Cell, amalgame:provenance, Provenance, Graph),
	% get_time(T), format_time(atom(Time), '%Y-%m-%dT%H-%M-%S%Oz', T),
	% option(evaluator(Evaluator), Prov, 'anonymous'),
	% rdf_assert(Provenance, rdf:type, amalgame:'Provenance', Graph),
	% rdf_assert(Provenance, dcterms:creator, literal(Evaluator), Graph),
	% rdf_assert(Provenance, dcterms:date, literal(Time), Graph),
	(   option(method(Methods), Prov)
	->  (   is_list(Methods)
	    ->	forall(member(Method, Methods),
		       (   term_to_atom(Method, MethodAtom),
			   rdf_assert(Provenance, amalgame:method, literal(MethodAtom), Graph)
		       ))
	    ;	term_to_atom(Methods, MethodAtom),
		rdf_assert(Provenance, amalgame:method, literal(MethodAtom), Graph)
	    )
	;   true
	),
	(   option(match(Matches), Prov)
	->  (   is_list(Matches)
	    ->	forall(member(Match, Matches),
		       (   term_to_atom(Match, MatchAtom),
			   rdf_assert(Provenance, amalgame:match, literal(MatchAtom), Graph)
		       ))
	    ;	term_to_atom(Matches, MatchAtom),
		rdf_assert(Provenance, amalgame:match, literal(MatchAtom), Graph)
	    )
	;   true
	),
	(   option(graph(PGraph), Prov)
	->  forall(member(rdf(_S,P,O), PGraph),
		   rdf_assert(Provenance, P, O, Graph))
	;   true
	),
	(	option(comment(Comment), Prov), Comment \= ''
	->	rdf_assert(Provenance, rdfs:comment, literal(Comment), Graph)
	;	true
	),
	(	option(relation(OriginalRelation), Prov)
	->	rdf_assert(Provenance, amalgame:relation, OriginalRelation, Graph)
	;	true
	),
	assign_prov(Cell, Tail, Options).

%%	edoal_to_triples(+Request, +EdoalGraph, +SkosGraph, +Options) is
%%	det.
%
%	Convert mappings in EdoalGraph to some triple-based format using
%	simple mapping relations such as defined by as SKOS, owl:sameAs
%	or dc:replaces.
%
%	Options:
%	* relation(URI): relation to be used, defaults to
%	skos:closeMatch
%	* min(Measure): minimal confidence level, defaults to 0.0.
%	* max(Measure): max confidence level, default to 1.0

edoal_to_triples(Request, EdoalGraph, TargetGraph, Options) :-
	rdf_assert(TargetGraph, rdf:type, amalgame:'ExportedAlignment', TargetGraph),
	rdf_transaction(
			forall(has_map([C1, C2], edoal, MatchOptions, EdoalGraph),
			       assert_as_single_triple(C1-C2-MatchOptions, Options, TargetGraph)
			      )
		       ),
	rdf_bnode(Process),
	opm_was_generated_by(Process, TargetGraph, TargetGraph,
			     [was_derived_from([EdoalGraph]),
			     request(Request)]).

assert_as_single_triple(C1-C2-MatchOptions, Options, TargetGraph) :-
	rdf_equal(skos:closeMatch, DefaultRelation),
	append([
		Options,
		MatchOptions,
		[DefaultRelation]
	       ],
	       AllOptions),
	member(relation(R), AllOptions),
	R \= no_override, !,
	rdf_assert(C1, R, C2, TargetGraph).

edoal_select(Request, EdoalGraph, TargetGraph, TargetRestGraph, Options) :-
	option(min(Min), Options, 0.0),
	option(max(Max), Options, 1.0),
	rdf_transaction(
			forall((has_map([C1, C2], edoal, MatchOptions, EdoalGraph),
				memberchk(measure(Measure), MatchOptions)
			       ),
			       (   (Measure < Min ; Measure > Max)
			       ->  append(Options, MatchOptions, NewOptions),
				   assert_cell(C1, C2, [graph(TargetRestGraph),
							alignment(TargetRestGraph)
						       |NewOptions])
			       ;   append(Options, MatchOptions, NewOptions),
				   assert_cell(C1, C2, [graph(TargetGraph),
							alignment(TargetGraph)
						       |NewOptions])
			       )
			      )
		       ),
	rdf_assert(TargetGraph, rdf:type, amalgame:'SelectionAlignment', TargetGraph),
	rdf_assert(TargetRestGraph, rdf:type, amalgame:'SelectionAlignment', TargetRestGraph),
	rdf_bnode(Process),
	rdf_assert(Process, amalgame:minimalConfidence, literal(type(xsd:float, Min)), TargetGraph),
	rdf_assert(Process, amalgame:maximalConfidence, literal(type(xsd:float, Max)), TargetGraph),
	opm_was_generated_by(Process, [TargetGraph, TargetRestGraph], TargetGraph,
			     [was_derived_from([EdoalGraph]),
			      request(Request)
			     ]).
