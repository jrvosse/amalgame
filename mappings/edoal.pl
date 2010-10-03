:-module(edoal, [
		 assert_alignment/2, 	% +URI, +OptionList
		 assert_cell/3,	        % +E1, +E2, +OptionList
		 edoal_to_triples/4,	% +Request, +EdoalGraph, +Options, +TargetGraph
		 edoal_select/4	        % +Request, +EdoalGraph, +Options, +TargetGraph
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
:- use_module(library(version)).

:- use_module(user(user_db)).

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

	% Asserting this triple slows things down dramatically on Prolog < 5.11.6
	(   option(alignment(A), Options)
	->  rdf_assert(A, align:map, Cell, Graph)
	;   debug(edoal, 'Warning: asserting EDOAL cell without parent alignment', [])
	),
	(   option(source(Source), Options)
	->  term_to_atom(Source, SourceAtom),
	    rdf_assert(Cell, amalgame:source, literal(SourceAtom), Graph)
	;   true
	),
	(   option(method(Method), Options)
	->  term_to_atom(Method, MethodAtom),
	    rdf_assert(Cell, amalgame:method, literal(MethodAtom), Graph)
	;   true
	),
	(   option(prov(Prov), Options)
	->  true,
	    git_component_property('ClioPatria', version(CP_version)),
	    git_component_property('amalgame',   version(AG_version)),
	    format(atom(Version), 'Manually evaluated using Amalgame ~w/Cliopatria ~w', [AG_version, CP_version]),
	    get_time(T), format_time(atom(Time), '%a, %d %b %Y %H:%M:%S %z', T),
	    option(evaluator(Evaluator), Prov, 'anonymous'),


	    rdf_bnode(Provenance),
	    rdf_assert(Cell, amalgame:provenance, Provenance, Graph),
	    rdf_assert(Provenance, rdf:type, amalgame:'Provenance', Graph),
	    rdf_assert(Provenance, owl:versionInfo, Version, Graph),
	    rdf_assert(Provenance, dcterms:creator, literal(Evaluator), Graph),
	    rdf_assert(Provenance, dcterms:date, literal(Time), Graph),
	    (	option(comment(Comment), Prov), Comment \= ''
	    ->	rdf_assert(Provenance, rdfs:comment, literal(Comment), Graph)
	    ;	true
	    ),
	    (	option(relation(OriginalRelation), Prov)
	    ->	rdf_assert(Provenance, amalgame:relation, OriginalRelation, Graph)
	    ;	true
	    )
	;   true
	).

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
	rdf_transaction(
			forall(has_map([C1, C2], edoal, MatchOptions, EdoalGraph),
			       assert_as_single_triple(C1-C2-MatchOptions, Options, TargetGraph)
			      )
		       ),
	provenance_stamp(Request, EdoalGraph, TargetGraph, Provenance),
	rdf_assert(Provenance, dcterms:title, literal('Provenance: about this exported alignment'), TargetGraph),
	rdf_assert(TargetGraph, rdf:type, amalgame:'ExportedAlignment', TargetGraph).


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



edoal_select(Request, EdoalGraph, TargetGraph, Options) :-
	option(min(Min), Options, 0.0),
	option(max(Max), Options, 1.0),
	rdf_transaction(
			forall(has_map([C1, C2], edoal, MatchOptions, EdoalGraph),
			       (   	option(measure(Measure), MatchOptions, 1.0),
					(   (Measure < Min ; Measure > Max)
					->  true
					;   append(Options, MatchOptions, NewOptions),
					    assert_cell(C1, C2, [graph(TargetGraph),
								 alignment(TargetGraph)
								|NewOptions])
					)
			       )
			      )
		       ),
	provenance_stamp(Request, EdoalGraph, TargetGraph, Provenance),
	rdf_assert(Provenance, dcterms:title, literal('Provenance: about this selected alignment'), TargetGraph),
	rdf_assert(Provenance, amalgame:minimalConfidence, literal(type(xsd:float, Min)), TargetGraph),
	rdf_assert(Provenance, amalgame:maximalConfidence, literal(type(xsd:float, Max)), TargetGraph),
	rdf_assert(TargetGraph, rdf:type, amalgame:'SelectionAlignment', TargetGraph).

provenance_stamp(Request, EdoalGraph, TargetGraph, Provenance) :-
	get_time(T), format_time(atom(Time), '%a, %d %b %Y %H:%M:%S %z', T),
	logged_on(User, 'anonymous'),
	git_component_property('ClioPatria', version(CP_version)),
	git_component_property('amalgame',   version(AG_version)),
	format(atom(Version), 'Made using Amalgame ~w/Cliopatria ~w', [AG_version, CP_version]),
	http_current_host(Request, Hostname, Port, [global(true)]),
	memberchk(request_uri(ReqURI), Request),
	memberchk(protocol(Protocol), Request),
	format(atom(ReqUsed), '~w://~w:~w~w', [Protocol,Hostname,Port,ReqURI]),
	rdf_bnode(Provenance),
	rdf_assert(Provenance, rdf:type, amalgame:'Provenance', TargetGraph),
	rdf_assert(Provenance, dcterms:date, literal(Time), TargetGraph),
	rdf_assert(Provenance, dcterms:creator, literal(User), TargetGraph),
	rdf_assert(Provenance, dcterms:source, EdoalGraph, TargetGraph),
	rdf_assert(Provenance, owl:versionInfo, literal(Version), TargetGraph),
	rdf_assert(Provenance, amalgame:request, literal(ReqUsed), TargetGraph),
	rdf_assert(TargetGraph, amalgame:provenance, Provenance, TargetGraph).

