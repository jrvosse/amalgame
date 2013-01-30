:- module(ag_provenance,
	[
	 provenance_graph/2,
	 add_amalgame_opm/3,
	 update_amalgame_opm/2,                      % -Strategy, +Mapping
	 flush_prov_cache/0,
	 remove_old_prov/2                           % +Process, +ProvGraph
	]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/opm)).
:- use_module(library(amalgame/map)).


%%	provenance_graph(+Strategy, ?Graph) is det.
%
%	True if Graph is the provenance graph associated with strategy.

provenance_graph(Strategy, Graph) :-
	rdf(Graph, amalgame:strategy, Strategy, Graph),
	!.

provenance_graph(Strategy, Graph) :-
	ground(Strategy),
	rdf(Strategy, amalgame:publish_ns, NS),
	(   atomic_list_concat([NS, prov],  Graph), \+ rdf_graph(Graph)
	->  true
	;   repeat, gensym('provgraph', Local),
	    atomic_list_concat([NS, Local], Graph),
	    \+ rdf_graph(Graph)
	),
	create_prov_graph(Strategy, Graph).

create_prov_graph(Strategy, Graph) :-
	format(atom(Label), 'Provenance graph for strategy ~p', [Strategy]),
	rdf_assert(Graph, rdf:type, prov:'Bundle', Graph),
	rdf_assert(Graph, amalgame:strategy, Strategy, Graph),
	rdf_assert(Graph, rdfs:label, literal(lang(en,Label)), Graph),
	% Copy Strategy triples to empty prov graph:
	findall(rdf(Strategy,P,O), rdf(Strategy,P,O,Strategy), STriples),
	forall(member(rdf(S,P,O), STriples), rdf_assert(S,P,O,Graph)).

flush_prov_cache :-
	clear_prov_cache.

update_amalgame_opm(Strategy, Mapping) :-
	provenance_graph(Strategy, ProvGraph),
	rdf_retractall(Mapping, _, _, ProvGraph),
	findall(rdf(Mapping,P,O),
		rdf(Mapping,P,O,Strategy),
		Triples),
	forall(member(rdf(S,P,O), Triples),
	       rdf_assert(S,P,O,ProvGraph)
	      ).

add_amalgame_opm(Strategy, Process, Results) :-
	rdf_equal(prov:used, OpmvUsed),
	rdf_equal(prov:wasDerivedFrom, OpmvWDF),

	provenance_graph(Strategy, ProvGraph),

	% Remove old info about Process from ProvGraph
	remove_old_prov(Process, ProvGraph),

	% Copy all triples about Process from Strategy to ProvGraph
	findall(rdf(Process, P, O), rdf(Process,P,O,Strategy), ProcessTriples),

	% Translate subProperties of ompv:used to prov:used
	findall(rdf(Process, OpmvUsed, S),
		(   rdf_has(Process, OpmvUsed, S, RealProp),
		    rdf(Process, RealProp, S, Strategy)
		),
		InputTriples),

	(   Results = scheme(Vocab)
	->  Artifacts = [Vocab]
	;   assert_counts(Results, ProvGraph),
	    pairs_keys(Results, Artifacts)
	),

	opm_was_generated_by(Process, Artifacts, ProvGraph, []),

	% Generate prov:wasDerivedFrom triples between Mappings
	findall(rdf(Target, OpmvWDF, Source),
		(   member(Target, Artifacts),
		    rdf_has(Process, prov:used, Source, RealProp),
		    rdf(Process, RealProp, S, Strategy)
		),
		DerivedTriples),

	% Copy all triples about the Mapping from Strategy to ProvGraph
	findall(rdf(S,P,O),
		(   member(S, Artifacts),
		    rdf(S,P,O,Strategy)
		), ArtifactTriples),

	append([ProcessTriples,
		ArtifactTriples,
		InputTriples,
		DerivedTriples], AllTriples),
	forall(member(rdf(S,P,O), AllTriples), rdf_assert(S,P,O,ProvGraph)).


%%	remove_old_prov(+Process, +Graph) is det.
%
%	Remove all provenance triples related to Process from Graph.
%

remove_old_prov(Process, ProvGraph) :-
	findall(Bnode,
		(   rdf(Process, _, Bnode, ProvGraph),
		    rdf_is_bnode(Bnode),
		    \+ (rdf(OtherProcess, _, Bnode, ProvGraph),
			OtherProcess \= Process),
		    \+ rdfs_individual_of(Bnode, prov:'SoftwareAgent')
		),
		Bnodes),
	forall(member(B,Bnodes), remove_old_prov(B,ProvGraph)),
	rdf_retractall(Process, _, _, ProvGraph),
	rdf_retractall(_, _ ,Process, ProvGraph).
