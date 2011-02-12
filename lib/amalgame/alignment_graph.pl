:- module(alignment_graph,
	  [ e/2,
	    flush_map_cache/0,
	    flush_map_cache/1,
	    graph_member/2,
	    merge_graphs/2,
	    merge_provenance/2,
	    materialize_alignment_graph/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(http/http_parameters)).

% components
:- use_module(library(amalgame/matchers/snowball_match)).
:- use_module(library(amalgame/select/select1_1)).

:- dynamic
	map_cache/2.

flush_map_cache :-
	flush_map_cache(_).
flush_map_cache(Id) :-
	retractall(map_cache(Id,_)).


e(Id, Mapping) :-
	rdf_has(Id, opmv:wasGeneratedBy, Process),
	rdf(Process, rdf:type, Type),
	do_process(Process, Type, Id, Mapping).

do_process(Process, Type, Id, Mapping) :-
	rdfs_subclass_of(Type, amalgame:'Match'),
	!,
 	rdf(Process, amalgame:source, Source),
	rdf(Process, amalgame:target, Target),
 	resource_to_term(Type, Module),
	process_options(Process, Module, Options),
	debug(align, 'running ~w matcher', [Module]),
 	call(Module:matcher, Source, Target, Mapping, Options),
	assert(map_cache(Id, Mapping)).

do_process(Process, Type, Id, Mapping) :-
	rdfs_subclass_of(Type, amalgame:'Select'),
	!,
 	rdf(Process, amalgame:source, Source),
 	resource_to_term(Type, Module),
	process_options(Process, Module, Options),
	findall(A, graph_member(A, Source), Graph0),
	sort(Graph0, Graph),
	debug(align, 'running ~w select', [Module]),
 	call(Module:selecter, Graph, Selected, Discarded, Undecided, Options),
	rdf_has(Id, opmv:used, _, P0),
	resource_to_term(P0, P),
	select_mapping(P, Selected, Discarded, Undecided, Mapping).

select_mapping(selected, Selected, _, _, Selected).
select_mapping(discarded, _, Discarded, _, Discarded).
select_mapping(undecided, _, _, Undecided, Undecided).


%%	process_options(+Process, -Options)
%
%

process_options(Process, Module, Options) :-
	rdf(Process, amalgame:parameters, literal(ParamString)),
	!,
	param_string_to_options(ParamString, Module, Options).
process_options(_, _, []).

param_string_to_options(String,Module, Params) :-
	Module:params(Params),
	parse_url_search(String, Search),
	FakeRequest = [search(Search)] ,
	http_parameters(FakeRequest, Params).

%%	resource_to_term(+RDFResource, -PrologTerm)
%
%	Convert Amalgame RDF classes to Prolog predicates.

resource_to_term(Resource, Term) :-
	rdf_global_id(_:Local, Resource),
	downcase_atom(Local, Term).


%%	graph_member(?Element, ?Graph)
%
%	Enumarate elements of Graph. Where Graph is a list, a skos
%	scheme URI or a named graph URI.

graph_member(_, Var) :-
	var(Var),
	!.
graph_member(E, List) :-
	is_list(List),
	!,
	member(E, List).
graph_member(E, scheme(Scheme)) :-
	!,
	rdf_has(E, skos:inScheme, Scheme).
graph_member(E, type(Class)) :-
	!,
	rdf_has(E, rdf:type, Class).
graph_member(E, graph(Graph)) :-
	!,
	rdf_has(E, rdf:type, _, Graph).
graph_member(E, Scheme) :-
	rdfs_individual_of(Scheme, skos:'ConceptScheme'),
	!,
	rdf(E, skos:inScheme, Scheme).
graph_member(E, Class) :-
	rdfs_individual_of(Class, rdfs:'Class'),
	!,
	rdfs_individual_of(E, Class).
graph_member(align(S,T,P), MappingId) :-
	rdfs_individual_of(MappingId, amalgame:'Mapping'),
	(   has_map(_,_,MappingId)
	->  has_map([S-T], P, MappingId)
	;   map_cache(MappingId, Mapping),
	    debug(align, 'using cache for ~w', [MappingId]),
	    member(align(S,T,P), Mapping)
	;   e(MappingId, Mapping),
	    member(align(S,T,P), Mapping)
	).






%%	merge_graphs(+ListOfGraphs, -Merged)
%
%	Merge alignment terms. ListOfGraphs is ordered.

merge_graphs([], []).
merge_graphs([L], L) :- !.
merge_graphs([[]|Tail], Merged) :- merge_graphs(Tail, Merged).
merge_graphs([L|Ls], [Merged|Ms]) :-
	smallest_heads(Ls, [L], Heads, Rest),
	merge_provenance(Heads, [Merged]),
	merge_graphs(Rest, Ms).


smallest_heads([], Smallest, Heads, Tails) :-
	heads_tails(Smallest, Heads, Tails).
smallest_heads([[]|Ls1], Ls0, Smallest, Rest) :-
	!,
	smallest_heads(Ls1, Ls0, Smallest, Rest).
smallest_heads([L1|Ls1], [L0|Ls0], Smallest, Rest) :-
	L1 = [align(S1,T1,_)|_],
	L0 = [align(S0,T0,_)|_],
	(   S1 == S0,
	    T1 == T0
 	->  smallest_heads(Ls1, [L0,L1|Ls0], Smallest, Rest)
	;   (   (S1 == S0, compare(<, T1, T0))
	    ;	compare(<, S1, S0)
	    )
	->  smallest_heads(Ls1, [L1], Smallest, Rest0),
	    append([L0|Ls0], Rest0, Rest)
 	;   Rest = [L1|Rest0],
	    smallest_heads(Ls1, [L0|Ls0], Smallest, Rest0)
	).


heads_tails([], [], []).
heads_tails([[H]|Ls], [H|Hs], Ts) :-
	!,
	heads_tails(Ls, Hs, Ts).
heads_tails([[H1|T1]|Ls], [H1|Hs], [T1|Ts]) :-
	heads_tails(Ls, Hs, Ts).

%%	merge_provenance(+AlignIn, -AlignOut)
%
%	Collects all provenance for similar source target pairs.
%	AlignIn is a sorted list of align/3 terms.

merge_provenance([], []).
merge_provenance([align(S, T, P)|As], Gs) :-
	group_provenance(As, S, T, P, Gs).

group_provenance([align(S,T,P)|As], S, T, P0, Gs) :-
	!,
	append(P, P0, P1),
	group_provenance(As, S, T, P1, Gs).
group_provenance(As, S, T, P, [align(S, T, P)|Gs]) :-
	merge_provenance(As, Gs).


%%	materialize_alignment_graph(+Alignments, +Options)
%
%	Assert Alignments as triples in the store.

materialize_alignment_graph(Input, Options) :-
	option(graph(Graph), Options, test),
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	),
	(   Input = []
	->  true
	;   rdf_persistency(Graph, false),
	    rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
	    save_alignment_graph(Input, Options)
	).

save_alignment_graph([], _).
save_alignment_graph([align(S,T,_Provenance)|As], Options) :-
        assert_cell(S, T, Options),
        save_alignment_graph(As, Options).
