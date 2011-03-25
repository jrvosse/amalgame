:- module(alignment_graph,
	  [ e/2,
	    flush_map_cache/0,
	    flush_map_cache/1,
	    graph_member/2,
	    merge_graphs/2,
	    merge_provenance/2,
	    materialize_alignment_graph/2,
	    debug_partition/2,
	    compare_align/4
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/opm)).

:- use_module(library(http/http_parameters)).

% components
:- use_module(library(amalgame/matchers/snowball_match)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/select/select1_1)).

:- dynamic
	map_cache/2.

flush_map_cache :-
	flush_map_cache(_).
flush_map_cache(Id) :-
	retractall(map_cache(Id,_)).

debug_partition(Label, Partition) :-
	forall(member(SetPred, Partition),
	       (SetPred =.. [Type, Set],
		source_count(Set, Count),
		length(Set, Length),
		debug(align, '~w ~w: ~w sources in ~w maps', [Label, Type, Count, Length])
	       )
	      ).
source_count(As, N) :-
	maplist(align_source, As, Ss0),!,
	sort(Ss0, Ss),
	length(Ss, N).

source_count(As, -1) :-
	length(As, N),
	debug(align, 'cluster count: ~w', [N]).

align_source(align(S,_,_), S).

e(Id, Mapping) :-
	rdf_display_label(Id, Label),
	debug(align, 'Expanding ~w', [Label]),
	rdf_has(Id, opmv:wasGeneratedBy, Process),
	rdf(Process, rdf:type, Type),
	do_process(Process, Type, Id, Mapping).

%%	do_process(+Process, +Type, +Id, -Mapping)
%
%	Execute process to generate Mapping with Id.

do_process(Process, Type, Id, Mapping) :-
	rdfs_subclass_of(Type, amalgame:'Match'),
	!,
	resource_to_term(Type, Module),
	process_options(Process, Module, Options),
	debug(align, 'running ~w matcher', [Module]),
	(   rdf(Process, amalgame:input, MappingIn)  % input is an alignment graphs
	->  call(Module:filter, MappingIn, Mapping, Options)
	;   rdf(Process, amalgame:source, Source),
	    rdf(Process, amalgame:target, Target)
	->  call(Module:matcher, Source, Target, Mapping, Options)
	),
	length(Mapping, N),
	(   N == 0
	->  gtrace
	;   debug(align, 'Caching ~w correspondences', [N]),
	    assert(map_cache(Id, Mapping))
	).

do_process(Process, Type, Id, Mapping) :-
	rdfs_subclass_of(Type, amalgame:'Select'),
	!,
 	rdf(Process, amalgame:input, Source),
 	resource_to_term(Type, Module),
	process_options(Process, Module, Options),
	findall(A, graph_member(A, Source), Graph0),
	sort(Graph0, Graph),
	length(Graph, N0),
	debug(align, 'running ~w select on ~w corr.', [Module, N0]),
 	call(Module:selecter, Graph, Selected, Discarded, Undecided, Options),
	rdf_has(Id, opmv:used, _, P0),
	resource_to_term(P0, P),
	select_mapping(P, Selected, Discarded, Undecided, Mapping),
	length(Mapping, N1),
	debug(align, 'Selected ~w (~w)', [N1,P]).

select_mapping(selected, Selected, _, _, Selected).
select_mapping(discarded, _, Discarded, _, Discarded).
select_mapping(undecided, _, _, Undecided, Undecided).


%%	process_options(+Process, +Module, -Options)
%
%	Options are the instantiated parameters for Module based on the
%	parameters string in Process.

process_options(Process, Module, Options) :-
	rdf(Process, amalgame:parameters, literal(ParamString)),
	!,
	module_options(Module, Options, Parameters),
	parse_url_search(ParamString, Search),
	Request = [search(Search)] ,
	http_parameters(Request, Parameters).
process_options(_, _, []).

%%	module_options(+Module, -Options, -Parameters)
%
%	Options  are  all  option  clauses    defined   for  Module.
%	Parameters is a specification list for http_parameters/3.
%	Module:parameter is called as:
%
%	    parameter(Name, Properties, Description)
%
%	Name is the name of the	the option, The Properties are as
%	supported by http_parameters/3.	Description is used by the help
%	system.

module_options(Module, Options, Parameters) :-
	findall(O-P,
		( call(Module:parameter, Name, Properties, _Description),
		  O =.. [Name, Value],
		  P =.. [Name, Value, Properties]
		),
		Pairs),
	pairs_keys_values(Pairs, Options, Parameters).

%%	http:convert_parameter(+Type, +In, -URI) is semidet.
%
%	HTTP parameter conversion for the following types:
%
%	    * uri
%	    This  conversion  accepts NS:Local and absolute URIs.

http:convert_parameter(uri, In, URI) :-
	(   sub_atom(In, B, _, A, :),
	    sub_atom(In, _, A, 0, Local),
	    xml_name(Local)
	->  sub_atom(In, 0, B, _, NS),
	    rdf_global_id(NS:Local, URI)
	;   is_absolute_url(In)
	->  URI = In
	).



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
	;   map_cache(MappingId, Mapping)
	->  debug(align, 'using cache for ~w', [MappingId]),
	    member(align(S,T,P), Mapping)
	;   e(MappingId, Mapping)
	->  member(align(S,T,P), Mapping)
	).

graph_member(_, Id) :-
	rdf_display_label(Id, Label),
	debug(align, 'WARNING: graph_member/2 failed for ~w (~w)', [Label,Id]),
	fail.




%%	merge_graphs(+ListOfGraphs, -Merged)
%
%	Merge alignment terms. ListOfGraphs is ordered.

merge_graphs([], []) :- !.
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
group_provenance(As, S, T, P, [align(S, T, Psorted)|Gs]) :-
	sort(P, Psorted),
	merge_provenance(As, Gs).


%%	materialize_alignment_graph(+Alignments, +Options)
%
%	Assert Alignments as triples in the store.

materialize_alignment_graph(Input, Options) :-
	rdf_bnode(ProcessBnode),
	option(graph(Graph), Options, test),
	option(opmv_graph(OpmGraph), Options, opm),
	(   option(process(Process), Options)
	->  option(matcher(Matcher), Process),
	    option(was_derived_from(DerivedFrom), Process),
	    rdf_assert(ProcessBnode, amalgame:matcher, Matcher, OpmGraph)
	;   DerivedFrom = []
	),
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	),
	(   Input = []
	->  true
	;   rdf_persistency(Graph, false),
	    rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
	    opm_was_generated_by(ProcessBnode, Graph, OpmGraph, [was_derived_from(DerivedFrom)|Options]),
	    mat_alignment_graph(Input, Options)
	).

mat_alignment_graph([], _).
mat_alignment_graph([align(S,T, P)|As], Options) :-
	assert_cell(S, T, [prov(P)|Options]),
	% option(graph(Graph), Options, test),
	% rdf_assert(S, skos:exactMatch, T, Graph),
        mat_alignment_graph(As, Options).

save_alignment_graph(Graph, Options) :-
	opm_include_dependency(Graph, Graph),
	rdf_save_canonical_turtle(Graph, Options).

%%	compare_align(Type, Order, A1, A2) is det.
%
%	compare alignment A1 and A2 on the standard order of the url of
%	their source or target concept, depending on Type.

xcompare_align(Type, Order, A1, A2) :-
	debug(align, 'compare_align ~w ~w ~w ~w' , [Type, Order, A1, A2]),
	fail.

compare_align(source, Order, align(S1,_,_), align(S2,_,_)) :- compare(Order, S1, S2).

compare_align(target, Order, align(_,T1,_), align(_,T2,_)) :- compare(Order, T1, T2).

compare_align(sourceplus, Order, align(S1,T1,P1), align(S2,T2,P2)) :-
	(   compare(FirstOrder, S1, S2), FirstOrder \== '='
	->  FirstOrder = Order
	;   compare(SecondOrder, T1, T2), SecondOrder \== '='
	->  SecondOrder = Order
	;   compare(Order, P1, P2)
	).
compare_align(targetplus, Order, align(S1,T1,P1), align(S2,T2,P2)) :-
	(   compare(FirstOrder, T1, T2), FirstOrder \== '='
	->  FirstOrder = Order
	;   compare(SecondOrder, S1, S2), SecondOrder \== '='
	->  SecondOrder = Order
	;   compare(Order, P1, P2)
	).
