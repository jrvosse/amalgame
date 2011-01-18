:- module(alignment_graph,
	  [ graph_member/2,
	    merge_graphs/2,
	    merge_provenance/2,
	    materialize_alignment_graph/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/edoal)).

%%	graph_member(?Element, ?Graph)
%
%	Enumarate elements of Graph. Where Graph is a list, a skos
%	scheme URI or a named graph URI.

graph_member(E, List) :-
	is_list(List),
	!,
	member(E, List).
graph_member(E, scheme(Scheme)) :-
	!,
	rdf_has(E, skos:inScheme, Scheme).
graph_member(E, graph(Graph)) :-
	!,
	rdf_has(E, rdf:type, _, Graph).


%%	merge_graphs(+ListOfGraphs, -Merged)
%
%	Merge alignment terms. ListOfGraphs is ordered.

merge_graphs([], []).
merge_graphs([L], L) :- !.
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
        rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
        save_alignment_graph(Input, Options).

save_alignment_graph([], _).
save_alignment_graph([align(S,T,_Provenance)|As], Options) :-
        assert_cell(S, T, Options),
        save_alignment_graph(As, Options).
