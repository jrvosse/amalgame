:- module(most_labels_jacco, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/alignment_graph)).

:- public partition/3.
:- multifile amalgame:component/2.

amalgame:component(partition, most_labels(alignment_graph, [selected(alignment_graph),
							   discarded(alignment_graph),
							   undecided(alignment_graph)
							  ], [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition(AlignmentGraph, ListOfGraphs, Options) :-
	ListOfGraphs = [one_to_one(P11),
			selected(S),
			discarded(D),
			undecided(U)
		      ],
	arity_select:partition(AlignmentGraph, [p11(P11), pxn(P1N), pnx(PN1), pnm(PNM)], Options),
	debug_partition('most label partition', [p11(P11), p1x(P1N), pnx(PN1), pnm(PNM)]),
	partition_(source, P1N, Ss, Ds, Us),
	partition_(target, PN1, St0, Dt0, Ut0),
	sort(St0, St), sort(Dt0, Dt), sort(Ut0, Ut),

	ord_union([Ss, St], S),
	ord_union([Ds, Dt], D0), ord_subtract(D0,S,D), ord_union(S, D, SD),
	ord_union([Us, Ut], U0), ord_subtract(U0,SD,U).

partition_nm([], [], [], []).
partition_nm([Cluster|Tail], S, D, U):-
	partition_nm(Tail, Sr, Dr, Ur),
	partition_cluster(Cluster, Sc, Dc, Uc),
	ord_union(Sr, Sc, S),
	ord_union(Dr, Dc, D),
	ord_union(Ur, Uc, U).

:- dynamic
	mapped_source/1,
	mapped_target/1.

partition_cluster(Cluster, S, D, U) :-
	group_label_count(Cluster, Counts),
	sort(Counts, Sorted),
	retractall(mapped_source(_)),
	retractall(mapped_target(_)),
	select_best_1_1(Sorted, S, D, U).


select_best_1_1([], [], [], []).
select_best_1_1([_C1-H1|Tail], Sel, Disc, Unsure) :-
	H1 = align(S,T,_P),
	(   \+ mapped_source(S),
	    \+ mapped_target(T)
	->
	    assert(mapped_source(S)),
	    assert(mapped_target(T)),
	    Sel = [H1|Sr], Disc = Dr
	;   Disc = [H1|Dr], Sel = Sr
	),
	select_best_1_1(Tail, Sr, Dr, Unsure).


partition_(_, [], [], [], []).
partition_(Type, [align(S,T,P)|As], Sel, Dis, Und) :-
	(   Type = source
	->  same_source(As, S, Same, Rest)
	;   same_target(As, T, Same, Rest)
	),
	(   most_labels([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(Type, Rest, SelRest, DisRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :- !, same_source(As, S, Same, Rest).
same_source(As, _S, [], As).
same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :- !, same_target(As, T, Same, Rest).
same_target(As, _S, [], As).


most_labels(As, Selected, [A|T]) :-
	group_label_count(As, Counts),
	sort(Counts, [N-Selected,N1-A|T0]),
	\+ N == N1,
	pairs_values(T0, T).

group_label_count([], []).
group_label_count([Align|As], [Count-Align|Ts]) :-
	Align = align(_,_,Provenance),
	findall(M, (member(P,Provenance),label_match(M,P)), Methods),
	length(Methods, Count0),
	Count is 1/Count0,
 	group_label_count(As, Ts).

label_match(SP-TP, Prov) :-
	memberchk(graph([rdf(_,SP,literal(_)),
			 rdf(_,TP,literal(_))
			]), Prov).
