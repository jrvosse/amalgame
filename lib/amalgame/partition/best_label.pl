:- module(best_label, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- public partition/3.
:- multifile amalgame:component/2.

amalgame:component(partition, best_label(alignment_graph, [selected(alignment_graph),
							   discarded(alignment_graph),
							   undecided(alignment_graph)
							  ], [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition(AlignmentGraph, ListOfGraphs, _Options) :-
	ListOfGraphs = [selected(S),
			discarded(D),
			undecided(U)
		      ],
	partition_(AlignmentGraph, S, D, U).

partition_([], [], [], []).
partition_([align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   best_label([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(Rest, SelRest, DisRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

% preferred-preferred = 0.
% preferred-other = 1.
% other-other = 2.

best_label(As, Selected, Rest) :-
	group_label_prop_types(As, Types0),
	sort(Types0, Types),
	Types = [N-Selected,N1-A|T0],
	N < N1,
	pairs_values(T0,T),
	Rest = [A|T].

group_label_prop_types([], []).
group_label_prop_types([Align|As], [Type-Align|Ts]) :-
	Align = align(_,_,Provenance),
	label_provenance_types(Provenance, Types),
	sort(Types, [Type|_]),
	group_label_prop_types(As, Ts).

label_provenance_types([], []).
label_provenance_types([Prov|Ps], [Type|Ts]) :-
	memberchk(graph([rdf(_,SP,literal(_)),
			 rdf(_,TP,literal(_))
			]), Prov),
	label_pair_type(SP, TP, Type),
	label_provenance_types(Ps, Ts).

label_pair_type(SP, TP, Type) :-
	label_type(SP, ST),
	label_type(TP, TT),
	(   (ST == p, TT == p)
	->  Type = 0
	;   (ST == p; TT == p)
	->  Type = 1
	;   Type = 2
	).

label_type(P, p) :-
	rdfs_subproperty_of(P, skos:prefLabel),
	!.
label_type(_, o).

