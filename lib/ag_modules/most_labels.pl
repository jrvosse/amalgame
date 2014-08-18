:- module(most_labels, []).

:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

parameter(type,
	  oneof([source,target]), target,
	  'source = select source with most matching labels for each target, target = select target with most matching labels for each source').

amalgame_module(amalgame:'MostLabels').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%	Source is sorted (e.g. on S).

selecter(AlignmentGraph, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, target),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, AlignmentGraph, Sel, Disc, Und)
	;   predsort(ag_map:compare_align(target), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, SortedAlignmentGraph, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Disc0, Disc),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).


%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition_(_, [], [], [], []).
partition_(source, [align(S,T,P)|At], Sel, Dis, Und) :-
	same_target(At, T, Same, Rest),
	(   most_labels([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

partition_(target, [align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   most_labels([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Rest, SelRest, DisRest, UndRest).

ap(Result, Most, SecondMost, align(S,T,P), align(S,T,Pnew)) :-
	append(P, [[method(most_labels),
		    score([result(Result),
			   most(Most),
			   second(SecondMost)])]], Pnew).

most_labels(As, Selected, Discarded) :-
	group_label_count(As, Counts),
	!,
	sort(Counts, Sorted),
	append(T0, [N2-DA, N1-Selected0], Sorted),
	N1 > N2,
	pairs_values(T0, T),
	Discarded0 = [DA|T],
	ap(selected, N1, N2, Selected0,  Selected),
	maplist(ap(discarded, N1, N2), Discarded0, Discarded).

group_label_count([],[]).
group_label_count([Align|As], [Count-Align|Ts]) :-
	Align = align(_,_,Provenance),
	findall(M, (member(P,Provenance),label_match(M,P)), Methods),
	length(Methods, Count),
	group_label_count(As, Ts).

label_match(SP-TP, Prov) :-
	memberchk(graph([rdf(_,SP,literal(_)),
			 rdf(_,TP,literal(_))
			]), Prov).
