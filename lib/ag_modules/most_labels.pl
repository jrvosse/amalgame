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

selecter(Mapping, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, target),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, Mapping, Sel, Disc, Und)
	;   sort_align(target, Mapping, TSorted),
	    partition_(SourceOrTarget, TSorted, Sel0, Disc0, Und0),
	    sort_align(source, Sel0,  Sel),
	    sort_align(source, Disc0, Disc),
	    sort_align(source, Und0,  Und)
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
	(   most_labels(source, [align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [align(S,T,P)|Same],
	    maplist(ap(source, undecided), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

partition_(target, [align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   most_labels(target, [align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   Undecided = [align(S,T,P)|Same],
	    maplist(ap(target, undecided), Undecided, UndecidedP),
	    append(UndecidedP, UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(target, Rest, SelRest, DisRest, UndRest).

ap(Type, Result, align(S,T,P), align(S,T,Pnew)) :-
	append([[method(most_labels),
		    score([result(Result), type(Type)])
		]], P, Pnew).

ap(Type, Result, Most, SecondMost, align(S,T,P), align(S,T,Pnew)) :-
	append([[method(most_labels),
		 score([result(Result), type(Type),
			most(Most),
			second(SecondMost)])
		]], P, Pnew).

most_labels(Type, As, Selected, Discarded) :-
	group_label_count(As, Counts),
	!,
	sort(Counts, Sorted),
	append(T0, [N2-DA, N1-Selected0], Sorted),
	N1 > N2,
	pairs_values(T0, T),
	Discarded0 = [DA|T],
	ap(Type, selected, N1, N2, Selected0,  Selected),
	maplist(ap(Type, discarded, N1, N2), Discarded0, Discarded).

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
