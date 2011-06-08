:- module(most_labels, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

parameter(type,
	  oneof(source,target), source,
	  'Select best sources or best targets').

amalgame_module(amalgame:'MostLabels').

%%      selecter(+Source, -Selected, -Discarded, -Undecided, +Options)
%
%

selecter(AlignmentGraph, Sel, Disc, Und, Options) :-
	option(type(SourceOrTarget), Options, source),
	(   SourceOrTarget = target
	->  partition_(SourceOrTarget, AlignmentGraph, Sel, Disc, Und)
	;   predsort(ag_map:compare_align(targetplus), AlignmentGraph, SortedAlignmentGraph),
	    partition_(SourceOrTarget, SortedAlignmentGraph, Sel0, Disc0, Und0),
	    predsort(ag_map:compare_align(sourceplus), Sel0,  Sel),
	    predsort(ag_map:compare_align(sourceplus), Disc0, Disc),
	    predsort(ag_map:compare_align(sourceplus), Und0,  Und)
	).


%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains
%	all ambiguous alignments and the second the unambiguous
%	ones.
%       Input is a sorted list of alignment terms.

partition_(_, [], [], [], []).
partition_(source, [align(S,T,P)|As], Sel, Dis, Und) :-
	same_source(As, S, Same, Rest),
	(   most_labels([align(S,T,P)|Same], Selected, Discarded)
	->  Sel = [Selected|SelRest],
	    append(Discarded, DisRest, Dis),
	    Und = UndRest
	;   append([align(S,T,P)|Same], UndRest, Und),
	    Sel = SelRest,
	    Dis = DisRest
	),
	partition_(source, Rest, SelRest, DisRest, UndRest).

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,
	same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

most_labels(As, Selected, Discarded) :-
	group_label_count(As, Counts),
	!,
	(   Counts = [_N-Selected]
	->  Discarded = []
	;   sort(Counts, [N-Selected,N1-A|T0]),
	    N > N1,
	    pairs_values(T0, T),
	    Discarded = [A|T]
	).

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
