:- module(ag_correspondence,
	  [
	      correspondence_source/2,
	      correspondence_target/2,
	      correspondence_evidence/2,
	      correspondence_element/3,

	      sort_correspondences/3, % +Type, +In, -Sorted
	      same_source/4,          % +List, +Source, -Same, -Rest
	      same_target/4,          % +List, +Target, -Same, -Rest
	      merge_provenance/2      % +List, -Merged

	  ]
	 ).

/** <module> Amalgame correspondences (map) module

This module contains predicates to deal with correspondences while
abstracting from the underlying formats. This should converge into a
set of functions around sorted lists with
align(Source,Target,EvidenceList) terms.

@author Jacco van Ossenbruggen
@license LGPL
*/

:- use_module(library(lists)).


%%	correspondence_source(?C,?S) is det.
%
%	Unifies S with the source of correspondence C.

correspondence_source(align(S,_,_), S).

%%	correspondence_target(?C,?T) is det.
%
%	Unifies T with the target of correspondence C.
%
correspondence_target(align(_,T,_), T).

%%	correspondence_evidence(?C,?E) is det.
%
%	Unifies E with the evidence list of correspondence C.

correspondence_evidence(align(_,_,E), E).

correspondence_element(source,   C, S) :- correspondence_source(C,S).
correspondence_element(target,   C, T) :- correspondence_target(C,T).
correspondence_element(evidence, C, E) :- correspondence_evidence(C,E).

%%	same_source(+List, +Source, -Same, -Rest) is det.
%
%	Same contains all alignments from List that have Source as a
%	source, Rest contains all alignments with a different source.
%	List, Same and Rest are assumed to be the usual lists of
%	amalgame's align(S,T,P), sorted on S.

same_source([align(S,T,P)|As], S, [align(S,T,P)|Same], Rest) :-
	!,  same_source(As, S, Same, Rest).
same_source(As, _S, [], As).

%%	same_target(+List, +Target, -Same, -Rest) is det.
%
%	Same contains all alignments from List that have Target as a
%	target, Rest contains all alignments with a different target.
%	List, Same and Rest are assumed to be the usual lists of
%	amalgame's align(S,T,P), sorted on T.

same_target([align(S,T,P)|As], T, [align(S,T,P)|Same], Rest) :-
	!,  same_target(As, T, Same, Rest).
same_target(As, _S, [], As).


%!	sort_correspondences(Type, In, Out) is det.
%
%	Sort list of correspondences on source or target.

sort_correspondences(source, In, Out) :-
	sort(In, Out).

sort_correspondences(target, In, Out) :-
	sort(In, In0),
	sort(2, @=<, In0, Out).

%%      merge_provenance(+AlignIn, -AlignOut)
%
%       Collects all provenance for similar source target pairs.
%       AlignIn is a sorted list of align/3 terms.

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

