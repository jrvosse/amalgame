:- module(jaccard,
	  [
	   jaccard_similarity/3
	  ]).

/*
@see http://en.wikipedia.org/wiki/Jaccard_coefficient
*/

%%	jaccard_coefficient(+Atom1, +Atom2, -Similarity) is det.
%
%	Returns the jaccard similarity coefficient of two atoms,
%	based on the overlap of their tokens.

jaccard_similarity(S1, S2, Similarity) :-
	tokenize_atom(S1, T1),
	tokenize_atom(S2, T2),
	sort(T1, Sorted1),
	sort(T2, Sorted2),
	ord_union(Sorted1, Sorted2, Union),
	ord_intersection(Sorted1, Sorted2, Intersection),
	length(Intersection, Length1),
	length(Union, Length2),
	Similarity is Length1/Length2.
