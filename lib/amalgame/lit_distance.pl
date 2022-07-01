/*  This file was part of ClioPatria.

*/

:- module(lit_distance,
	  [
	    literal_distance/3		% +L1, +L2, -D
	  ]).


:- use_module(library(semweb/rdf_litindex)).


		 /*******************************
		 *	       DISTANCE		*
		 *******************************/

goal_expansion(forall(C0, A0), \+ (C, \+ A)) :-
        expand_goal(C0, C),
        expand_goal(A0, A).

%
%	Compute the distance between two   literals. Distance values are
%	>= 0, where 0  means  perfect   match.  Handicaps  are  given to
%	inserted, deleted, moved and modified tokens.  See the above URL
%	for a description of the best edit-sequence comparing strings.

literal_distance(L1, L2, Distance) :-
	tokens(L1, TL1),
	tokens(L2, TL2),
	length(TL1, N1),
	length(TL2, N2),
	abs(N1-N2) =< min(N1, N2),	% too much difference in length
	cheapest_edit_path(TL1, TL2, EditCost, _Path),
	Distance is EditCost/max(N1,N2).

tokens(Tokens, Tokens) :-
	is_list(Tokens), !.
tokens(Spec, Tokens) :-
	atom(Spec), !,
	rdf_tokenize_literal(Spec, Tokens).
tokens(Number, [Number]) :-
	number(Number).


%%	cheapest_edit_path(+List1, +List2, -Distance, -Path)
%
%	Compute the cheapest edit path. As edit operations are weighted,
%	this is not necessarily the shorted   one,  but the algorithm is
%	basically the same.
%
%	@see http://www.ling.ohio-state.edu/~cbrew/2002/winter/684.02/string-distance.html

cheapest_edit_path(Toks1, Toks2, Distance, Path) :-
%	DelCost   = 10,			% Delete token
%	InsCost	  = 8,			% Insert token
	MatchCost = 0,			% Matched token
	CaseCost  = 1,			% Different case
	DWIMCost  = 3,			% Spelling error
	StemCost  = 5,			% Tokens have same stem
	SubstCost = 10,			% Replaced token
	MovCost   = 2,			% Token is moved

	T1 =.. [string|Toks1],
	T2 =.. [string|Toks2],
	functor(T1, _, M),
	functor(T2, _, N),
	X is M + 1,
	Y is N + 1,
	M1 is M - 1,
	N1 is N - 1,
	new_array(X, Y, Array),
	nb_set_array(Array, 0, 0, c(0, [])),
	forall(between(0, M1, I),
	       (   get_array(Array, I, 0, c(V0, P0)),
		   I1 is I+1,
		   arg(I1, T1, D),
		   del_cost(D, DC),
		   V is V0 + DC,
		   nb_link_array(Array, I1, 0, c(V, [del(D)|P0]))
	       )),
	forall(between(0, N1, J),
	       (   get_array(Array, 0, J, c(V0, P0)),
		   J1 is J+1,
		   arg(J1, T2, I),
		   ins_cost(I, IC),
		   V is V0 + IC,
		   nb_link_array(Array, 0, J1, c(V, [ins(I)|P0]))
	       )),
	forall(between(0, M1, I),
	       forall(between(0, N1, J),
		      (   I1 is I + 1,
			  J1 is J + 1,
		          arg(I1, T1, V1),
			  arg(J1, T2, V2),
			  (   V1 == V2
			  ->  Subst = MatchCost
			  ;   downcase_atom(V1, L),
			      downcase_atom(V2, L)
			  ->  Subst = CaseCost
			  ;   dwim_match(V1, V2)
			  ->  Subst = DWIMCost
			  ;   same_stem(V1, V2)
			  ->  Subst = StemCost
			  ;   Subst = SubstCost
			  ),
			  get_array(Array, I,  J, c(C1, P1)),
			  get_array(Array, I1, J, c(C2, P2)),
			  get_array(Array, I, J1, c(C3, P3)),

			  SubstC is C1 + Subst,
			  (   memberchk(del(V2), P2)
			  ->  del_cost(V2, DC2),
			      InsC is C2 - DC2 + MovCost
			  ;   ins_cost(V2, InsCost),
			      InsC is C2 + InsCost
			  ),
			  (   memberchk(ins(V1), P3)
			  ->  ins_cost(V1, IC1),
			      DelC is C3 - IC1 + MovCost
			  ;   del_cost(V1, DelCost),
			      DelC is C3 + DelCost
			  ),

			  (   SubstC < InsC
			  ->  (   SubstC < DelC
			      ->  nb_link_array(Array, I1, J1,
						c(SubstC, [subst(V1, V2)|P1]))
			      ;   nb_link_array(Array, I1, J1,
						c(DelC, [del(V1)|P3]))
			      )
			  ;   (   DelC < InsC
			      ->  nb_link_array(Array, I1, J1,
						c(DelC, [del(V1)|P3]))
			      ;   nb_link_array(Array, I1, J1,
						c(InsC, [ins(V2)|P2]))
			      )
			  )))),
%	pp_array(Array),
	get_array(Array, M, N, c(Distance, Path0)),
	reverse(Path0, Path).

ins_cost((','), 1) :- !.
ins_cost(_, 8).

del_cost((','), 1) :- !.
del_cost(_, 10).

same_stem(T1, T2) :-
	atom(T1), atom(T2), !,
	porter_stem(T1, Stem),
	porter_stem(T2, Stem).


		 /*******************************
		 *     SIMPLE ARRAY PACKAGE	*
		 *******************************/

new_array(X, Y, Array) :-
	Size is X * Y + 3,
	functor(Array, array, Size),
	arg(1, Array, 2),		% dimensions
	arg(2, Array, X),		% columns
	arg(3, Array, Y).		% rows

get_array(Array, X, Y, Val) :-
	arg(2, Array, Cols),
	Pos is X + Y*Cols + 4,
	arg(Pos, Array, Val).

nb_set_array(Array, X, Y, Val) :-
	arg(2, Array, Cols),
	Pos is X + Y*Cols + 4,
	nb_setarg(Pos, Array, Val).

nb_link_array(Array, X, Y, Val) :-
	arg(2, Array, Cols),
	Pos is X + Y*Cols + 4,
	nb_linkarg(Pos, Array, Val).

		 /*******************************
		 *	     DEBUGGING		*
		 *******************************/

end_of_file.

pp_array(Array) :-
	functor(Array, array, _),
	arg(1, Array, 2),
	arg(2, Array, Cols),
	arg(3, Array, Rows),
	MaxX is Cols-1,
	MaxY is Rows-1,
	forall(between(0, MaxY, Y),
	       (   forall(between(0, MaxX, X),
			  (	  get_array(Array, X, Y, V),
				  format('~w ', [V])
			  )),
		   nl
	       )).

