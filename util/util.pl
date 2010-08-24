:- module(util,
	  [ find_unique/4 		% +Var, +Goal, +Max, -Results
	  ]).


% :- use_module(util(owl_ultra_lite)).
% :- use_module(util(iface_util)).
% :- use_module(serql(rdfql_runtime)).
% :- use_module(library('semweb/rdf_db')).
% :- use_module(library('semweb/rdfs')).
% :- use_module(library(pairs)).
% :- use_module(library(nb_set)).
% :- use_module(library(rbtrees)).

		 /*******************************
		 *	       APPLY		*
		 *******************************/

:- meta_predicate
	find_unique(-, 0, +, -).

%%	find_unique(Var, :Goal, +MaxResults, -SortedSet)
%
%	Find at most MaxResults distinct solutions for Var in Goal.

find_unique(T, G, inf, Ts) :- !,
	findall(T, G, Raw),
	sort(Raw, Ts).
find_unique(T, G, Max, Ts) :-
	empty_nb_set(Set),
	State = count(0),
	(	G,
		add_nb_set(T, Set, true),
		arg(1, State, C0),
		C is C0 + 1,
		nb_setarg(1, State, C),
		C == Max
	->	true
	;	true
	),
	nb_set_to_list(Set, Ts).
