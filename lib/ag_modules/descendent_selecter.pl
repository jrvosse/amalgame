:- module(descendent_selecter,
	  []).

:- use_module(structure_selecter).
:- use_module(descendent_match).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'DescendentSelecter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct children only').
parameter(type,
          oneof([source, target, all]), all,
          'Select all descendent matches or pick the best source/target.').

selecter(In, Sel, Dis, Und, Options) :-
	selecter(descendent_match, In, Sel, Dis, Und, Options).

