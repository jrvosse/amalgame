:- module(ancestor_selecter,
	  []).

:- use_module(ancestor).
:- use_module(structure_selecter).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'AncestorSelecter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct parents only').
parameter(type,
          oneof([source, target, all]), all,
          'Select all ancestor matches or pick the best source/target.').

selecter(In, Sel, Dis, Und, Options) :-
	selecter(ancestor_match, In, Sel, Dis, Und, Options).


