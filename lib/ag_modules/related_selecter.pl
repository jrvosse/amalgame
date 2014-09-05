:- module(related_selecter,
	  []).

:- use_module(structure_selecter).
:- use_module(related).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'RelatedSelecter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct related concepts only').
parameter(type,
          oneof([source, target, all]), all,
          'Select all related concepts or pick the best source/target.').

selecter(In, Sel, Dis, Und, Options) :-
	selecter(related_match, In, Sel, Dis, Und, Options).
