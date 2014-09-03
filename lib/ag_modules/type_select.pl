:- module(type_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/5.

amalgame_module(amalgame:'TypeSelect').

parameter(class, uri, '',
	  'rdfs:Class from which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

selecter(VocSpec, Sel, Dis, [], Options) :-
	option(mode(select), Options),
	option(class(Class), Options),
	Sel = and((VocSpec), type(Class)),
	Dis = and((VocSpec), not(type(Class))).

