:- module(type_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.
:- public partition/5.

amalgame_module(amalgame:'TypeSelect').
amalgame_module(amalgame:'TypePartition').

parameter(class, uri, '',
	  'rdfs:Class from which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

selecter(Scheme, and((Scheme), type(Class)), Options) :-
	option(mode(select), Options),
	option(class(Class), Options).
selecter(Scheme, and((Scheme), not(type(Class))), Options) :-
	option(mode(remove), Options),
	option(class(Class), Options).

partition(VocSpec, Sel, Dis, [], Options) :-
	option(mode(select), Options),
	option(class(Class), Options),
	Sel = and((VocSpec), type(Class)),
	Dis = and((VocSpec), not(type(Class))).

