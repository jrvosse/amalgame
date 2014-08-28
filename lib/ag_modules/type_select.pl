:- module(type_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.

amalgame_module(amalgame:'TypeSelect').

parameter(class, uri, '',
	  'rdfs:Class from which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

% a bit naive at the moment: we simply change the query that should be
% done.

selecter(Scheme, and((Scheme), type(Class)), Options) :-
	option(mode(select), Options),
	option(class(Class), Options).
selecter(Scheme, and((Scheme), not(type(Class))), Options) :-
	option(mode(remove), Options),
	option(class(Class), Options).
