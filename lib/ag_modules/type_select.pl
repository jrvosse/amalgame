:- module(type_select, []).

:- use_module(library(semweb/rdf_db)).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.

amalgame_module(amalgame:'TypeSelect').

parameter(class, uri, '',
	  'rdfs:Class from which to select the concepts').

% a bit naive at the moment: we simply change the query that should be
% done.

selecter(Scheme, and((Scheme), type(Class)), Options) :-
   	option(class(Class), Options).
