:- module(propvalue_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.

amalgame_module(amalgame:'PropertyValueSelect').

parameter(property, uri, '',
	  'RDF property by which to select the concepts').
parameter(value, uri, '',
	  'RDF object/property value by which to select the concepts').

% a bit naive at the moment: we simply change the query that should be
% done.

selecter(Scheme, and((Scheme), propvalue(Property, Value)), Options) :-
	option(property(Property), Options),
	option(value(Value), Options).
