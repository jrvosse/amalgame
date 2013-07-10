:- module(propvalue_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.

amalgame_module(amalgame:'PropertyValueSelect').

parameter(property, uri, any,
	  'RDF property by which to select the concepts').
parameter(value, uri, any,
	  'RDF object/property value by which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts with this property/value pair').

% a bit naive at the moment: we simply change the query that should be
% done.

selecter(Scheme, and((Scheme), propvalue(Property, Value)), Options) :-
	option(mode(select), Options),
	option(property(Property), Options),
	option(value(Value),       Options).

selecter(Scheme, and((Scheme), not(propvalue(Property, Value))), Options) :-
	option(mode(remove), Options),
	option(property(Property), Options),
	option(value(Value),       Options).
