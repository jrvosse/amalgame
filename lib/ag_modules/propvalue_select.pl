:- module(propvalue_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/5.

amalgame_module(amalgame:'PropertyValueSelect').

parameter(property, uri, any,
	  'RDF property by which to select the concepts').
parameter(value, uri, any,
	  'RDF object/property value by which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts with this property/value pair').

selecter(VocSpec, Sel, Dis, [], Options) :-
	option(property(Property), Options),
	option(value(Value),       Options),
	S =  and((VocSpec),	propvalue(Property, Value)),
	D =  and((VocSpec), not(propvalue(Property, Value))),
	(   option(mode(select), Options, select)
	->  Sel = S, Dis = D
	;   Sel = D, Dis = S
	).
