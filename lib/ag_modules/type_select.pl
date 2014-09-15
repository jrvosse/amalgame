:- module(type_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public specifier/5.
:- public selecter/5.

:- use_module(library(option)).
:- use_module(library(amalgame/vocabulary)).

amalgame_module(amalgame:'TypeSelect').

parameter(class, uri, '',
	  'rdfs:Class from which to select the concepts').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

specifier(VocSpec, Sel, Dis, none, Options) :-
	option(mode(select), Options),
	option(class(Class), Options),
	Sel = and((VocSpec), type(Class)),
	Dis = and((VocSpec), not(type(Class))).

selecter(VocSpec, SelConcepts, DisConcepts, [], Options) :-
	specifier(VocSpec, SelSpec, DisSpec, none, Options),
	all_vocab_members(SelSpec, SelConcepts),
	all_vocab_members(DisSpec, DisConcepts).
