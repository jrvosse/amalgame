:- module(voc_exclude, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public specifier/5.
:- public selecter/5.

:- use_module(library(option)).
:- use_module(library(amalgame/vocabulary)).

amalgame_module(amalgame:'VocExclude').

parameter(type, oneof([source,target]), source,
	  'Exclude matching sources or targets').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

specifier(VocSpec, SelSpec, DisSpec, none, Options) :-
	S =  and((VocSpec), not(is_mapped(Options))),
	D =  and((VocSpec), is_mapped(Options)),
	(   option(mode(select), Options, select)
	->  SelSpec = S, DisSpec = D
	;   SelSpec = D, DisSpec = S
	).

selecter(VocSpec, SelConcepts, DisConcepts, [], Options) :-
	specifier(VocSpec, SelSpec, DisSpec, none, Options),
	all_vocab_members(SelSpec, SelConcepts),
	all_vocab_members(DisSpec, DisConcepts).

