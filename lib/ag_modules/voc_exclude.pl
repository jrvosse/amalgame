:- module(voc_exclude, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/5.


amalgame_module(amalgame:'VocExclude').

parameter(type, oneof([source,target]), source,
	  'Exclude matching sources or targets').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts of this type').

selecter(VocSpec, Sel, Dis, [], Options) :-
	S =  and((VocSpec), not(is_mapped(Options))),
	D =  and((VocSpec), is_mapped(Options)),
	(   option(mode(select), Options, select)
	->  Sel = S, Dis = D
	;   Sel = D, Dis = S
	).

