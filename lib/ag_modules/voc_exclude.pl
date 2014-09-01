:- module(voc_exclude, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.


amalgame_module(amalgame:'VocExclude').

parameter(type, oneof([source,target]), source,
	  'Exclude matching sources or targets').

selecter(Scheme, and((Scheme), not(is_mapped(Options))), Options).
