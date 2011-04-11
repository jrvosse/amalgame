:- module(amalgame_modules,
	  [ amalgame_modules/1,
	    amalgame_module_def/3
	  ]).


%%	amalgame_modules(+Modules:class-module)
%
%

amalgame_modules(Ms) :-
	M = Class-Module,
	findall(M, ag_module_def(Class, _, Module), Ms0),
	keysort(Ms0, Ms).

%%	amalgame_module_def(?Class, ?Type, ?Module)
%
%

amalgame_module_def(Class, Type, Module) :-
	current_predicate(Module:amalgame_module/2),
	Module:amalgame_module(Class, Type).

