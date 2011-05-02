:- module(amalgame_modules,
	  [ current_amalgame_module/2,    % ?URI, ?Module
	    amalgame_module_id/2,         % +Class, +Type, -Module
 	    amalgame_module_parameters/2  % +Module, -Parameters
	  ]).

:- use_module(library(semweb/rdf_db)).

%%	current_amalgame_module(?URI, ?ModuleName)
%
%	True if Module is an amalgame module identified by URI.

current_amalgame_module(URI, Module) :-
 	current_predicate(Module:amalgame_module/1),
	Module:amalgame_module(URI0),
 	rdf_global_id(URI0, URI).

%%	amalgame_module_id(+URI, -Module)
%
%	True if Module is an amalgame module with URI and of type Class.
%
%	@error existence_error(amalgame_module)
%	@see current_amalgame_module/1 for a version that fails if there
%	is no module

amalgame_module_id(URI, Module) :-
	(   current_amalgame_module(URI, M)
	->  Module = M
	;   throw(error(existence_error(amalgame_module, [URI]), _))
	).

%%	amalgame_module_parameters(+Module,-Parameters)
%
%       Parameters is a list of parameters for Module.

amalgame_module_parameters(Module, Params) :-
	current_predicate(Module:parameter/4),
	!,
	findall(parameter(Id, Type, Default, Desc),
		Module:parameter(Id, Type, Default, Desc),
		Params).
amalgame_module_parameters(_, []).


