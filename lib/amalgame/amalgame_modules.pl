:- module(amalgame_modules,
	  [ current_amalgame_module/3,    % ?class, ?Type, ?Module
	    amalgame_module_id/3,         % +Class, +Type, -Module
	    amalgame_class_modules/2,	  % +Class, -Modules:[uri-module]
	    amalgame_module_parameters/2  % +Module, -Parameters
	  ]).

:- use_module(library(semweb/rdf_db)).

%%	current_amalgame_module(?Class, ?URI, ?ModuleName)
%
%	True if Module is an amalgame module identified by URI and
%	belonging to Class.

current_amalgame_module(Class, URI, Module) :-
 	current_predicate(Module:amalgame_module/2),
	Module:amalgame_module(Class0, URI0),
	rdf_global_id(Class0, Class),
	rdf_global_id(URI0, URI).

%%	amalgame_module_id(+Class, +URI, -Module)
%
%	True if Module is an amalgame module with URI and of type Class.
%
%	@param Type is an RDF Class
%	@error existence_error(amalgame_module)
%	@see current_amalgame_module/1 for a version that fails if there
%	is no module

amalgame_module_id(Class, URI, Module) :-
	(   current_amalgame_module(Class, URI, M)
	->  Module = M
	;   throw(error(existence_error(amalgame_module, [Class, URI]), _))
	).

:- rdf_meta
	amalgame_class_modules(r, -).

%%	amalgame_class_modules(+Class, -Modules)
%
%	Modules is a list of currently loaded amalgames modules of type
%	Class.

amalgame_class_modules(Class, Modules) :-
 	findall(URI-M, current_amalgame_module(Class, URI, M), Modules).

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


