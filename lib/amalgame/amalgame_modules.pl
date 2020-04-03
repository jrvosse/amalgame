:- module(amalgame_modules,
	  [
	    amalgame_module_id/2,         % +URI, -Module
	    amalgame_modules_of_type/2,   % +Class, -Modules
	    amalgame_module_parameters/2, % +Module, -Parameters
	    amalgame_module_property/2,	  % +URI, ?Term,
	    process_options/3
	  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(amalgame/rdf_util)).

:- rdf_meta
	amalgame_module_property(r,?),
	amalgame_module_id(r,-),
	amalgame_modules_of_type(r,-),
	current_amalgame_module(r, ?).

%%	current_amalgame_module(?URI, ?ModuleName) is nondet.
%
%	True if Module is an amalgame module identified by URI.
%	@see amalgame_module_id/2 for a det version that throws an
%	existence_error if there is no module

current_amalgame_module(URI, Module) :-
	current_predicate(Module:amalgame_module/1),
	Module:amalgame_module(URI0),
	rdf_global_id(URI0, URI).

%%	amalgame_module_id(+URI, -Module) is det.
%
%	True if Module is an amalgame module identified by URI.
%	@error existence_error(amalgame_module)
%	@see current_amalgame_module/2 for a notdet version
%	that fails if there is no module.

amalgame_module_id(URI, Module) :-
	(   current_amalgame_module(URI, M)
	->  Module = M
	;   throw(error(existence_error(amalgame_module, [URI]), _))
	).

%%	amalgame_modules_of_type(+Class, -Modules) is det.
%
%	Modules are loaded amalgame modules of type Class.

amalgame_modules_of_type(Class, Modules) :-
	findall(Label-[URI,Module],
		( current_amalgame_module(URI, Module),
		  rdfs_subclass_of(URI, Class),
		  rdf_display_label(URI, Label)
		),
		LabeledModules),
	sort(LabeledModules, Sorted),
	pairs_values(Sorted, Modules).

%%	amalgame_module_parameters(+Module,-Parameters) is det.
%
%       Parameters is a list of parameters for Module.

amalgame_module_parameters(Module, Params) :-
	current_predicate(Module:parameter/4),
	!,
	findall(parameter(Id, Type, Default, Desc),
		Module:parameter(Id, Type, Default, Desc),
		Params).
amalgame_module_parameters(_, []).

%%      amalgame_module_property(+URI, ?Term)
%
%	True if module URI has property.
%
%	@param Term is one of:
%	 label(Label)
%	 a label of the module
%
%	 desc(Desc)
%	 a description of the module

amalgame_module_property(URI, label(Label)) :-
	!,
	(   rdf_label(URI, Lit)
	->  literal_text(Lit, Label)
	;   rdf_global_id(_:Label, URI)
	).
amalgame_module_property(URI, desc(Desc)) :-
	!,
	rdf_has(URI, skos:definition, Lit),
	literal_text(Lit, Desc).

amalgame_module_property(URI, explanation_graph(ExplainURI)) :-
	!,
	rdf_global_id(_:Local, URI),
	atomic_list_concat(['cpack/amalgame/web/img/', Local, '-*'], Path),
	absolute_file_name(Path, _FullPath,
			   [extensions([png, jpg]),
			    file_errors(fail),
			    access(read),
			    expand(true)
			   ]),
	http_absolute_location(img(Local), ExplainURI, []).

%%	process_options(+Process, +Module, -Options)
%
%	Options are the instantiated parameters for Module based on the
%	parameters string in Process.

process_options(Process, Module, Options) :-
	rdf(Process, amalgame:parameters, ParamLiteral),
        literal_text(ParamLiteral, ParamText),
	!,
	module_options(Module, Options, Parameters),
	parse_url_search(ParamText, Search0),
	rdf_expand_uri_values(Search0, Search),
	Request = [search(Search)] ,
	http_parameters(Request, Parameters).
process_options(_, _, []).


%%	module_options(+Module, -Options, -Parameters)
%
%	Options  are  all  option  clauses    defined   for  Module.
%	Parameters is a specification list for http_parameters/3.
%	Module:parameter is called as:
%
%	    parameter(Name, Properties, Description)
%
%	Name is the name of the	the option, The Properties are as
%	supported by http_parameters/3.	Description is used by the help
%	system.

module_options(Module, Options, Parameters) :-
	current_predicate(Module:parameter/4),
	!,
	findall(O-P,
		( call(Module:parameter, Name, Type, Default, _Description),
		  O =.. [Name, Value],
		  param_options(Type, Default, ParamOptions),
		  P =.. [Name, Value, ParamOptions]
		),
		Pairs),
	pairs_keys_values(Pairs, Options, Parameters).
module_options(_, _, []).


param_options(Type, Default, Options) :-
	(   is_list(Type)
	->  Options = [default(Default)|Type]
	;   Options = [default(Default), Type]
	).
