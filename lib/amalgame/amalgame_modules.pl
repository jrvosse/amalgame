:- module(amalgame_modules,
	  [ current_amalgame_module/2,    % ?URI, ?Module
	    amalgame_module_id/2,         % +Class, +Type, -Module
 	    amalgame_module_parameters/2, % +Module, -Parameters
	    amalgame_module_property/2	  % +URI, ?Term
 	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- rdf_meta
	amalgame_module_property(r,?),
	module_input_type(r,?).

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
%
%	 input_type(InputType)
%	 a constant defining the type of input required

amalgame_module_property(URI, input_type(Type)) :-
	!,
	rdfs_subclass_of(URI, Class),
	module_input_type(Class, Type).
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

module_input_type(amalgame:'Matcher', sourcetarget).
module_input_type(amalgame:'MatchFilter', mapping).
module_input_type(amalgame:'MappingSelecter', mapping).
module_input_type(amalgame:'VocabSelecter', vocab).
module_input_type(amalgame:'Merger', mappings).

