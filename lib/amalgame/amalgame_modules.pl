:- module(amalgame_modules,
	  [ current_amalgame_module/2,    % ?URI, ?Module
	    amalgame_module_id/2,         % +URI, -Module
	    amalgame_modules_of_type/2,   % +Class, -Modules
	    amalgame_module_parameters/2, % +Module, -Parameters
	    amalgame_module_property/2	  % +URI, ?Term
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_path)).

:- rdf_meta
	amalgame_module_property(r,?),
	amalgame_module_id(r,-),
	amalgame_modules_of_type(r,-).

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

%%	amalgame_modules_of_type(+Class, -Modules)
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
	atomic_concat('cpack/amalgame/web/img/', Local, Path),
	absolute_file_name(Path, FullPath,
			   [extensions([png, jpg]),
			    file_errors(fail),
			    access(read)
			   ]),
	file_base_name(FullPath, Base),
	http_absolute_location(img(Base), ExplainURI, []).

