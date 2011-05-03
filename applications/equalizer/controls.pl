:- module(eq_controls,
	  [ html_controls//0
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(amalgame/amalgame_modules)).


html_controls -->
	{ findall(URI-M, current_amalgame_module(URI, M), Modules)
	},
 	html([div(id(infobox), []),
	      div([id(input), class('control vocab')],
		  \html_align_select),
	      div([class('yui3-accordion')],
		  \html_modules(Modules))
	     ]).


html_align_select -->
	html(table([tr([td(input([type(button), id(sourcebtn), value('set as source')])),
			td([input([type(text), id(sourceLabel), size(40), autocomplete(off)]),
			    input([type(hidden), id(source), name(source)])
			   ])
		       ]),
		    tr([td(input([type(button), id(targetbtn), value('set as target')])),
			td([input([type(text), id(targetLabel), size(40), autocomplete(off)]),
			    input([type(hidden), id(target), name(target)])
			   ])
		       ])
		   ])).


%%	html_modules(+Modules:uri-module)
%
%	Emit YUI3 node accordion items for each module.
%
%	@TBD

html_modules([]) --> !.
html_modules([URI-Module|Ms]) -->
	html_module(URI, Module),
	html_modules(Ms).

html_module(URI, Module) -->
	{  amalgame_module_parameters(Module, Params),
	   module_inputs(URI, Inputs),
	   concat_atom(Inputs, ' ', Classes)
	},
 	html(div([id(Module), class('yui3-accordion-item control '+Classes)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:void(0)'),
			  class('yui3-accordion-item-trigger')],
			 \module_label(URI))),
		   div(class('yui3-accordion-item-bd'),
		       [ div(class(desc),
			     \module_desc(URI)),
			 form(id(URI),
			      [ table(tbody(\html_parameter_form(Params))),
				div(class('control-buttons'),
				    input([type(button), class('control-submit'), value('Go')]))
			      ])
		       ])
		 ])).

module_inputs(M, [mapping, vocab]) :-
	rdfs_subclass_of(M,amalgame:'Matcher'),
	!.
module_inputs(M, [mapping]) :-
	rdfs_subclass_of(M,amalgame:'Selecter'),
	!.
module_inputs(M, [vocab]) :-
	rdfs_subclass_of(M,amalgame:'Voc_Exclude'),
	!.
module_inputs(_, [mapping, vocab]).


module_label(M) -->
	{ rdf_label(M, Lit),
	  !,
	  literal_text(Lit, Label)
	},
	html(Label).
module_label(M) -->
	{ rdf_global_id(_:Label, M)
	},
	html(Label).

module_desc(M) -->
	{ rdf_has(M, skos:definition, Lit),
	  !,
	  literal_text(Lit, Txt)
	},
	html(Txt).
module_desc(_) --> !.

%%	html_module_parameters(+ParameterList)
%
%	Emit html form components corresponding to Parameters.

html_parameter_form([]) --> !.
html_parameter_form([parameter(Name, Type, Default, Desc)|Ps]) -->
 	html(tr(title(Desc),
		 [td(label(Name)),
		  td(\input_value(Type, Default, Name))
		  ])),
	html_parameter_form(Ps).


%%	input_value(+Type, +Value, +Name)// is det.
%
%	Emit an form-field for Value.

:- multifile
	input_item/5.	       % input_item(+Type, +Value, +Name)//

input_value(Type, Value, Name) -->
 	(   input_item(Type, Value, Name)
	->  []
	;   builtin_input_item(Type, Value, Name)
	).

builtin_input_item(boolean, Value, Name) --> !,
	builtin_input_item(oneof([true,false]), Value, Name).
builtin_input_item(between(L,U), Value, Name) --> !,
	html(input([ type(range),
		     name(Name),
 		     min(L), max(U), value(Value)
		   ])).
builtin_input_item(oneof(List), Value, Name) --> !,
	html(select([name(Name)], \oneof(List, Value))).
builtin_input_item(atom, Value, Name) --> !,
	html(input([name(Name), size(40), value(Value)])).
builtin_input_item(_, Value, Name) -->
	{ format(string(S), '~q', [Value])
	},
	html(input([name(Name), size(40), value(S)])).

oneof([], _) -->
	[].
oneof([H|T], Value) -->
	(   {H == Value}
	->  html([ option([selected(selected),value(H)], H) ])
	;   html([ option([                   value(H)], H) ])
	),
	oneof(T, Value).

