:- module(eq_controls,
	  [ html_controls//0
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(amalgame/amalgame_modules)).

current_modules_of_input_type(Types, Modules) :-
	findall(URI-Module,
		( current_amalgame_module(URI, Module),
		  amalgame_module_property(URI, input_type(Type)),
		  member(Type, Types)
		),
		Modules).

html_controls -->
	{ current_modules_of_input_type([mapping,vocab], InputModules),
	  current_modules_of_input_type([sourcetarget], MatchModules)
 	},
	html([div([id(select), class('control-set')],
		  [ div(class(hd), 'Select'),
		    div(class('bd hidden'),
			[ div([id(info), class(c)],
			      \html_node_props),
			      div([id(properties), class(c)], []),
			  \html_modules(InputModules)
			])
		  ]),
	      div([id(align), class('control-set')],
		  [ div(class(hd), 'Align'),
		    div(class(bd),
			[ div(class(c), \html_align_select),
			      \html_modules(MatchModules)
			])
		  ])
	     ]).

html_node_props -->
	html(table([tr([td(id(type), []),
			td(id(uri), []),
			td(input([type(button), id(delete), value(delete)]))
		       ]),
		    tr([td(label),
			td(input([type(text), id(label), size(30)])),
			td(input([type(button), id(updateLabel), value(change)]))
		       ])
		   ])).

html_align_select -->
	html(table([tr([td(input([type(button), id(sourcebtn), value('set as source')])),
			td([input([type(text), id(sourceLabel), size(35), autocomplete(off)]),
			    input([type(hidden), id(source), name(source)])
				  ])
		       ]),
		    tr([td(input([type(button), id(targetbtn), value('set as target')])),
			td([input([type(text), id(targetLabel), size(35), autocomplete(off)]),
			    input([type(hidden), id(target), name(target)])
				  ])
		       ])
		   ])).


%%	html_modules(+Modules:uri-module)
%
%	Emit YUI3 node accordion items for each module.
%
%	@TBD

html_modules(Modules) -->
	html(div([class('yui3-accordion module-list')],
		 \html_module_items(Modules))).


html_module_items([]) --> !.
html_module_items([URI-Module|Ms]) -->
	{  amalgame_module_parameters(Module, Params),
	   amalgame_module_property(URI, input_type(InputType))
	},
	html_accordion_item('control '+InputType,
			    \module_label(URI),
			    [ \module_desc(URI),
			      \module_form(URI, Params)
			    ]),
 	html_module_items(Ms).

module_form(URI, Params) -->
	html(form([input([type(hidden), name(process), value(URI)]),
		   table(tbody(\html_parameter_form(Params))),
		   div(class('control-buttons'),
		       input([type(button), class('control-submit'), value('Go')]))
		  ])).

module_label(URI) -->
	{ amalgame_module_property(URI, label(L))
	},
	html(L).

module_desc(URI) -->
	{ amalgame_module_property(URI, desc(D))
	},
	!,
	html(div(class(desc), D)).
module_desc(_) --> !.


%%	html_accordion_item(+CSSClass, +Header, +Body)
%
%	Emit HTML markup for an YUI3 accordion item.

html_accordion_item(Class, Header, Body) -->
	html(div([class('yui3-accordion-item '+Class)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:void(0)'),
			  class('yui3-accordion-item-trigger')],
			 Header)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).


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
builtin_input_item(uri, Value, Name) --> !,
	{ rdf_global_id(NS:Local, Value)
	},
	html(input([name(Name), size(35), value(NS+':'+Local)])).
builtin_input_item(atom, Value, Name) --> !,
	html(input([name(Name), size(35), value(Value)])).
builtin_input_item(_, Value, Name) -->
	{ format(string(S), '~q', [Value])
	},
	html(input([name(Name), size(35), value(S)])).

oneof([], _) -->
	[].
oneof([H|T], Value) -->
	(   {H == Value}
	->  html([ option([selected(selected),value(H)], H) ])
	;   html([ option([                   value(H)], H) ])
	),
	oneof(T, Value).

