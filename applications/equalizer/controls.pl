:- module(eq_controls,
	  [ html_controls//0,
	    html_parameter_form//1,
	    module_input_type/2,
	    module_special_type/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/html_write)).
:- use_module(library(amalgame/amalgame_modules)).

html_controls -->
	{ amalgame_modules_of_type(amalgame:'Selecter', Selecters),
 	  amalgame_modules_of_type(amalgame:'Matcher', Matchers)
  	},
	html([\html_control_set(true,
				'Current node',
				\html_info_control),
	      \html_control_set(false,
				'Selecters',
				\html_select_control(Selecters)),
	      \html_control_set(false,
				'Matchers',
				\html_match_control(Matchers))
	     ]).

html_control_set(Active, Header, Body) -->
        { active_class(Active, Class)
	},
	html(div([class('control-set '+Class)],
		 [ div(class('hd'),
		       a([href('javascript:void(0)'),
			  class('trigger')],
			 Header)),
		   div(class('bd'),
		       Body)
		 ])).

active_class(true, active).
active_class(false, '').

html_info_control -->
	html(div(id(info),
		 [div(class('bd hidden'),
		      [ div([id(details), class(c)],
			    \html_node_props),
 			form(class('control c'),
			     [div([id(properties)], []),
			      div(class('control-buttons'),
				  input([type(button), class('control-submit'), value('Go')]))
			     ])
		      ]),
		  div([class('empty c')],
		      ['select a node in the graph']),
		  div([class('loading c hidden')], [])
		 ])).

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

html_select_control(Modules) -->
	html(div(id(select),
		 [ div(class(c),
		       ul([li('select a set of correspondences from a mapping'),
			   li('select a set of concepts from a vocabulary')
			  ])),
		   \html_modules(Modules)
		 ])).

html_match_control(Modules) -->
	html(div(id(match),
		 [ div(class(c),
		       \html_align_input),
		   \html_modules(Modules)
		 ])).

html_align_input -->
	html([h4('Choose input'),
	      div(class(i),
		  \html_mapping_select),
	      div(class(i),
		  \html_source_target_select)
	     ]).

html_mapping_select -->
	html(table([tr([td(input([type(button), id(inputbtn), value('set as input')])),
			td([input([type(text), id(inputLabel), size(35), autocomplete(off)]),
			    input([type(hidden), id(input), name(input)])
			   ])
		       ])
		   ])).

html_source_target_select -->
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
html_module_items([[URI,Module]|Ms]) -->
	{  amalgame_module_parameters(Module, Params),
	   module_input_type(URI, InputType),
	   module_special_type(URI, SpecialType)
	},
	html_accordion_item('control '+SpecialType+' '+InputType,
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
builtin_input_item(uri, Value, Name) -->
	{ rdf_global_id(NS:Local, Value),!
	},
	html(input([name(Name), size(35), value(NS+':'+Local)])).
builtin_input_item(uri, Value, Name) -->
	html(input([name(Name), size(35), value(Value)])).
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


%%	module_input_type(+ModuleURI, -InputType)
%
%	InpuType defines for which type of input the module can be
%	used.

module_input_type(M, mapping) :-
	rdfs_subclass_of(M, amalgame:'MappingSelecter'),
	!.
module_input_type(M, vocab) :-
	rdfs_subclass_of(M, amalgame:'VocabSelecter'),
	!.
module_input_type(_, '').

%%	module_special_type(+ModuleURI, -Type).
%
%	Type is set for modules that require additional javascript
%	control in the UI.

module_special_type(M, subtract) :-
	rdfs_subclass_of(M, amalgame:'Subtracter'),
	!.
module_special_type(M, merger) :-
	rdfs_subclass_of(M, amalgame:'Merger'),
	!.
module_special_type(M, match) :-
	rdfs_subclass_of(M, amalgame:'Matcher'),
	!.
module_special_type(_, '').
