:- module(eq_controls,
	  [ html_controls//0
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/html_write)).
:- use_module(library(amalgame/amalgame_modules)).

html_controls -->
	{ amalgame_modules_of_type(amalgame:'Selecter', Selecters),
	  amalgame_modules_of_type(amalgame:'Matcher', Matchers)
 	},
	html([div([id(info), class('control-set')],
		  \html_info_control),
	      div([id(select), class('control-set')],
		  \html_select_control(Selecters)),
	      div([id(match), class('control-set')],
		  \html_match_control(Matchers))
	     ]).

html_info_control -->
	html([ div(class(hd), 'Current node'),
	       div(class('bd hidden'),
		   [ div([id(details), class(c)],
			 \html_node_props),
		     div([id(properties), class(c)],
			  [])
		   ]),
	       div([class('empty c')],
		   ['select a node in the graph']),
	       div([class('loading c hidden')], [])
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

html_select_control(Modules) -->
	html([div(class(hd), 'Selecters'),
	      div(class('bd'),
		  [ div(class(c),
			ul([li('select a set of correspondences from a mapping'),
			    li('select a set of concepts from a vocabulary')
			   ])),
		    \html_modules(Modules)
		  ])
	     ]).

html_match_control(Modules) -->
	html([div(class(hd), 'Matchers'),
	      div(class(bd),
		  [ div(class(c),
			\html_align_input),
		    \html_modules(Modules)
		  ])
	     ]).

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
	   module_css_class(URI, CssClass)
	},
	html_accordion_item('control '+CssClass,
			    \module_label(URI),
			    [ \module_desc(URI),
			      \module_form(URI, Params)
			    ]),
 	html_module_items(Ms).

module_css_class(M, mapping) :-
	rdfs_subclass_of(M, amalgame:'MappingSelecter'),
	!.
module_css_class(M, vocab) :-
	rdfs_subclass_of(M, amalgame:'VocabSelecter'),
	!.
module_css_class(M, match) :-
	rdfs_subclass_of(M, amalgame:'Matcher'),
	!.
module_css_class(_, '').

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

