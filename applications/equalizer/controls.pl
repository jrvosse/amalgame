:- module(eq_controls,
	  [ html_controls//0,
	    html_parameter_form//1,
	    module_input_type/2,
	    module_special_type/2,
	    status_option/1, % Move to eq_util?
	    html_options//1  % idem
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/html_write)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/map)).


:- rdf_meta
	status_option(r).

html_controls  -->
	{ amalgame_modules_of_type(amalgame:'Selecter', Selecters),
	  amalgame_modules_of_type(amalgame:'Matcher', Matchers),
	  amalgame_modules_of_type(amalgame:'SetOperator', Analyzers)
	},
	html([
	    \html_control_set(hint_control_set, true,
			      'Suggestion for next step',
			      div([id(hint), class('hint help c')], 'No hints available.')
			     ),
	    \html_control_set(analyze_control_set, false,
			      'Step 1: analyze',
			      \html_analyzers_control(Analyzers)),
	    \html_control_set(match_control_set, false,
			      'Step 2a: match or ...',
			      \html_match_control(Matchers)),
	    \html_control_set(select_control_set, false,
			      'Step 2b:	... select',
			      \html_select_control(Selecters)),
	    \html_control_set(current_control_set, true,
			      'Current node details',
			      \html_info_control)
	     ]).

html_control_set(Id, Active, Header, Body) -->
        { active_class(Active, Class)
	},
	html(div([id(Id), class('control-set '+Class)],
		 [ div(class('hd'),
		       a([href('javascript:void(0)'),
			  class('trigger')],
			 Header)),
		   div(class('bd'),
		       Body)
		 ])).

active_class(true, active).
active_class(false, '').

html_analyzers_control(Modules) -->
	html(div(id(analyzers),
		 [ div(class('bd hidden'),
		       [
			     \html_modules(Modules)
		 ])])).

html_info_control -->
	html(div(id(info),
		 [div(class('bd hidden'),
		      [ div([id(details), class(c)],
			    [\html_node_props,
			     div(class('control-buttons'),
				 [ button(id(evaluate), evaluate),
				   button(id(delete), delete),
				   button(id(update), update)

				 ])
			    ]),
			form([id(infocontent), class('control c')],
			     [div([id(properties)], []),
			      div(class('control-buttons'),
				  button(class('control-submit'), 'update'))
			     ]),
			div([class('loading c hidden')], [])
		      ]),
		  div([class('empty c')],
		      ['select a node in the graph'])
		 ])).

html_node_props -->
	{ findall(Status, status_option(Status), StatusOptions),
	  supported_map_relations(RelationOptions)
	},
	html(table([tr([td(id(type), []),
			td(id(uri), [])
		       ]),
		    tr([td([span(class(abbrev),'abbrev/'), label ]),
			td([input([type(text), class(abbrev), id(abbrev), maxlength(1), style('width:1em')]),
			    input([type(text), id(label)])])
		       ]),
		    tr([id(publish_ns)],[td(namespace),
			td(input([type(text), id(namespace)], []))
			]),
		    tr([td(comment),
			td(textarea([rows(1), id(comment)], []))
		       ]),
		    tr(id(statusrow),
		       [td(status),
			td(select([id(status), autocomplete(off)],
				 [ option(value('')),
				   \html_options(StatusOptions)
				 ]))
		       ]),
		    tr([id(relationrow)],
		      [td(relation),
		       td(select([id(default_relation), autocomplete(off)],
				 [ option(value('')),
				   \html_options(RelationOptions)
				 ]))
		      ])
		   ])).

html_options([]) --> !.
html_options([R|Rs]) -->
	{ rdf_display_label(R, Label)
	},
	html(option(value(R), Label)),
	html_options(Rs).

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
	html(table([tr([td(button(id(inputbtn), 'set as input')),
			td([input([type(text), id(inputLabel), autocomplete(off)]),
			    input([type(hidden), id(input), name(input)])
			   ])
		       ])
		   ])).

html_source_target_select -->
	html(table([tr([td(button(id(sourcebtn), 'set as source')),
			td([input([type(text), id(sourceLabel), autocomplete(off)]),
			    input([type(hidden), id(source), name(source)])
				  ])
		       ]),
		    tr([td(button(id(targetbtn), 'set as target')),
			td([input([type(text), id(targetLabel), autocomplete(off)]),
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
			      \module_form(URI, Params, SpecialType)
			    ]),
	html_module_items(Ms).

module_form(URI, Params, LastAction) -->
	{
	 (   amalgame_module_property(URI, explanation_graph(ExplainMe))
	 ->  Explain = input([type(hidden), name(graphic), value(ExplainMe)])
	 ;   Explain = ''
	 )
	},
	html(form([input([type(hidden), name(process), value(URI)]),
		   input([type(hidden), name(lastAction), value(LastAction)]),
		   Explain,
		   table(tbody(\html_parameter_form(Params))),
		   div(class('control-buttons'),
		       button(class('control-submit'), 'Go'))
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
	html(input([name(Name), value(NS+':'+Local)])).
builtin_input_item(uri, Value, Name) -->
	html(input([name(Name), value(Value)])).
builtin_input_item(atom, Value, Name) --> !,
	html(input([name(Name), value(Value)])).
builtin_input_item(_, Value, Name) -->
	{ format(string(S), '~q', [Value])
	},
	html(input([name(Name), value(S)])).

oneof([], _) -->
	[].
oneof([H|T], Value) -->
	(   {H == Value}
	->  html([ option([selected(selected),value(H)], \mylabel(H)) ])
	;   html([ option([                   value(H)], \mylabel(H)) ])
	),
	oneof(T, Value).

mylabel(Value) -->
	{ rdf_global_id(NS:Local, Value),!
	},
	html([NS, ':', Local]).
mylabel(Value) -->
	html(Value).
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

module_special_type(M, secinput) :-
	(   rdfs_subclass_of(M, amalgame:'Subtracter')
	;   rdfs_subclass_of(M, amalgame:'AncestorMatcher')
	;   rdfs_subclass_of(M, amalgame:'DescendentMatcher')
	;   rdfs_subclass_of(M, amalgame:'RelatedMatcher')
	;   rdfs_subclass_of(M, amalgame:'SetOperator')
	),
	!.
module_special_type(M,preloaded) :-
	rdfs_subclass_of(M, amalgame:'SelectPreLoaded'),
	!.


module_special_type(M, match) :-
	rdfs_subclass_of(M, amalgame:'Matcher'),
	!.
module_special_type(M, select) :-
	rdfs_subclass_of(M, amalgame:'MappingSelecter'),
	!.
module_special_type(_, '').

%%	status_option(-Status)
%
%	List of status types.

status_option(amalgame:final).
status_option(amalgame:intermediate).
status_option(amalgame:discarded).
status_option(amalgame:imported).
