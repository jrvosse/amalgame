:- module(eq_controls,
	  [ json_hint/2,
	    html_controls//0,
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
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(components(label)).

:- use_module(eq_util).

:- rdf_meta
	status_option(r).

html_controls  -->
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
			    [\html_node_props,
			     div(class('control-buttons'),
				 [ button(id(delete), delete),
				   button(id(update), update)
				 ])
			    ]),
			form([id(infocontent), class('control c')],
			     [div([id(properties)], []),
			      div(class('control-buttons'),
				  button(class('control-submit'), 'Update'))
			     ]),
			div([id(hint), class('hint help c')], 'No hints available.')
		      ]),
		  div([class('empty c')],
		      ['select a node in the graph']),
		  div([class('loading c hidden')], [])
		 ])).

html_node_props -->
	{ findall(R, status_option(R), Rs)
	},
	html(table([tr([td(id(type), []),
			td(id(uri), [])
		       ]),
		    tr([td(label),
			td(input([type(text), id(label)]))
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
				   \html_options(Rs)
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

json_hint(Strategy, Hint) :-
	% If there are no mappings yet, advise an exact label match
	\+ rdf(_, rdf:type, amalgame:'Mapping',Strategy),
	!,
	rdf_equal(Match, amalgame:'ExactLabelMatcher'),
	rdf_display_label(Match, Label),
	format(atom(Text), 'hint: maybe you\'d like to try a simple label Matcher like ~w to create your first mapping', [Label]),
	rdf(Strategy, amalgame:includes, Voc1, Strategy),
	rdf(Strategy, amalgame:includes, Voc2, Strategy),
	Voc1 \== Voc2,
	concept_count(Voc1, Strategy, Count1),
	concept_count(Voc2, Strategy, Count2),
	(   Count1 < Count2
	->  Source = Voc1, Target = Voc2
	;   Source = Voc2, Target = Voc1
	),
	Hint =	json([
		    data(json([
			     process(Match),
			     source(Source),
			     target(Target),
			     alignment(Strategy)
			      ])),
		    text(Text)
		     ]).

json_hint(Strategy, Hint) :-
	% if there are end-point mappings with ambiguous correspondences, advise an ambiguity remover
	has_ambiguous_endpoint(Strategy, Mapping),
	rdf_equal(Process, amalgame:'AritySelect'),
	rdf_display_label(Process, PLabel),
	rdf_display_label(Mapping, MLabel),
	format(atom(Text), 'hint: maybe you\'d like to remove the ambiguity from node "~w" (~p) by running ~w', [MLabel, Mapping, PLabel]),
	Hint =	json([
		    data(json([
			     process(Process),
			     input(Mapping),
			     alignment(Strategy)
			      ])),
		    text(Text)
		     ]).

json_hint(_, json([])).


has_ambiguous_endpoint(Strategy, Mapping) :-
	rdf_transaction(
	    (
	    rdf(Mapping, rdf:type, amalgame:'Mapping', Strategy), % We are looking for a Mapping
	    \+ rdf(_Process, amalgame:input, Mapping, Strategy),  % that has not been used as an input yet,
	    \+ ( rdf(Mapping, opmv:wasGeneratedBy, AmbRemover),	  % is not a result of arity select,
		 rdfs_individual_of(AmbRemover, amalgame:'AritySelect')
	       ),                                                  % and the nr of source and target mappings
	    \+ mapping_counts(Mapping, Strategy, N, N, N, _, _)   %  differs from the total number of mappings
	    )
		       ).

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
	{
	 (   amalgame_module_property(URI, explanation_graph(ExplainMe))
	 ->  Explain = input([type(hidden), name(graphic), value(ExplainMe)])
	 ;   Explain = ''
	 )
	},
	html(form([input([type(hidden), name(process), value(URI)]),
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
	(   rdfs_subclass_of(M, amalgame:'Subtracter');
	    rdfs_subclass_of(M, amalgame:'AncestorMatcher');
	    rdfs_subclass_of(M, amalgame:'DescendentMatcher');
	    rdfs_subclass_of(M, amalgame:'RelatedMatcher')
	),
	!.

module_special_type(M, merger) :-
	rdfs_subclass_of(M, amalgame:'Merger'),
	!.
module_special_type(M, match) :-
	rdfs_subclass_of(M, amalgame:'Matcher'),
	!.
module_special_type(_, '').

%%	status_option(-Status)
%
%	List of status types.

status_option(amalgame:final).
status_option(amalgame:intermediate).
status_option(amalgame:discarded).
status_option(amalgame:imported).
