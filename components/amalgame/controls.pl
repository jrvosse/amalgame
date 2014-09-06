:- module(ag_controls,
	  [ html_controls//0
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/html_write)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/map)).
:- use_module(components(amalgame/util)).

html_controls  -->
	{ amalgame_modules_of_type(amalgame:'Partitioner', Partitioners),
	  amalgame_modules_of_type(amalgame:'Matcher', Matchers),
	  amalgame_modules_of_type(amalgame:'SetOperator', Analyzers)
	},
	html([
	    \html_control_set(hint_control_set, true,
			      'Suggestion for next step',
			      div([id(hint), class('hint help c')], 'No hints available.')
			     ),
	    \html_control_set(current_control_set, true,
			      'About the selected node',
			      \html_info_control),
	    \html_control_set(match_control_set, false,
			      'Generate',
			      \html_match_control(Matchers)),
	    \html_control_set(select_control_set, false,
			      'Partition',
			      \html_select_control(Partitioners)),
	    \html_control_set(analyze_control_set, false,
			      'Combine',
			      \html_analyzers_control(Analyzers))
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
				 [
				   \html_options([''|StatusOptions],'')
				 ]))
		       ]),
		    tr([id(relationrow)],
		      [td(relation),
		       td(select([id(default_relation), autocomplete(off)],
				 [
				   \html_options([''|RelationOptions], '')
				 ]))
		      ])
		   ])).



html_select_control(Modules) -->
	html(div(id(select),
		 [ div(class(c),
		       ul([li('partition a set according to some criterium'),
			   li('typically resulting in selected and discarded subsets')
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



%%	module_input_type(+ModuleURI, -InputType)
%
%	InpuType defines for which type of input the module can be
%	used.

module_input_type(M, mapping) :-
	rdfs_subclass_of(M, amalgame:'MappingPartitioner'),
	!.
module_input_type(M, vocab) :-
	rdfs_subclass_of(M, amalgame:'CandidateGenerator'),
	!.
module_input_type(M, vocab) :-
	rdfs_subclass_of(M, amalgame:'VocabPartitioner'),
	!.
module_input_type(_, '').

%%	module_special_type(+ModuleURI, -Type).
%
%	Type is set for modules that require additional javascript
%	control in the UI.

module_special_type(M, secinput) :-
	rdf_has(M, amalgame:need_secondary_inputs, literal(type(xsd:boolean, true))),
	!.
module_special_type(M, preloaded) :-
	rdfs_subclass_of(M, amalgame:'SelectPreLoaded'),
	!.
module_special_type(M, generate) :-
	rdfs_subclass_of(M, amalgame:'CandidateGenerator'),
	!.
module_special_type(M, match) :-
	rdfs_subclass_of(M, amalgame:'Matcher'),
	!.
module_special_type(M, select) :-
	rdfs_subclass_of(M, amalgame:'MappingPartitioner'),
	!.
module_special_type(_, '').
