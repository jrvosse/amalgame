:- module(eq_controls,
	  [ html_controls//0
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(amalgame/amalgame_modules)).


:- http_handler(amalgame(data/addprocess), http_add_process, []).

http_add_process(Request) :-
	http_parameters(Request,
			[ input(Input,
				[uri, description('URI of input mapping')]),
			  process(Process,
				  [uri, description('URI of the process')]),
			  alignment(Graph,
				    [uri, description('URI of the alignment graph to which the process is added')])
			],
			[form_data(Params0)]),
	subtract(Params0, [input=_,process=_], Params),
 	assert_process(Input, Process, Graph, Params),
	reply_json(json([input=Input,
			 process=Process
			])).

assert_process(Input, Process, Graph, Params) :-
	rdf_bnode(ProcessURI),
	rdf_bnode(OutputURI),
	uri_query_components(Search, Params),
 	rdf_transaction((rdf_assert(ProcessURI, rdf:type, Process, Graph),
			 rdf_assert(ProcessURI, amalgame:input, Input, Graph),
			 rdf_assert(ProcessURI, amalgame:parameters, Search, Graph))),
	rdf_transaction((rdf_assert(OutputURI, rdf:type, amalgame:'Mapping', Graph),
			 rdf_assert(OutputURI, opmv:wasGeneratedBy, ProcessURI, Graph))).



html_controls -->
 	html([div([id(info), class('yui3-accordion')],
		  [ \html_accordion_item(infobox, 'Info', [])
		  ]),
	      div([id(mappingcontrols), class('controlset hidden yui3-accordion')],
		  [\html_mapping_controls
		  ]),
	      div([id(processcontrols), class('controlset hidden yui3-accordion')],
		  []),
	      div([id(vocabcontrols), class('controlset hidden yui3-accordion')],
		  [])
	     ]).

html_mapping_controls -->
	{ amalgame_class_modules(amalgame:'Matcher', Matchers),
	  amalgame_class_modules(amalgame:'Selecter', Selecters)
	},
	html_accordion_item(filter, 'Filter', \html_tab_view(Matchers)),
	html_accordion_item(select, 'Select', \html_tab_view(Selecters)).


%%	html_accordion_item(+Id, +Label, +Body)
%
%	Emit YUI3 node accordion html markup.

html_accordion_item(Id, Label, Body) -->
	html(div([id(Id), class('yui3-accordion-item')],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:void(0)'),
			  class('yui3-accordion-item-trigger')],
			 Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).

%%	html_tab_view(+Modules)
%
%	Emit YUI3 tabview html markup.
%
html_tab_view(Modules) -->
	html(div(class('yui3-tabview'),
		 [ ul(\html_tab_list(Modules)),
		   div(\html_tab_panel(Modules))
		 ])).

html_tab_list([]) --> !.
html_tab_list([URI-Module|Ms]) -->
	{ module_label(URI, Label)
	},
	html(li(a([href('#'+Module)], Label))),
	html_tab_list(Ms).

module_label(M, Label) :-
	rdf_label(M, Lit),
	!,
	literal_text(Lit, Label).
module_label(M, Label) :-
	rdf_global_id(_:Label, M).

html_tab_panel([]) --> !.
html_tab_panel([_URI-Module|Ms]) -->
	{ amalgame_module_parameters(Module, Params)
	},
	html(div(id(Module),
		[ table(tbody(\html_parameter_form(Params, Module))),
		  div(class('control-buttons'),
		      input([type(button), class('control-submit'), value('Go')]))
		])),
	html_tab_panel(Ms).

%%	html_module_parameters(+ParameterList)
%
%	Emit html form components corresponding to Parameters.

html_parameter_form([],_) --> !.
html_parameter_form([parameter(Id, Type, Default, Desc)|Ps], Module) -->
 	html(tr(title(Desc),
		 [td(label(Id)),
		  td(\input_value(Type, Default, Module:Id))
		  ])),
	html_parameter_form(Ps, Module).


%%	input_value(+Type, +Value, +Id)// is det.
%
%	Emit an form-field for Value.

:- multifile
	input_item/5.			% input_item(+Type, +Value, +Id)//

input_value(Type, Value, Id) -->
	{ html_name(Id, Name) },
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


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	html_name(+Setting, -Name) is det.
%%	html_name(-Setting, +Name) is det.
%
%	Convert between Module:Setting and Name for use in form-fields.

html_name(Module:Setting, Name) :-
	atomic_list_concat([Module, Setting], ':', Name).


