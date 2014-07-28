:- module(ag_component_util,
	  [ html_ag_header//1,
	    html_showlist//1,
	    html_options//2,
	    html_parameter_form//1
	  ]).

:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

html_ag_header(Options) -->
	{
	  findall(Rank-(Path-Label), ag:menu_item(Rank=Path, Label), Items0),
	  keysort(Items0, ItemsSorted),
	  pairs_values(ItemsSorted, Items)
	},
	html(div(id(header),
		 [ div(class(title),
		       a(href(location_by_id(http_amalgame_main_page)), 'Amalgame')),
		   ul(\html_ag_menu(Items, Options))
		 ])).

html_showlist([]) --> !.
html_showlist([H]) -->  html(H),!.
html_showlist([H1,H2|Tail]) -->  html([H1,', ']), html_showlist([H2|Tail]).

html_options([],_) --> !.
html_options([R|Rs], Default) -->
	{ rdf_display_label(R, Label),
	  (   R == Default
	  ->  Selected = [selected(selected)]
	  ;   Selected = []
	  )
	},
	html(option([value(R) | Selected], Label)),
	html_options(Rs, Default).

html_ag_menu([], _) --> !.
html_ag_menu([Handler-Label|Is], Options) -->
	html_menu_item(Handler, Label, Options),
	html_ag_menu(Is, Options).

html_menu_item(Handler, Label, Options) -->
	{ option(active(Handler), Options)
	},
	!,
	html(li(class(selected), span(Label))).
html_menu_item(Handler, Label, Options) -->
	{ option(strategy(Strategy), Options),
	  option(focus(Focus), Options, Strategy),
	  http_link_to_id(http_ag_build,
			  [strategy(Strategy)], ReturnToAfterLogin),
	  http_link_to_id(Handler, [
				    'openid.return_to'(ReturnToAfterLogin),
				    focus(Focus),
				    strategy(Strategy)
				   ],
			  Link)
	},
	html(li(a(href(Link), Label))).

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
