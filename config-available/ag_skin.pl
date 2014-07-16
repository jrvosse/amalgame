:- module(ag_skin, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).


:- use_module(cliopatria(hooks)).
% :- use_module(skin(cliopatria)).
:- use_module(components(label)).
% :- use_module(components(menu)).
:- use_module(components(simple_search)).

:- set_setting_default(graphviz:format, svg).

:- html_resource(css('amalgame.css'),
		 [requires([ css('cliopatria.css')
			   ])
		 ]).
:- html_resource(cliopatria,
		 [ virtual(true),
		   requires([ css('amalgame.css')
			    ])
		 ]).

:- html_resource(sortable,
		 [ virtual(true),
		   requires([ js('sorttable.js')
			    ])
		 ]).

cliopatria:display_link(literal(type(XSD,L)), _Options) -->
	{
	 rdf_equal(XSD, xsd:dateTime)
	},
	format_xsd_timestamp(L).

cliopatria:display_link(Cell, _Options) -->
	{
	 rdfs_individual_of(Cell, align:'Cell'),
	 resource_link(Cell, HREF)
	},
	html(a([class(r_def), href(HREF)], ['Map: ', \turtle_label(Cell)])).

cliopatria:display_link(SkosXLLabel, _Options) -->
	{ rdfs_individual_of(SkosXLLabel, skosxl:'Label'),
	  rdf_has(SkosXLLabel, skosxl:literalForm, Literal),
	  literal_text(Literal, Label),
	  resource_link(SkosXLLabel, HREF)
	},
	html(a([class('skosxllabel'), href(HREF)], Label)).

rdf_label:display_label_hook(Cell, _Lang, Label) :-
	rdfs_individual_of(Cell, align:'Cell'),
	atom_concat('Map: ', Cell, Label).

cliopatria:predicate_order(P, "zzz") :- rdf_equal(align:map, P).
cliopatria:predicate_order(P, 400) :-
	rdf_has(P, rdfs:isDefinedBy, 'http://www.w3.org/ns/prov#').
cliopatria:predicate_order(P, 405) :-
	rdf_has(P, rdfs:isDefinedBy, 'http://purl.org/vocabularies/amalgame').

user:body(amalgame(search), Body) -->
	{
	 http_link_to_id(http_amalgame_main_page, [], BackOfficeLink)
	},
	html_requires(cliopatria),
	html(body(class(['yui-skin-sam', ag_search, cliopatria]),
		  [
		    div(class(ag_search),
			[
			 \simple_search_form,
			 div(class(content), Body)
			]),
			br(clear(all)),
			div(class(footer),
			    \(cliopatria:server_address)
			),
		        div([class(backoffice)],
			    [a(href(BackOfficeLink), 'back office')
			    ])
		  ])).

% Amalgame is an extension of ClioPatria and uses the ClioPatria
% skin.

:- multifile
        user:body//2.

user:body(user(Style), Body) -->
        user:body(cliopatria(Style), Body).

format_xsd_timestamp(L) -->
	{
	 sub_atom(L,St,_Wt,_Et,'T'),
	 sub_atom(L,_Sp,_Wp,Ep,'+'), ST is St + 1, ET is Ep +1,
	 sub_atom(L,0,St,_,DatePart),
	 sub_atom(L,ST,_,ET,TimePart)
	},
	html(span(class('time:instant'), [TimePart, ' on ',DatePart])).
