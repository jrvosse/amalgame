:- module(ag_skin, []).

:- use_module(library(version)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(cliopatria(hooks)).
:- use_module(cliopatria(skin)).
:- use_module(components(label)).
:- use_module(components(menu)).
:- use_module(components(simple_search)).

user:file_search_path(icons, amalgame('web/img')).
user:file_search_path(css,   amalgame('web/css')).

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
cliopatria:resource_link(Alignment, Link) :-
	rdfs_individual_of(Alignment, amalgame:'Alignment'),
	http_link_to_id(http_list_alignment, [graph(Alignment)], Link).
cliopatria:resource_link(Voc, Link) :-
	rdfs_individual_of(Voc, skos:'ConceptScheme'),
	http_link_to_id(http_list_skos_voc, [voc(Voc)], Link).

cliopatria:page_body(Body) -->
	html_requires(css('amalgame.css')),
	html(body(class('yui-skin-sam'),
		  [ div(id(sidebar), \cp_menu),
		    \simple_search_form,
		    br(clear(all)),
		    div(id(content), Body),
		    br(clear(all)),
		    div([id(address)],
			 \(cliopatria:server_address)
			)
		  ])).

cliopatria:server_address -->
	html_requires(css('cliopatria.css')),
	html([ 	\logo,
		\server_address(amalgame)
	     ]).


cliopatria:predicate_order(P, "zzz") :- rdf_equal(align:map, P).
cliopatria:predicate_order(P, 400) :-
	rdf_has(P, rdfs:isDefinedBy, 'http://purl.org/net/opmv/ns').
cliopatria:predicate_order(P, 405) :-
	rdf_has(P, rdfs:isDefinedBy, 'http://purl.org/vocabularies/amalgame').

cliopatria:bnode_label(TimeInstant) -->
	{ rdf(TimeInstant, time:inXSDDateTime, Literal)
	},
	html(\turtle_label(Literal)).

user:body(amalgame(search), Body) -->
	{
	 http_link_to_id(http_list_skos_vocs, [], BackOfficeLink)
	},
	html_requires(css('amalgame.css')),
	html(body(class('yui-skin-sam ag_search'),
		  [
		    div(class(ag_search),
			[
			 \simple_search_form,
			 div(id(content), Body)
			]),
			br(clear(all)),
			div([id(address)],
			 \(cliopatria:server_address)
			),
		        div([class(backoffice)],
			    [a(href(BackOfficeLink), 'back office')
			    ])
		  ])).

logo -->
	{
	 http_absolute_location(icons('econnect-banner.jpg'), LogoImg, [])
	},
	html(a([class(logo),
		id(econnectlink),
		target(new),
		href('http://www.europeanaconnect.eu/')
	       ],
	       img([src(LogoImg),
		    alt('EuropeanaConnect project logo')
		   ],[])
	      )
	    ).
