:- module(ag_skin, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(version)).

:- use_module(cliopatria(hooks)).
:- use_module(cliopatria(skin)).
:- use_module(components(menu)).
:- use_module(components(simple_search)).

user:file_search_path(icons, amalgame('web/img')).
user:file_search_path(css,   amalgame('web/css')).

:- set_setting_default(graphviz:format, svg).

:- html_resource(css('amalgame.css'),
		 [requires([
			    css('rdf_browse.css')
			   ])
		 ]).

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
