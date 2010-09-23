:- module(ag_skin, []).

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


logo -->
	{
	 http_absolute_location(icons('econnect-banner.jpg'), LogoImg, [])
	},
	html(a([class(logo),
		id(econnectlink),
		target(new),
		href('http://www.europeanaconnect.eu/'),
		style('float: left; margin-right: 1%;')
	       ],
	       img([src(LogoImg),
		    style('border-style: none;'),
		    alt('EuropeanaConnect project logo')
		   ],[])
	      )
	    ).

cliopatria:server_address -->
	{
	  git_component_property('ClioPatria', version(CP_Version)),
	  git_component_property('ClioPatria', home_url(CP_Home)),
	  git_component_property('amalgame', version(AG_Version)),
	  git_component_property('amalgame', home_url(AG_Home))

	},
	html_requires(css('cliopatria.css')),
	html([ 	\logo,
		address([id(amalgame)],
			a(href(AG_Home),
			  'Amalgame ~w'-[AG_Version])
		       ),
	       address(class(cliopatria),
		       [
			 a(href(CP_Home),
			   'ClioPatria ~w'-[CP_Version]),
			 \current_page_doc_link
		       ])
	     ]).

