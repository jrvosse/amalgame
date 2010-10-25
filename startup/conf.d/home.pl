:- module(conf_home, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).

:- use_module(amalgame_apps(voc_stats)).

/** <module> Configure Amalgame Home Page

*/

:- http_handler(cliopatria(home), home, []).

home(_Request) :-
	http_absolute_location(icons('econnect-logo-big.jpg'), LogoImg, []),
	format(atom(HomeStyle), 'div.ag_search { background-image: url(~w);',[LogoImg]),
	reply_html_page(amalgame(search),
			[title('Amalgame: home'),
			 style(HomeStyle)
			],
			[\show_schemes
			]).

