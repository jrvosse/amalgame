:- module(conf_home, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(amalgame_apps(voc_stats)).

/** <module> Configure Amalgame Home Page

*/

:- http_handler(cliopatria(home), home, []).

home(_Request) :-
		reply_html_page(amalgame(search),
			[title('Amalgame: home')
			],
			[\show_schemes
			]).

