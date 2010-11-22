:- module(conf_home, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).

:- use_module(applications(vocabularies/vocabularies)).
:- use_module(applications(vocabularies/components)).

/** <module> Configure Amalgame Home Page

*/

:- http_handler(cliopatria(home), home, []).

%%	home(+Request) is det.
%
%	Override default ClioPatria homepage by a page with a big
%	econnect logo, a simple search box and an overview of the SKOS
%	concept schemes loaded in the repository.
%
%	Notes:
%       * +Request is currently not used.
%	* The default ClioPatria and amalgame functionality is available
%	via the small "back office" link on the bottom left of the page.
%	The latter is implemented via the amalgame(search) style
%	implemented by user:body//2 in skin.pl.

home(_Request) :-
	http_absolute_location(icons('econnect-logo-big.jpg'), LogoImg, []),
	format(atom(HomeStyle), 'div.ag_search { background-image: url(~w);',[LogoImg]),
	reply_html_page(amalgame(search),
			[title('Amalgame: home'),
			 style(HomeStyle)
			],
			[\show_schemes
			]).

