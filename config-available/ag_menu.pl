:-module(ag_menu, []).

:- use_module(cliopatria(hooks)).
:- use_module(applications(skos_browser)).

/** <module> Amalgame menu.

This module is a ClioPatria plugin that extends the ClioPatria menu.
*/

%%	cliopatria:menu_item(-Item, -Label) is nondet.
%
%	Provide the Amalgame extensions to the Cliopatria menu.

cliopatria:menu_popup_order(amalgame, 120).
cliopatria:menu_item(100=amalgame/http_amalgame_main_page, 'Alignment interface').
cliopatria:menu_item(250=amalgame/http_skos_browser,   'Vocabulary browser').
