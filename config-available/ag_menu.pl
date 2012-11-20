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
cliopatria:menu_item(100=amalgame/http_eq, 'Amalgame (new, experimental UI)').

% cliopatria:menu_popup_order(vocabularies, 320).
cliopatria:menu_item(250=amalgame/http_skos_browser,   'Voc browser (old UI)').
cliopatria:menu_item(260=amalgame/http_list_skos_vocs, 'Voc stats   (old UI)').

% cliopatria:menu_popup_order(alignments, 350).
cliopatria:menu_item(300=amalgame/http_list_alignments,'Map stats   (old UI)').
cliopatria:menu_item(350=amalgame/http_list_overlap,   'Map overlap (old UI)').


