:-module(ag_menu, []).

:- use_module(cliopatria(hooks)).

/** <module> Amalgame menu.

This module is a ClioPatria plugin that extends the ClioPatria menu.
*/

%%	cliopatria:menu_item(-Item, -Label) is nondet.
%
%	Provide the Amalgame extensions to the Cliopatria menu.

cliopatria:menu_item(150=places/http_concept_finder,	'Vocabularies').

cliopatria:menu_popup_order(amalgame, 350).
cliopatria:menu_item(100=amalgame/http_list_alignments, 'Alignments').
cliopatria:menu_item(200=amalgame/http_list_overlap,    'Overlaps').
cliopatria:menu_item(300=amalgame/http_list_skos_vocs,  'Vocabularies').

