:-module(ag_menu, []).

:- use_module(cliopatria(hooks)).

/** <module> Amalgame menu.

This module is a ClioPatria plugin that extends the ClioPatria menu.
*/

%%	cliopatria:menu_item(-Item, -Label) is nondet.
%
%	Provide the Amalgame extensions to the Cliopatria menu.


cliopatria:menu_popup_order(vocabularies, 320).
cliopatria:menu_item(100=vocabularies/http_concept_finder, 'Browse').
cliopatria:menu_item(200=vocabularies/http_list_skos_vocs, 'Statistics').

cliopatria:menu_popup_order(alignments, 350).
cliopatria:menu_item(100=alignments/http_list_overlap,    'Overlaps').
cliopatria:menu_item(200=alignments/http_list_alignments, 'Statistics').


