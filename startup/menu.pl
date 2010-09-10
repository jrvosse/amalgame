:-module(ag_menu, []).

:- use_module(library(http/html_write)).
:- use_module(cliopatria(hooks)).

/** <module> Amalgame menu.
  This module adds the amalgame functionality to the cliopatria menu.
*/

cliopatria:menu_item(view/http_concept_finder,	'Vocabularies').

cliopatria:menu_popup_order(amalgame, 3500).
cliopatria:menu_item(amalgame/http_list_alignments, 'Alignments').
cliopatria:menu_item(amalgame/http_list_overlap,    'Overlaps').
cliopatria:menu_item(amalgame/http_list_skos_vocs,  'Vocabularies').

