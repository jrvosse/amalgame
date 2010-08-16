:-module(ag_menu, []).

:- use_module(library(http/html_write)).
:- use_module(cliopatria(hooks)).

% Position amalgame menu between Cliopatria's File and Query menu.
cliopatria:menu_popup_order(amalgame, 1500).

cliopatria:menu_item(amalgame/list_skos_vocs,  'SKOS Vocabularies').
cliopatria:menu_item(amalgame/list_alignments, 'List alignments').
cliopatria:menu_item(amalgame/find_overlap,    'Alignment overlap').


